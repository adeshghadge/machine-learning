import math
import numpy as np
import matplotlib.pyplot as plt
import tifffile as tiff

from train_unet import weights_path, get_model, normalize, PATCH_SZ, N_CLASSES

def rotate_img(img, r):
    # channels in img are last!!!
    # r is a transformation type (an integer from 0 to 7)
    reverse_x = r % 2 == 1         # True for r in [1,3,5,7]
    reverse_y = (r // 2) % 2 == 1  # True for r in [2,3,6,7]
    swap_xy = (r // 4) % 2 == 1    # True for r in [4,5,6,7]
    if reverse_x:
        img = img[::-1, :, :]
    if reverse_y:
        img = img[:, ::-1, :]
    if swap_xy:
        img = img.transpose([1, 0, 2])
    return img

    
def predict(img, model, patch_sz=160, border_sz=20, n_classes=5, augment=True):
    ## model is a trained CNN    
    # border is a place around center in the patch where predictions are usually bad in u-nets
    img_height = img.shape[0]
    img_width = img.shape[1]
    n_channels = img.shape[2]

    # make extended img so that it contains integer number of crossed-by-border patches
    center_sz = patch_sz - 2 * border_sz
    npatches_vert = int(math.ceil((img_height - 2*border_sz)/center_sz))
    npatches_horizon = int(math.ceil((img_width - 2*border_sz)/center_sz))
    extended_height = 2*border_sz + center_sz * npatches_vert
    extended_width = 2*border_sz + center_sz * npatches_horizon
    ext_img = np.zeros(shape=(extended_height, extended_width, n_channels), dtype=np.float32)

    # fill extended image with mirrors:
    ext_img[:img_height, :img_width, :] = img
    for i in range(img_height, extended_height):
        ext_img[i, :, :] = ext_img[2*img_height - i - 1, :, :]
    for j in range(img_width, extended_width):
        ext_img[:, j, :] = ext_img[:, 2*img_width - j - 1, :]

    # now assemble all patches in one array
    patches_list = []
    for i in range(0, npatches_vert):
        for j in range(0, npatches_horizon):
            x0, x1 = i * center_sz, (i + 1) * center_sz + 2 * border_sz
            y0, y1 = j * center_sz, (j + 1) * center_sz + 2 * border_sz
            if augment:
                for r in range(8):
                    patches_list.append(rotate_img(ext_img[x0:x1, y0:y1, :], r))
            else:
                patches_list.append(ext_img[x0:x1, y0:y1, :])
    patches_arr = np.asarray(patches_list) # np.transpose(patches_list, (0, 1, 2, 3))

    # predictions:
    patches_predict = model.predict(patches_arr, batch_size=4)
    confidence_map_patch = np.full(shape=(patch_sz, patch_sz, n_classes), fill_value=0.1)  # low confidence for borders
    confidence_map_patch[border_sz:border_sz+center_sz, border_sz:border_sz+center_sz, :] = 1 # high confidence for center
    confidence_map_img = np.zeros(shape=(extended_height, extended_width, n_classes), dtype=np.float32)
    prd = np.zeros(shape=(extended_height, extended_width, n_classes), dtype=np.float32)
    
    for k in range(0, patches_predict.shape[0]):  # for all predicted patches
        if augment:
            r = k % 8   # patch transformation type (0..7)
            i = k // 8 // npatches_horizon  # patch x-coordinate
            j = k // 8 % npatches_horizon   # patch y-coordinate
            x0, x1 = i * center_sz, (i + 1) * center_sz + 2 * border_sz
            y0, y1 = j * center_sz, (j + 1) * center_sz + 2 * border_sz
            confidence_map_img[x0:x1, y0:y1, :] += confidence_map_patch
            prd[x0:x1, y0:y1, :] += rotate_img(patches_predict[k, :, :, :], r) * confidence_map_patch
        else:
            i = k // npatches_horizon
            j = k % npatches_horizon
            x0, x1 = i * center_sz, (i + 1) * center_sz + 2 * border_sz
            y0, y1 = j * center_sz, (j + 1) * center_sz + 2 * border_sz
            confidence_map_img[x0:x1, y0:y1, :] += confidence_map_patch
            prd[x0:x1, y0:y1, :] += patches_predict[k, :, :, :] * confidence_map_patch
    prd /= confidence_map_img
    return prd[:img_height, :img_width, :]



def picture_from_mask(mask, threshold=0):
    colors = {
        0: [150, 150, 150],  # Buildings
        1: [223, 194, 125],  # Roads & Tracks
        2: [27, 120, 55],    # Trees
        3: [166, 219, 160],  # Crops
        4: [116, 173, 209]   # Water
    }
    z_order = {
        1: 3,
        2: 4,
        3: 0,
        4: 1,
        5: 2
    }
    pict = 255*np.ones(shape=(3, mask.shape[0], mask.shape[1]), dtype=np.uint8)
    
    #for i in range(1, 6):
    #    cl = z_order[i]

    for ch in range(3):
        pict[ch,:,:][mask[:,:] > threshold] = colors[3][ch]

    return pict


if __name__ == '__main__':
    model = get_model()
    model.load_weights(weights_path)
    test_id = 'test'
    img = normalize(tiff.imread('data/mband/{}.tif'.format(test_id)).transpose([1,2,0]))   # make channels last
    mask = predict(img, model, patch_sz=PATCH_SZ, n_classes=N_CLASSES) #.transpose([2,0,1])  # make channels first
    map = picture_from_mask(mask, 0.5)

    tiff.imsave('result.tif', (255*mask).astype('uint8'))
    tiff.imsave('map.tif', map)


