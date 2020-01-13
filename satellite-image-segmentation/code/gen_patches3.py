from scipy import ndimage
import random
import numpy as np

def rotate_img(img, r):
    # channels in img are last!!!
    # r is a transformation type (an integer from 0 to 7)
    reverse_x = r % 2 == 1         # True for r in [1,3,5,7]
    reverse_y = (r // 2) % 2 == 1  # True for r in [2,3,6,7]
    swap_xy = (r // 4) % 2 == 1    # True for r in [4,5,6,7]
    if reverse_x:
        img = img[::-1, :]
    if reverse_y:
        img = img[:, ::-1]
    if swap_xy:
        img = img.transpose([1, 0, 2])
    return img   

def get_rand_patch(img, mask, sz=160, augment=True):
    """
    :param img: ndarray with shape (x_sz, y_sz, num_channels)
    :param mask: binary ndarray with shape (x_sz, y_sz, num_classes)
    :param sz: size of random patch
    :return: patch with shape (sz, sz, num_channels)
    """
    assert len(img.shape) == 3 and img.shape[0] > sz and img.shape[1] > sz and img.shape[0:2] == mask.shape[0:2]
    xc = random.randint(0, img.shape[0] - sz)
    yc = random.randint(0, img.shape[1] - sz)
    patch_img = img[xc:(xc + sz), yc:(yc + sz)]
    patch_mask = mask[xc:(xc + sz), yc:(yc + sz)]

    if augment:
        j = random.randint(0, 7)
        patch_img = rotate_img(patch_img, j)
        patch_mask = rotate_img(patch_mask, j)
    
    return patch_img, patch_mask

def get_patches(x_dict, y_dict, n_patches, sz=160):
    x = list()
    y = list()
    total_patches = 0
    while total_patches < n_patches:
        img_id = random.sample(x_dict.keys(), 1)[0]
        img = x_dict[img_id]
        mask = y_dict[img_id]

        img_patch, mask_patch = get_rand_patch(img, mask, sz, True)
        x.append(img_patch)
        y.append(mask_patch)
        total_patches += 1
    print('Generated {} patches'.format(total_patches))
    return np.array(x), np.array(y)


