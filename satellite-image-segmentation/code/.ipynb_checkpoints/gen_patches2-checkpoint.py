from scipy import ndimage
import random
import numpy as np

def get_rand_patch(img, mask, sz=160):
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
    
    return patch_img, patch_mask

def get_patches(x_dict, y_dict, n_patches, sz=160):
    x = list()
    y = list()
    total_patches = 0
    while total_patches < n_patches:
        img_id = random.sample(x_dict.keys(), 1)[0]
        img = x_dict[img_id]
        mask = y_dict[img_id]
        
        imgTransformType = random.randint(0,3)
    
        if(imgTransformType == 0):
            pass
        elif(imgTransformType == 1): 
            img = img[:,::-1,:] #ndimage.rotate(img, angle=45, reshape=False, axes=(1,2))
            mask = mask[:,::-1,:] #ndimage.rotate(mask, angle=45, reshape=False, axes=(1,2))
        elif(imgTransformType == 2):
            img = img[:,:,::-1] #ndimage.rotate(img, angle=90, reshape=False, axes=(1,2))
            mask = mask[:,:,::-1] #ndimage.rotate(mask, angle=90, reshape=False, axes=(1,2))
        elif(imgTransformType == 3):
            img = img[:,::-1,::-1] #ndimage.rotate(img, angle=135, reshape=False, axes=(1,2))
            mask = mask[:,::-1,::-1] #ndimage.rotate(mask, angle=135, reshape=False, axes=(1,2))
        #elif(imgTransformType == 4):
        #    img = ndimage.rotate(img, angle=180, reshape=False, axes=(1,2))
        #    mask = ndimage.rotate(mask, angle=180, reshape=False, axes=(1,2))
        #elif(imgTransformType == 5):
        #    img = ndimage.rotate(img, angle=225, reshape=False, axes=(1,2))
        #    mask = ndimage.rotate(mask, angle=225, reshape=False, axes=(1,2))
        #elif(imgTransformType == 6):
        #    img = ndimage.rotate(img, angle=270, reshape=False, axes=(1,2))
        #    mask = ndimage.rotate(mask, angle=270, reshape=False, axes=(1,2))
        #elif(imgTransformType == 7):
        #    img = ndimage.rotate(img, angle=315, reshape=False, axes=(1,2))
        #    mask = ndimage.rotate(mask, angle=315, reshape=False, axes=(1,2))

        img_patch, mask_patch = get_rand_patch(img, mask, sz)
        x.append(img_patch)
        y.append(mask_patch)
        total_patches += 1
    print('Generated {} patches'.format(total_patches))
    return np.array(x), np.array(y)


