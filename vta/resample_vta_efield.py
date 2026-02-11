import os
import glob
import numpy as np
import nibabel as nib
from nilearn.image import resample_to_img

Current_Folder = os.getcwd()

Mask2mm_Path = os.path.join(Current_Folder, 'Masks/MNI152_2mm_brain_mask.nii.gz')
Mask05mm_Path = os.path.join(Current_Folder, 'Masks/MNI152_05mm_brain_mask.nii.gz')

VTAsRaw_Path = os.path.join(Current_Folder, 'ROIs/VTAs_Raw/')
EfieldsRaw_Path = os.path.join(Current_Folder, 'ROIs/Efields_Raw/')
VTAs2mm_Path = os.path.join(Current_Folder, 'ROIs/VTAs_2mm/')
Efields2mm_Path = os.path.join(Current_Folder, 'ROIs/Efields_2mm/')
VTAs05mm_Path = os.path.join(Current_Folder, 'ROIs/VTAs_05mm/')
Efields05mm_Path = os.path.join(Current_Folder, 'ROIs/Efields_05mm/')


res = '2mm'
# res = '05mm'
Mask_Paths = {
    '2mm': Mask2mm_Path,
    '05mm': Mask05mm_Path
}
VTAs_Paths = {
    '2mm': VTAs2mm_Path,
    '05mm': VTAs05mm_Path
}
Efields_Paths = {
    '2mm': Efields2mm_Path,
    '05mm': Efields05mm_Path
}


mask_image = nib.load(Mask_Paths[res])
VTAsRaw_Paths = glob.glob(os.path.join(VTAsRaw_Path, '**', '*'), recursive=True)

for VTA_path in VTAsRaw_Paths:
    img = nib.load(VTA_path)
    img_data = img.get_fdata()
    img_data = img_data.astype(np.float32)
    img = nib.Nifti1Image(img_data, img.affine)
    
    resliced_img = resample_to_img(img, mask_image, interpolation='nearest')
    
    new_path = VTAs_Paths[res] + "/Lateral/" + os.path.basename(VTA_path).replace('.', f'_{res}.', 1)

    nib.save(resliced_img, new_path)
    print(new_path)
    
print("Resampling done.")
