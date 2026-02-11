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



file_dict = {}

for file_name in os.listdir(os.path.join(VTAs_Paths[res], "Lateral")):
    if f"-R_{res}" in file_name or f"-L_{res}" in file_name:
        parts = file_name.split("_", 2)
        prefix = "_".join(parts[:1])
        
        if prefix in file_dict:
            file_dict[prefix].append(os.path.join(VTAs_Paths[res], "Lateral", file_name))
        else:
            file_dict[prefix] = [os.path.join(VTAs_Paths[res], "Lateral", file_name)]

print(f"{len(file_dict)} pairs found")
            
for prefix in file_dict.keys():
    if len(file_dict[prefix]) == 2:
        img1 = nib.load(file_dict[prefix][0])
        img2 = nib.load(file_dict[prefix][1])
        
        data1 = img1.get_fdata()
        data2 = img2.get_fdata()
        
        if data1.shape != data2.shape:
            raise ValueError("The dimensions of the two images do not match!")
        
        combined_data = data1 + data2
        
        combined = nib.Nifti1Image(combined_data, affine=img1.affine)
        
        output_path = os.path.join(VTAs_Paths[res], f"{prefix}_{res}.nii.gz")
        
        nib.save(combined, output_path)
        os.remove(file_dict[prefix][0])
        os.remove(file_dict[prefix][1])
        print(output_path)
        
print("Merging done.")




threshold_value = 200 # [V/m] Change if needed
mask_image = nib.load(Mask_Paths[res])
EfieldsRaw_Paths = glob.glob(os.path.join(EfieldsRaw_Path, '**', '*'), recursive=True)

for Efield_path in EfieldsRaw_Paths:
    img = nib.load(Efield_path)
    img_data = img.get_fdata()
    img_data_thresholded = np.where(img_data < threshold_value, 0, img_data)
    img_data_thresholded = img_data_thresholded.astype(np.float32)
    img_thresholded = nib.Nifti1Image(img_data_thresholded, img.affine)

    resliced_img = resample_to_img(img_thresholded, mask_image, interpolation='nearest')
    
    new_path = Efields_Paths[res] + "/Lateral/" + os.path.basename(Efield_path).replace('.', f'_{res}_{threshold_value}Vm.', 1)
    
    nib.save(resliced_img, new_path)
    print(new_path)
    
print("Resampling done.")




file_dict = {}

for file_name in os.listdir(os.path.join(Efields_Paths[res], "Lateral")):
    if f"-R_{res}" in file_name or f"-L_{res}" in file_name:
        parts = file_name.split("_", 2)
        prefix = "_".join(parts[:1])
        
        if prefix in file_dict:
            file_dict[prefix].append(os.path.join(Efields_Paths[res], "Lateral", file_name))
        else:
            file_dict[prefix] = [os.path.join(Efields_Paths[res], "Lateral", file_name)]

print(f"{len(file_dict)} pairs found")

for prefix in file_dict.keys():
    if len(file_dict[prefix]) == 2:
        img1 = nib.load(file_dict[prefix][0])
        img2 = nib.load(file_dict[prefix][1])
        
        data1 = img1.get_fdata()
        data2 = img2.get_fdata()
        
        if data1.shape != data2.shape:
            raise ValueError("The dimensions of the two images do not match!")
        
        combined_data = data1 + data2
        
        combined = nib.Nifti1Image(combined_data, affine=img1.affine)
        
        output_path = os.path.join(Efields_Paths[res], f"{prefix}_{res}_{threshold_value}Vm.nii.gz")
        
        nib.save(combined, output_path)
        os.remove(file_dict[prefix][0])
        os.remove(file_dict[prefix][1])
        print(output_path)
        
print("Merging done.")
