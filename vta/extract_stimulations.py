import os
import glob
import gzip
import shutil

Current_Folder = os.getcwd()
VTAsRaw_Path = os.path.join(Current_Folder, 'ROIs/VTAs_Raw/')
EfieldsRaw_Path = os.path.join(Current_Folder, 'ROIs/Efields_Raw/')


Dataset_Path = "D:\\DM\\Projects\\MANIA\\Dataset\\derivatives\\leaddbs\\"
Subject_Paths = glob.glob(Dataset_Path + '*sub-*')
Total_Subjects = len(Subject_Paths)

print(f"{Total_Subjects} subjects were found.")


for subject_path in Subject_Paths:
    subject_id = os.path.basename(subject_path)[4:]
    search_stim = os.path.join(subject_path, f"stimulations/MNI152NLin2009bAsym/{subject_id}*/")
    stim_folders = glob.glob(search_stim)
    
    for folder in stim_folders:
        folder_name = os.path.basename(os.path.normpath(folder)).upper()
        files = ["*binary*L.nii", "*binary*R.nii", "*efield_*L.nii", "*efield_*R.nii"]
        
        for pattern in files:
            matched_files = glob.glob(os.path.join(folder, pattern))
            
            for file in matched_files:
                if file.endswith('L.nii'):
                    new_filename = f"{folder_name}_L.nii.gz"
                else:
                    new_filename = f"{folder_name}_R.nii.gz"
                
                if 'efield_' in pattern:
                    destination = os.path.join(EfieldsRaw_Path, new_filename)
                else:
                    destination = os.path.join(VTAsRaw_Path, new_filename)
                
                with open(file, 'rb') as f_in:
                    with gzip.open(destination, 'wb') as f_out:
                        shutil.copyfileobj(f_in, f_out)

print("File copying completed.")
