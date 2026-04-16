import os
import shutil

# Source root folder
src_root = r"D:\\mri_predistim\\20251113-135007\\3DT1\\rawdata"

# Destination folder (change username if needed)
dst_root = r"C:\\Users\\paulo\Desktop\\all_T1w_MRIs"

# Create destination folder if it doesn't exist
os.makedirs(dst_root, exist_ok=True)

# Walk through all subdirectories
for root, dirs, files in os.walk(src_root):
    for file in files:
        if file.endswith("T1w.nii.gz"):
            src_file = os.path.join(root, file)

            # To avoid overwriting, include part of the path in filename
            patient_id = root.split("\\")[-4] if "sub-" in root else "unknown"
            new_name = f"{patient_id}_{file}"

            dst_file = os.path.join(dst_root, new_name)

            shutil.copy2(src_file, dst_file)
            print(f"Copied: {src_file} -> {dst_file}")

print("Done!")