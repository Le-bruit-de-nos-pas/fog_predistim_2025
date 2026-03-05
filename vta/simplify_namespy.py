import os

folder = r"C:\Users\paulo\Desktop\raw"

for file in os.listdir(folder):
    if file.endswith(".nii.gz"):

        base = file.split("_")[0]      # sub-01004DJ
        base = base.replace("-", "")   # sub01004DJ

        new_name = base + ".nii.gz"
        old_path = os.path.join(folder, file)
        new_path = os.path.join(folder, new_name)

        counter = 2
        while os.path.exists(new_path):
            new_name = f"{base}{counter}.nii.gz"
            new_path = os.path.join(folder, new_name)
            counter += 1

        os.rename(old_path, new_path)
        print(f"{file} -> {new_name}")

print("Done.")