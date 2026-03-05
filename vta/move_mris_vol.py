import os
import shutil
import time

source_root = r"D:\mri_predistim\20251113-135007\3DT1\rawdata"
destination = r"C:\Users\paulo\Desktop\raw"

os.makedirs(destination, exist_ok=True)

MAX_TRIES = 10

for root, dirs, files in os.walk(source_root):
    for file in files:
        if file.endswith(".nii.gz"):

            source_file = os.path.join(root, file)
            dest_file = os.path.join(destination, file)

            for attempt in range(1, MAX_TRIES + 1):
                try:
                    shutil.copy2(source_file, dest_file)
                    print(f"Copied: {file}")
                    break

                except Exception as e:
                    print(f"Attempt {attempt}/10 failed for {file}: {e}")
                    time.sleep(1)

                    if attempt == MAX_TRIES:
                        print(f"FAILED after 10 attempts: {file}")

print("Done.")