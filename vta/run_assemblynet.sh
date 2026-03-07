
#!/bin/bash
#
# AssemblyNet batch runner
#
# Usage:
# chmod +x run_assemblynet.sh
# ./run_assemblynet.sh
#
sudo docker run --rm \
-v /mnt/c/Users/paulo/MRI/inputs:/data \
-v /mnt/c/Users/paulo/MRI/outputs:/data_out \
-e CUDA_VISIBLE_DEVICES="" \
volbrain/assemblynet:1.0.0 \
-recursive \
-pattern-t1 "*.nii*" \
-global-csv /data_out/global_volumetry_info.csv \
-no-pdf-report \
-batch-size 1 \
/data/ /data_out/
