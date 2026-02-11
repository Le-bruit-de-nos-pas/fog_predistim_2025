import os
import glob
import gzip
import shutil

Current_Folder = os.getcwd()
VTAsRaw_Path = os.path.join(Current_Folder, 'ROIs/VTAs_Raw/')
EfieldsRaw_Path = os.path.join(Current_Folder, 'ROIs/Efields_Raw/')
