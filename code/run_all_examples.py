import os
from os import listdir
from os.path import isfile, splitext

new_ext = '.mylang'

code_files = []

for f in listdir("."):
    if not isfile(f):
        print("skip " + f)
        continue
        
    root, ext = splitext(f)
    if ext == new_ext:
        code_files.append(f)
        
for code_file in code_files:
    print(f'----------- running "{code_file}": -----------')
    os.system(f' ..\\target\\debug\\new_language.exe {code_file} & cd code')