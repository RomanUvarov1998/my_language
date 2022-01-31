from os import listdir
from os.path import isfile, splitext

new_ext = '.mylang'

for f in listdir("."):
    if not isfile(f):
        print("skip " + f)
        continue
        
    root, ext = splitext(f)
    if ext == new_ext or ext == ".py":
        print("skip " + f)
        continue
    
    base_file = f
    new_file = root + new_ext
    
    with open(base_file , 'r') as f1:
        with open(new_file, 'w') as f2:
            f2.write(f1.read())
            
    print("renamed '" + base_file + "' to '" + new_file +"'")