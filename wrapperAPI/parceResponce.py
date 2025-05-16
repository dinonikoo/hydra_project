import os
from fromHaskellToDsl.main import *
from fromJavaToDSL.main import *

def strToFile(json_string, source_language):
    ext = "" #расширение файла в зависимости от выбранного языка
    if source_language=="Java": ext = "java"
    elif source_language=="Python": ext="py"
    elif source_language=="Haskell": ext="hs"

    current_dir = os.getcwd()
    path_to_gen = os.path.join(current_dir, "wrapperAPI","genFiles",f"initialFile.{ext}")

    with open(path_to_gen,'w') as file: file.write(json_string)

def fileToDSL(path_to_file,source_language):
    pass