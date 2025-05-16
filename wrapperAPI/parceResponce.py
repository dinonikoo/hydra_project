import os
import sys
from pathlib import Path

# Добавить корень проекта в PYTHONPATH
project_root = Path(__file__).parent.parent
sys.path.append(str(project_root))

from fromJavaToDSL.main import *
from fromHaskellToDsl.main import *

current_dir = os.getcwd()
current_dir = os.path.join(current_dir, "wrapperAPI","genFiles")

def strToFile(json_string, source_language):
    ext = "" #расширение файла в зависимости от выбранного языка
    if source_language=="Java": ext = "java"
    elif source_language=="Python": ext="py"
    elif source_language=="Haskell": ext="hs"

    path_to_gen = os.path.join(current_dir,f"initialFile.{ext}")

    with open(path_to_gen,'w') as file: file.write(json_string)

def fileToDSL(path_to_file_dir,source_language):
    if source_language=="Java":
        res = convertFileAPI(path_to_file_dir)
    elif source_language=="Haskell":
        res = convertFileAPIHaskell(path_to_file_dir)

    if res==False:
        path_to_gen = os.path.join(current_dir,f"Error.txt")
        with open(path_to_gen,'w') as file: file.write("Transcription to DSL failed")
        return
    else:
        path_to_gen = os.path.join(current_dir,f"genDSL.hs")
        with open(path_to_gen,'w') as file: file.write(res)

# testStr = """
# main :: IO 
# main = do
#     let x = 10
# """
# strToFile(testStr,"Haskell")
# fileToDSL(current_dir,"Haskell")