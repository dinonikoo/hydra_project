import os
import sys
from pathlib import Path
import subprocess 
import shutil
import requests


# Добавить корень проекта в PYTHONPATH
project_root = Path(__file__).parent.parent
sys.path.append(str(project_root))

from fromJavaToDSL.main import *
from fromHaskellToDsl.HaskellToDsl import *
from fromPythonToDSL.main import *
from Parsers.haskell_parser import *
from Parsers.java_parser import *
from Parsers.python_parser import *


global_dir = os.getcwd()
current_dir = os.path.join(global_dir, "hydra_project", "wrapperAPI","genFiles")
SCOTTY_URL = "http://localhost:3000/translate"

def strToFile(json_string, source_language):
    ext = "" #расширение файла в зависимости от выбранного языка
    if source_language=="Java": ext = "java"
    elif source_language=="Python": ext="py"
    elif source_language=="Haskell": ext="hs"

    path_to_gen = os.path.join(current_dir,f"initialFile.{ext}")

    with open(path_to_gen,'w') as file: file.write(json_string)


def fileToDSL(source_language):
    if source_language=="Java":
        res = convertFileAPI(os.path.join(current_dir, "initialFile.java"))
    elif source_language=="Haskell":
        res = convertFileAPIHaskell(current_dir)
    elif source_language=="Python":
        with open(os.path.join(current_dir, "initialFile.py")) as f: src = f.read()
        res = to_haskell_module(src)

    if res==False:
        path_to_gen = os.path.join(current_dir,"Error.txt")
        with open(path_to_gen,'w') as file: file.write("Transcription to DSL failed")
        return False
    else:
        path_to_gen = os.path.join(current_dir,f"genDSL.hs")
        with open(path_to_gen,'w') as file: file.write(res)
        return res

def DSLToLang(dsl_code, target_language):
    payload = {
    "dslCode": dsl_code,
    "targetLang": target_language,
    "outputDir": current_dir
    }

    # Отправляем POST-запрос на сервер
    responce = requests.post(SCOTTY_URL, json=payload)
    return responce

def parseTranslatedFile(target_lang):
    parsedFilesPath = os.path.join(current_dir,"parsedFiles")
    for root, dirs, files in os.walk(os.path.join(current_dir,"hydra")):
            for file in files:
                full_path = os.path.join(root, file)
                out_file = os.path.join(parsedFilesPath,file)
                if target_lang=="Haskell" and full_path.endswith(".hs"): parseHydraHaskell(full_path,out_file)
                elif target_lang=="Python" and full_path.endswith(".py"): parseHydraPython(full_path,out_file)
                elif target_lang=="Java" and full_path.endswith(".java"): parseHydraJava(full_path,out_file)

        
