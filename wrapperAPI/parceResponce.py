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


global_dir = os.getcwd()
current_dir = os.path.join(global_dir, "wrapperAPI","genFiles")
SCOTTY_URL = "http://localhost:3000/translate"

headersDSL = """import Hydra.Codegen
    ( TTerm, TElement, Module(Module), Namespace(Namespace) )
import Hydra.Dsl.Base as Base ( definitionInModule, el, string )
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Io        as Io
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Sets      as Sets
import Hydra.Dsl.Lib.Strings as Strings ()
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import Hydra.Sources.Tier0.Core
    ( TTerm, TElement, Module(Module), Namespace(Namespace) )
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

"""

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
        # name_module_i = -1
        # for i in range(len(res)):
        #     if res[i]==":" and res[i+1]==":":
        #         name_module_i = i
        #         continue
        #     if name_module_i!=-1 and res[i]=="\n": 
                
        # res = "{-# LANGUAGE OverloadedStrings #-}\n\n module Hydra.GenDSL where\n\n"+headersDSL+res
        path_to_gen = os.path.join(current_dir,f"genDSL.hs")
        with open(path_to_gen,'w') as file: file.write(res)
        return res

def DSLToLang(dsl_code, target_language):
    # res = "" #содержимое DSL в виде строки
    # with open(path_to_file,'r') as f: res = f.read()
    payload = {
    "dslCode": dsl_code,
    "targetLang": target_language,
    "outputDir": current_dir
    }

    # Отправляем POST-запрос на сервер
    responce = requests.post(SCOTTY_URL, json=payload)
    return responce

# with open(os.path.join(current_dir,"genDSL.hs"),'r') as f: res = f.read()
# DSLToLang(res,"Java")

# testStr = """public class Main
# {
#     public void main()
#     {
#         string s = "Hello";
#     } 
# }
# """
# strToFile(testStr,"Java")
# fileToDSL("Java")
# path_to_genDSL = os.path.join(current_dir,"genDSL.hs")
# DSLToLang(path_to_genDSL,"Java")

# Адрес Scotty-сервера (порт 3000)


# DSL-код, который ты хочешь отправить
# Параметры для перевода


# Обработка ответа

