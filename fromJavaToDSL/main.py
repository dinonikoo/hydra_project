import sys
from pathlib import Path

# Добавляем текущую директорию в пути поиска
sys.path.append(str(Path(__file__).parent))

from javaParser import *
from translationDSL import *
from pathlib import Path

#def check_directory(directory_):
#    if Path(directory_).exists():
     #   java_list = list(Path(directory_).rglob("*.java"))
        #print("Директория существует, обнаружено ", len(java_list))
        # директория существует

def convertFileAPI(path_to_file):
    result = parse(path_to_file)
    a = generate(result)
    return a

# if __name__ == "__main__":
#     print("Пожалуйста, введите директорию с файлами Java:")
#     #directory = input()
#     directory = "C:\\Users\katya\Desktop\example"

#     while not(check_directory(directory)):
#         print("Ошибка: неверно указана директория.")
#         print("Пожалуйста, введите директорию с файлами Java:")
#         directory = input()

#     result = parse(directory)
#     a = generate(result)
#     print(a)