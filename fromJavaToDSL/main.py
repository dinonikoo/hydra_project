from javaParser import *
from translationDSL import *

def check_directory(directory_):
    if Path(directory_).exists():
        java_list = list(Path(directory_).rglob("*.java"))
        #print("Директория существует, обнаружено ", len(java_list))
        # директория существует
        return True
    else:
        return False

if __name__ == "__main__":
    #print("Пожалуйста, введите файл")
    #directory = input()
    #directory = "C:\\Users\katya\Desktop\example"
    filename = input("Пожалуйста, введите путь к файлу: ")
    while not(check_directory(filename)):
        print("Ошибка: неверно указана директория.")
        print("Пожалуйста, введите путь к файлу: ")
        filename = input()

    with open(filename, "r", encoding="utf-8") as f:
        content = f.read()
    #print(content)

    result = parse(filename)
    a = generate(result)
    print(a)