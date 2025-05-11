from pathlib import Path

valid_directory = False

def parse_code(code):
    lines = code.strip().split('\n')
    #print(lines)
    structures = []
    current_structure = None

    current_fields = []

    for line in lines:
        line = line.strip()
        if len(line) == 0:
            continue
        if line.startswith("public class") or line.startswith("public interface"):
            if current_structure:
                current_structure["elements"] = current_fields
                structures.append(current_structure)
            current_fields = []
            keywords = line.split()
            entity_type = keywords[1] # получаем тип
            entity_name = keywords[2]
            #print(entity_type, entity_name)
            current_structure = {
                "name": entity_name,
                "type": entity_type,
                "elements": []
            }
        elif line[-1] == ';':
            if "=" in line: # объявляется значение
                decl, value = line.split("=", 1)
                value = value.strip().strip(";").strip('"')
                parts = decl.strip().split()
                if len(parts) >= 2:
                    type = parts[-2]  # тип перед именем
                    name = parts[-1]
                    current_fields.append({
                        "type": "const",
                        "name": name,
                        "field_type": type,
                        "value": value
                    })
            else:
                parts = line.strip().strip(";").split()
                if len(parts) >= 2:
                    type = parts[-2]
                    name = parts[-1]
                    current_fields.append({
                        "type": "field",
                        "name": name,
                        "field_type": type
                    })

            #print(current_fields)

    if current_structure:
        current_structure["elements"] = current_fields
        structures.append(current_structure)

    return structures


while (not(valid_directory)):
    print("Пожалуйста, введите директорию с файлами Java:")
    #directory = input()
    directory = "C:\\Users\\katya\Desktop"
    if Path(directory).exists():
        # директория существует
        valid_directory = True
        java_list = list(Path(directory).rglob("*.java"))
        if len(java_list) == 0:
            print("Внимание: найдено 0 файлов *.java")
        for file in java_list:
            with open(file, 'r') as c:
                code = c.read()
                structures = parse_code(code)
                print(structures)

    else:
        print("Ошибка: неверно указана директория.")