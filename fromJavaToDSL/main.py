from pathlib import Path
import re

valid_directory = False
TOKEN_REGEX = re.compile(r'''
    "(?:.\\|[^"])*"        |  # строковые литералы
    \d+                    |  # целые числа
    [A-Za-z_]\w*           |  # переменные
    ==|!=|<=|>=|&&|\|\|    |  # двойные операторы
    [+\-*/%<>=?:(){}\.]         # одиночные операторы и скобки
''', re.VERBOSE)


def parse_code(code):
    lines = code.strip().split('\n')
    structures = []
    current_structure = None
    current_fields = []
    current_functions = []

    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if len(line) == 0:
            i += 1
            continue

        if line.startswith("public class") or line.startswith("public interface"):
            if current_structure:
                current_structure["elements"] = current_fields + current_functions
                structures.append(current_structure)
            current_fields = []
            current_functions = []
            keywords = line.split()
            entity_type = keywords[1]  # class или interface
            entity_name = keywords[2]
            current_structure = {
                "name": entity_name,
                "type": entity_type,
                "elements": []
            }

        elif line.startswith("static"):  # начало функции
            function_lines = [line]
            brace_count = line.count("{") - line.count("}")
            i += 1

            while brace_count > 0 and i < len(lines):
                next_line = lines[i].strip()
                function_lines.append(next_line)
                brace_count += next_line.count("{") - next_line.count("}")
                i += 1

            function_block = " ".join(function_lines)
            function_signature = function_block.split("{")[0]
            return_statement = None

            if "return" in function_block:
                return_part = function_block.split("return")[1]
                return_statement = return_part.split(";")[0].strip()

            function_signature = function_signature.replace("static", "").strip()
            parts = function_signature.split("(")
            return_type_and_name = parts[0].strip().split()
            return_type = return_type_and_name[0]
            function_name = return_type_and_name[1]
            params = parts[1].replace(")", "").strip()
            params = params.split()
            param_type = params[0]
            param_name = params[1]

            tokens = tokenize_return(return_statement)
            ast = parse_return(tokens)

            current_functions.append({
                "type": "function",
                "name": function_name,
                "return_type": return_type,
                "parameter_type": param_type,
                "parameter_name": param_name,
                "return_statement": ast
            })
            continue  # уже увеличили i когда по строчкам скакали

        elif line.endswith(";"):
            if "=" in line:
                decl, value = line.split("=", 1)
                value = value.strip().strip(";").strip('"')
                parts = decl.strip().split()
                if len(parts) >= 2:
                    type_ = parts[-2]
                    name = parts[-1]
                    current_fields.append({
                        "type": "const",
                        "name": name,
                        "field_type": type_,
                        "value": value
                    })
            else:
                parts = line.strip().strip(";").split()
                if len(parts) >= 2:
                    type_ = parts[-2]
                    name = parts[-1]
                    current_fields.append({
                        "type": "field",
                        "name": name,
                        "field_type": type_
                    })

        i += 1

    if current_structure:
        current_structure["elements"] = current_fields + current_functions
        structures.append(current_structure)

    return structures

def tokenize_return(e):
    e = re.sub(r'([A-Za-z_]\w*)\.(toLowerCase|toUpperCase|isEmpty|length)',
                  lambda m: m.group(0) if m.group(1) == 'Character' else f'{m.group(2)}String({m.group(1)})', e)
    # у строчек и символов есть ряд функций с одинаковыми названиями, но они по разному называются.
    # чтобы потом не было путаницы, я аргумент ВСЕГДА пишу в скобочках и помечаю функцию String, если она для строчек
    # эта штука как раз все приводит в единый вид
    e = e.strip()
    tokens = TOKEN_REGEX.findall(e)
    #print(tokens, "токены")
    return tokens

def parse_return(tokens):
    # тернарные операторы
    def parse_ternary(tokens):
        condition = parse_logic(tokens)
        if len(tokens) > 0 and tokens[0] == '?':
            tokens.pop(0)  # убираем '?'
            then_expr = parse_logic(tokens)
            tokens.pop(0)  # убираем ':'
            else_expr = parse_logic(tokens)
            return {"type": "ternary", "condition": condition, "then": then_expr, "else": else_expr}
        return condition

    # логические операторы
    def parse_logic(tokens):
        left = parse_comparison(tokens)
        while len(tokens) > 0 and tokens[0] in ['&&', '||']:
            op = tokens.pop(0)
            right = parse_comparison(tokens)
            left = {"type": "binary", "operator": op, "left": left, "right": right}
        return left

    # сравнения, равенство
    def parse_comparison(tokens):
        left = parse_arithmetic(tokens)
        while len(tokens) > 0 and tokens[0] in ['==', '!=', '<', '>', '<=', '>=']:
            op = tokens.pop(0)
            right = parse_arithmetic(tokens)
            left = {"type": "binary", "operator": op, "left": left, "right": right}
        return left

    # арифметические операторы
    def parse_arithmetic(tokens):
        left = parse_unary(tokens)
        while len(tokens) > 0 and tokens[0] in ['+', '-', '*', '/', '%']:
            op = tokens.pop(0)
            right = parse_unary(tokens)
            left = {"type": "binary", "operator": op, "left": left, "right": right}
        return left

    def parse_unary(tokens): #это унарные операторы, а также переменные и разные литералы
        if len(tokens) == 0:
            return None

        if tokens[0] in ('!', '-'):
            op = tokens.pop(0)
            operand = parse_unary(tokens)
            return {"type": "unary", "operator": op, "operand": operand} # унарное: отрицание или минус

        # скобки
        if tokens[0] == '(':
            tokens.pop(0)
            expr = parse_ternary(tokens)
            if tokens[0] == ')':
                tokens.pop(0)
            return expr

        # число, сохраняется именно КАК ЧИСЛО
        if re.match(r'\d+', tokens[0]):
            return {"type": "literal", "value": int(tokens.pop(0))}

        if tokens[0].startswith('"') and tokens[0].endswith('"'):
            return {"type": "literal", "value": tokens.pop(0)[1:-1]}  # строчка без кавычек. ИМЕННО СТРОЧКА, поэтому путаницы не будет

        # перед тем как объявить токен просто переменной, нужно проверить не функция ли это
        if is_function_call_start(tokens):
            return parse_function_call(tokens)

        # variable - переменная
        if re.match(r'[A-Za-z_]\w*', tokens[0]):
            return {"type": "variable", "name": tokens.pop(0)}

        raise ValueError(f"Unexpected token: {tokens[0]}")

    def is_function_call_start(tokens):
        # обработка например Character . isLowerCase (
        if len(tokens) >= 4 and re.match(r'[A-Za-z_]\w*', tokens[0]) and tokens[1] == '.' and re.match(r'[A-Za-z_]\w*',
                                                                                                       tokens[2]) and \
                tokens[3] == '(':
            return True
        # обработка например toLowerCase (
        if len(tokens) >= 2 and re.match(r'[A-Za-z_]\w*', tokens[0]) and tokens[1] == '(':
            return True
        return False

    def parse_function_call(tokens): # проверка что вызывается функция
        name_parts = []
        while tokens and tokens[0] not in ['(', ')', ',', ';']:
            if tokens[0] == '.':
                tokens.pop(0)
            else:
                name_parts.append(tokens.pop(0))
            if tokens[0] == '(':
                break

        function_name = '.'.join(name_parts)

        tokens.pop(0)  # убираем (
        args = []
        while tokens[0] != ')':
            args.append(parse_ternary(tokens))  # допускаем вложенность. может быть несколько разных конструкций друг в друге
            if tokens[0] == ',':
                tokens.pop(0)
        tokens.pop(0)  # убираем )

        return {"type": "function_call", "name": function_name, "arguments": args}

    return parse_ternary(tokens)



def main_():
    valid_directory = False
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

main_()