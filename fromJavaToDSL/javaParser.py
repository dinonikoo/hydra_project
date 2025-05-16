#TODO: наследование?
#TODO: сделать замену на  Static через одну регулярку + возможно объединить логику function_call и method_call
#TODO: проверить работу floorMod

from pathlib import Path
import re

TOKEN_REGEX = re.compile(r'''
    '(?:\\.|[^'])'         |  # символьные литералы  - чтобы не путать 'A' и A, где 'A' char, A - переменная
    "(?:.\\|[^"])*"        |  # строковые литералы - аналогично для строк чтоб не путать
    \d+                    |  # целые числа
    [A-Za-z_]\w*           |  # переменные
    ==|!=|<=|>=|&&|\|\|    |  # двойные операторы
    [+\-*/%<>=?:(){}\.]         # одиночные операторы и скобки
''', re.VERBOSE)



def parse_code(code):
    #поддержка вложенности - например для List, внутри может быть много List, вложенных друг в друга
    def parse_type(type_str):
        type_str = type_str.strip()

        if type_str.startswith("List<") and type_str.endswith(">"):
            inner = type_str[5:-1].strip()
            return {'type': 'List', 'of': parse_type(inner)}
        else:
            return {'type': type_str}

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
        elif "=" in line and not line.endswith(";"):
            # начало многострочного поля
            multiline_field = [line]
            i += 1
            while i < len(lines) and not lines[i].strip().endswith(";"):
                multiline_field.append(lines[i].strip())
                i += 1
            if i < len(lines):
                multiline_field.append(lines[i].strip())
            line = " ".join(multiline_field).strip()
            # собрали поле, даже если оно на разных строчках (как например массивы)
            if "=" in line:
                line = re.sub(r'\bjava\.util\.List<', 'List<', line)
                # чтобы привести все к одному виду - List
                decl, value = line.split("=", 1)
                value = value.strip().strip(";")
                parts = decl.strip().split()
                tokens_ = tokenize_return(value)
                ast_ = parse_return(tokens_)
                if len(parts) >= 2:
                    type_ = parts[-2]
                    name = parts[-1]
                    current_fields.append({
                        "type": "const",
                        "name": name,
                        "field_type": parse_type(type_), # разбор типа на части,
                        # в итоге будет {'type': 'List', 'of': {'type': 'String'}} например
                        "value_ast": ast_ # разбор на части, предполагается, что могут быть вызовы функций
                    })
            i += 1
            continue
        elif line.endswith(";"):
            if "=" in line:
                decl, value = line.split("=", 1)
                value = value.strip().strip(";")
                tokens_ = tokenize_return(value)
                ast_ = parse_return(tokens_)
                parts = decl.strip().split()
                if len(parts) >= 2:
                    type_ = parts[-2]
                    name = parts[-1]
                    current_fields.append({
                        "type": "const",
                        "name": name,
                        "field_type": type_,
                        "value": ast_
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
    def method_replacer(m):
        method = m.group(1)
        return f'Static{method}'

    #заменим все функции Character.функция на Charфункция - чтобы не путать с функциями, которые вызываем через объект
    e = re.sub(r'\bCharacter\.(toLowerCase|toUpperCase|isLowerCase|isUpperCase)', method_replacer, e)
    e = re.sub(r'\bMath\.(floorMod)', method_replacer, e)

    # Arrays.asList и java.util.Arrays.asList --> asList
    e = re.sub(r'\b(?:java\.util\.)?Arrays\.asList', 'asList', e)
    e = e.strip()
    tokens = TOKEN_REGEX.findall(e)

    return tokens

def parse_return(tokens):
    # тернарные операторы
    def parse_ternary(tokens_):
        condition = parse_logic(tokens_)
        if len(tokens_) > 0 and tokens_[0] == '?':
            tokens_.pop(0)  # убираем '?'
            then_expr = parse_logic(tokens_)
            tokens_.pop(0)  # убираем ':'
            else_expr = parse_logic(tokens_)
            return {"type": "ternary", "condition": condition, "then": then_expr, "else": else_expr}
        return condition

    # логические операторы
    def parse_logic(tokens_):
        left = parse_comparison(tokens_)
        while len(tokens_) > 0 and tokens_[0] in ['&&', '||']:
            op = tokens_.pop(0)
            right = parse_comparison(tokens_)
            left = {"type": "binary", "operator": op, "left": left, "right": right}
        return left

    # сравнения, равенство
    def parse_comparison(tokens_):
        left = parse_arithmetic(tokens_)
        while len(tokens_) > 0 and tokens_[0] in ['==', '!=', '<', '>', '<=', '>=']:
            op = tokens_.pop(0)
            right = parse_arithmetic(tokens_)
            left = {"type": "binary", "operator": op, "left": left, "right": right}
        return left

    # арифметические операторы
    def parse_arithmetic(tokens_):
        return parse_additive(tokens_)

    # РАЗНЫЙ УРОВЕНЬ ПРИОРИТЕТА, ПОЭТОМУ РАЗВЕЛИ additive и multiplicative
    def parse_additive(tokens_):
        left = parse_multiplicative(tokens_)
        while tokens_ and tokens_[0] in ['+', '-']:
            op = tokens_.pop(0)
            right = parse_multiplicative(tokens_)
            left = {"type": "binary", "operator": op, "left": left, "right": right}
        return left

    def parse_multiplicative(tokens_):
        left = parse_unary(tokens_)
        while tokens_ and tokens_[0] in ['*', '/', '%']:
            op = tokens_.pop(0)
            right = parse_unary(tokens_)
            left = {"type": "binary", "operator": op, "left": left, "right": right}
        return left

    def parse_unary(tokens_): #это унарные операторы, а также переменные и разные литералы
        if len(tokens_) == 0:
            return None

        if tokens_[0] in ('!', '-'):
            op = tokens_.pop(0)
            operand = parse_unary(tokens_)
            return {"type": "unary", "operator": op, "operand": operand} # унарное: отрицание или минус

        #скобки
        if tokens_[0] == '(':
            tokens_.pop(0)
            expr = parse_ternary(tokens_)
            if tokens_ and tokens_[0] == ')':
                tokens_.pop(0)
            return parse_postfix_chain(expr, tokens_)

        if re.match(r'\d+', tokens_[0]):
            expr = {"type": "literal", "value": int(tokens_.pop(0))}
            return parse_postfix_chain(expr, tokens_)

        if tokens_[0].startswith('"') and tokens_[0].endswith('"'):
            val = tokens_.pop(0)[1:-1]
            expr = {"type": "literal", "value": val, "value_type": "string"}
            return parse_postfix_chain(expr, tokens_)

        if tokens_[0].startswith("'") and tokens_[0].endswith("'") and len(tokens_[0]) >= 3:
            char_token = tokens_.pop(0)
            val = bytes(char_token[1:-1], "utf-8").decode("unicode_escape")
            expr = {"type": "literal",
                    "value": val,
                    "value_type": "char"}
            return parse_postfix_chain(expr, tokens_)

        # вызов функции?
        if is_function_call_start(tokens_):
            expr = parse_function_call(tokens_)
            return parse_postfix_chain(expr, tokens_)

        if re.match(r'[A-Za-z_]\w*', tokens_[0]):
            expr = {"type": "variable",
                    "name": tokens_.pop(0)}
            return parse_postfix_chain(expr, tokens_)

        return None

        #raise ValueError("Внимание: неизвестный токен ", tokens_[0])

    # это нужно для поддержки вызовов-цепочек через точку. Такие методы будут method_call - в отличие function_call для символов
    # т.к. Character.методы статические
    def parse_postfix_chain(expr, tokens_):
        while tokens_ and tokens_[0] == '.':
            tokens_.pop(0)
            if tokens_ and re.match(r'[A-Za-z_]\w*', tokens_[0]):
                method_name = tokens_.pop(0)
                if tokens_ and tokens_[0] == '(':
                    tokens_.pop(0)  # убираем (
                    args = []
                    while tokens_ and tokens_[0] != ')':
                        args.append(parse_ternary(tokens_))
                        if tokens_ and tokens_[0] == ',':
                            tokens_.pop(0)
                    if tokens_ and tokens_[0] == ')':
                        tokens_.pop(0)  # убираем )
                    expr = {
                        "type": "method_call",
                        "caller": expr,
                        "method": method_name,
                        "arguments": args
                    }
                else:
                    expr = {
                        "type": "field_access",
                        "caller": expr,
                        "field": method_name
                    }
        return expr

    def is_function_call_start(tokens_):
        if len(tokens_) >= 2 and re.match(r'[A-Za-z_]\w*', tokens_[0]) and tokens_[1] == '(':
            return True
        if len(tokens_) >= 4 and tokens_[1] == '.' and tokens_[3] == '(':
            return True
        return False

    def parse_function_call(tokens_): # проверка что вызывается функция
        name_parts = []
        while tokens_ and tokens_[0] not in ['(', ')', ',', ';']:
            if tokens_[0] == '.':
                tokens_.pop(0)
            else:
                name_parts.append(tokens_.pop(0))
            if tokens_[0] == '(':
                break

        function_name = '.'.join(name_parts)

        tokens_.pop(0)  # убираем (
        args = []
        while tokens_[0] != ')':
            args.append(parse_ternary(tokens_))  # допускаем вложенность. может быть несколько разных конструкций друг в друге
            if tokens_[0] == ',':
                tokens_.pop(0)
        tokens_.pop(0)  # убираем )

        return {"type": "function_call", "name": function_name, "arguments": args}

    return parse_ternary(tokens)

def parse(directory):
    result = []
    java_list = list(Path(directory).rglob("*.java"))
    if len(java_list) == 0:
        print("Внимание: найдено 0 файлов *.java")
    for file in java_list:
        with open(file, 'r') as c:
            code = c.read()
            structures = parse_code(code)
            print(structures)
            result.append(structures)
    return result