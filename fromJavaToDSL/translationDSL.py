#TODO: необходимо добавить импорты к финальному файлу

#TODO: все сделать в один модуль?

#TODO: заменять функции из структуры на функции из гидры
#TODO: тернарный оператор


def indent(text, level): # отступы
    return "\n".join("  " * level + line for line in text.splitlines())

def hydra_type(java_type):
    # перевод типов данных для полей класса
    if java_type in ("Integer", "int", "integer"):
        return "Types.int32"
    elif java_type in ("Short", "short"):
        return "Types.int16"
    elif java_type in ("Long", "long"):
        return "Types.int64"
    elif java_type in ("Boolean", "boolean"):
        return "Types.boolean"
    elif java_type == "String":
        return "Types.string"
    elif isinstance(java_type, dict) and java_type.get("type") == "List":
        inner = hydra_type(java_type["of"])
        return f"Types.list {inner}"
    else:
        return "Types.string"

def hydra_type_TElement(java_type):
    # при прохождении list в конце остается 'type': 'String', который явл. словарём
    if isinstance(java_type, dict):
        #print(java_type, isinstance(java_type, dict), java_type.get("type"))
        if java_type.get("type") in ("Integer", "int", "integer"):
            return "Int"
        elif java_type.get("type") in ("Boolean", "boolean"):
            return "Bool"
        elif java_type.get("type") == "String":
            return "String"

    # перевод типов данных для функций (тип параметра и возвр. значения - если java_type обычная строчка)
    if java_type in ("Integer", "int", "integer"):
        return "Int"
    elif java_type in ("Boolean", "boolean"):
        return "Bool"
    elif java_type == "String":
        return "String"
    elif isinstance(java_type, dict) and java_type.get("type") == "List":
        inner = hydra_type_TElement(java_type["of"])
        #print(inner, java_type["of"])
        return f"[{inner}]"
    else:
        return "unknown"

def format_value(value_ast):
    if value_ast["type"] == "literal":
        val = value_ast["value"]
        if "value_type" in value_ast:
            if value_ast["value_type"] == "string":
                return f'Base.string "{val}"'
            else:
                return f"Base.int32 {ord(val)}"
        else:
            return f"Base.int32 {val}"
        #return f'Base.string "{val}"' if isinstance(val, str) else f"Base.int32 {val}"
    elif value_ast["type"] == "function_call":
        function_map = {
            "Character.isLowerCase": "Chars.isLower",
            "Character.isUpperCase": "Chars.isUpper",
            "Character.toUpperCase": "Chars.toUpper",
            "Character.toLowerCase": "Chars.toLower",
            "isEmptyString": "Strings.isEmpty",
            "lengthString": "Strings.length",
            "toLowerCaseString": "Strings.toLower",
            "toUpperCaseString": "Strings.toUpper",
        }
        func = value_ast["name"]
        args = [format_value(arg) for arg in value_ast["arguments"]]
        mapped_func = function_map.get(func, func)

        if func == "asList":
            return f"Base.list [{', '.join(args)}]"
        elif func in function_map:
            return f"{mapped_func}({args[0]})" if len(args) == 1 else f"{mapped_func}({')('.join(args)})"
        else:
            return f"{func}({', '.join(args)})"

    elif value_ast["type"] == "binary":
        left = format_value(value_ast["left"])
        right = format_value(value_ast["right"])
        op = value_ast["operator"]
        op_map = {
            "+": "Math.add",
            "-": "Math.sub",
            "*": "Math.mul",
            "/": "Math.div",
            "%": "Math.rem",
            "==": "Equality.equalInt32",
            ">": "Equality.gtInt32",
            ">=": "Equality.gteInt32",
            "<": "Equality.ltInt32",
            "<=": "Equality.lteInt32"
        }
        if op in op_map:
            return f'{op_map[op]} ({left}) ({right})'
        else:
            return f"({left} {op} {right})"

    elif value_ast["type"] == "variable":
        return f'Base.var "{value_ast["name"]}"'

    elif value_ast["type"] == "unary":
        operand = format_value(value_ast["operand"])
        op = value_ast["operator"]

        if op == "-":
            return f"Math.neg ({operand})"
        return f"({op}{operand})"

    return "-- unsupported expression"


def generate_class_module(module_name, classes):
    elements = []
    for cls in classes:
        fields = []
        for f in cls["elements"]:
            field_type = hydra_type(f["field_type"])
            fields.append(f'  "{f["name"]}" Types.>: {field_type}')
        fields_block = ",\n".join(fields)
        fields_block_ = indent(fields_block, 3)
        el = f'  def "{cls["name"]}" $ \n      A.doc "This type generated from Java" $ \n      Types.record [\n{fields_block_}\n  ]'
        elements.append(el)
    # БЛОК С ЭЛЕМЕНТАМИ
    elements_block = ",\n".join(elements) # неймспейс берём по имени модуля, имя модуля - по имени класса/интерфейса, остальное фиксированное
    return f'''{module_name}Module :: Module
{module_name}Module = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just "{module_name} class generated code"
  where
    ns = Namespace "hydra.{module_name.lower()}" 
    def = datatype ns
    core = typeref $ moduleNamespace hydraCoreModule
    testing = typeref ns

    elements = [
{indent(elements_block, 2)}
    ]
'''

def generate_interface_module(module_name, iface):
    elements = []
    blocks = []
    for el in iface["elements"]:
        if el["type"] == "const":
            field_type = hydra_type(el["field_type"])
            field_type_TElement = hydra_type_TElement(el["field_type"])
            name = el["name"]
            #print("ОБРАБОТКА ПОЛЯ ", name)
            #print(el)
            if "value_ast" in el:
                val_expr = format_value(el["value_ast"])
                #print("TRUE", el["value_ast"])
                # пишем Base.тип, если возвращается простое значение
                if el["value_ast"]["type"] == "literal":
                    val_expr = f'Base.{field_type.split(".")[-1]} {val_expr}'
            else: # если в составе есть функции, возвращается TTerm и Base.тип не нужно
                val_expr = f'{format_value(el["value"])}'

            block = f'''{name}Def :: TElement {field_type_TElement}
{name}Def = definitionInModule {module_name}Module "{name}" $
  {val_expr}'''
        elif el["type"] == "function":
            arg_type = hydra_type_TElement(el["parameter_type"])
            ret_type = hydra_type_TElement(el["return_type"])
            body = format_value(el["return_statement"])
            block = f'''{el["name"]}Def :: TElement ({arg_type} -> {ret_type})
{el["name"]}Def = definitionInModule {module_name}Module "{el["name"]}" $
  Base.lambda "{el["parameter_name"]}" $
    {body}'''
        else:
            continue
        elements.append(f'el {el["name"]}Def')
        blocks.append(block)

    elements_block = ",\n".join(elements)
    elements_block_ = indent(elements_block, 1)
    blocks_code = "\n\n".join(blocks)

    header = f'''{module_name}Module :: Module
{module_name}Module = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just "{module_name} interface generated code"
  where
    ns = Namespace "hydra.{module_name.lower()}"
    def = datatype ns
    core = typeref $ moduleNamespace hydraCoreModule
    testing = typeref ns

    elements = [
{indent(elements_block_, 2)}
      ] 
    
{blocks_code}
'''
    return header

def generate(data):
    #print(data)
    output = []
    for unit in data:
        for item in unit:
            #print("ITEM", item)
            if item["type"] == "class":
                output.append(generate_class_module(item["name"], [item]))
            elif item["type"] == "interface":
                output.append(generate_interface_module(item["name"], item))
    return "\n\n".join(output)