#TODO: проверки типов? для пользовательских функций

imports = '''{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.PersonType where

import           Hydra.Dsl.Annotations as A
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms       as Terms
import           Hydra.Dsl.Types       as Types
import           Hydra.Sources.Tier0.Core
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Maybe            as Y
import           Hydra.Sources.Tier1.Mantle
import           Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Module as Module
import qualified Hydra.Dsl.Lib.Logic as Logic
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Lib.Chars as Chars
'''

method_map = {
    "toUpperCase": {"name": "Strings.toUpper", "args_count": 0, "caller_type": "String", "arg_types": [],
                    "return_type": "String"},
    "toLowerCase": {"name": "Strings.toLower", "args_count": 0, "caller_type": "String", "arg_types": [],
                    "return_type": "String"},
    "equals": {"name": "Equality.equalString", "args_count": 1, "caller_type": "String",
               "arg_types": ["String"], "return_type": "Bool"},
    "isEmpty": {"name": "Strings.isEmpty", "args_count": 0, "caller_type": "String", "arg_types": [],
                "return_type": "Bool"},
    "length": {"name": "Strings.length", "args_count": 0, "caller_type": "String", "arg_types": [],
               "return_type": "Int"}
}

# здесь статические - те, которые нельзя вызвать через объект
function_map = {
    "StaticisLowerCase": {"name": "Chars.isLower", "args_count": 1, "arg_types": ["Int"],
                          "return_type": "Bool"},
    "StaticisUpperCase": {"name": "Chars.isUpper", "args_count": 1, "arg_types": ["Int"],
                          "return_type": "Bool"},
    "StatictoUpperCase": {"name": "Chars.toUpper", "args_count": 1, "arg_types": ["Int"],
                          "return_type": "Int"},
    "StatictoLowerCase": {"name": "Chars.toLower", "args_count": 1, "arg_types": ["Int"],
                          "return_type": "Int"},
    "StaticfloorMod": {"name": "Math.mod", "args_count": 2, "arg_types": ["Int", "Int"], "return_type": "Int"}
}

op_map = {
    "+": {"name": "Math.add", "return_type": "Int"},
    "-": {"name": "Math.sub", "return_type": "Int"},
    "*": {"name": "Math.mul", "return_type": "Int"},
    "/": {"name": "Math.div", "return_type": "Int"},
    "%": {"name": "Math.rem", "return_type": "Int"},
    "==": {"name": "Equality.equalInt32", "return_type": "Bool"},
    "!=": {"name": "Equality.notEqualInt32", "return_type": "Bool"},
    ">": {"name": "Equality.gtInt32", "return_type": "Bool"},
    ">=": {"name": "Equality.gteInt32", "return_type": "Bool"},
    "<": {"name": "Equality.ltInt32", "return_type": "Bool"},
    "<=": {"name": "Equality.lteInt32", "return_type": "Bool"},
}


def indent(text, level): # отступы
    return "\n".join("  " * level + line for line in text.splitlines())


#объединенная логика двух функций
def hydra_type_common(java_type, mode):
    base_map = {
        "int": ("Types.int32", "Int"),
        "Integer": ("Types.int32", "Int"),
        "short": ("Types.int16", "Int"),
        "long": ("Types.int64", "Int"),
        "boolean": ("Types.boolean", "Bool"),
        "Boolean": ("Types.boolean", "Bool"),
        "String": ("Types.string", "String")
    }
    if isinstance(java_type, dict) and java_type.get("type") == "List":
        inner = hydra_type_common(java_type["of"], mode)
        return f"Types.list {inner}" if mode == "full" else f"[{inner}]"
    key = java_type if isinstance(java_type, str) else java_type.get("type")
    return base_map.get(key, ("-- unsupported type", "-- unsupported type"))[0 if mode == "full" else 1]

# заменяем старые функции:
def hydra_type(java_type): return hydra_type_common(java_type, mode="full")
def hydra_type_TElement(java_type): return hydra_type_common(java_type, mode="short")


def infer_type(ast):
    if ast["type"] == "literal":
        value_type = ast.get("value_type")
        return "String" if value_type in ("string", "String") else "Int"
    elif ast["type"] == "variable":
        return "Unknown"  # неизвестный тип переменной
    elif ast["type"] == "binary":
        op = ast["operator"]
        if op in op_map:
            left_type = infer_type(ast["left"])
            right_type = infer_type(ast["right"])
            if left_type == right_type:
                return op_map[op]["return_type"]
        else:
            return "-- unsupported binary operator"

    elif ast["type"] == "method_call":
        method_info = method_map.get(ast["method"])
        return method_info.get("return_type", "Unknown") if method_info else "Unknown"

    elif ast["type"] == "function_call":
        func = ast["name"]
        args = ast["arguments"]

        # обработка asList раньше всех проверок
        if func == "asList":
            if not args:
                raise TypeError("asList требует хотя бы один аргумент")

            types = [infer_type(arg) for arg in args]
            first = types[0]

            if not all(t == first for t in types):
                raise TypeError(f"asList: все элементы списка должны быть одного типа, но получены: {types}")

            return f"[{first}]"

        if func not in function_map:
            raise ValueError(f"Unsupported function: {func}")

        spec = function_map[func]

        if spec["args_count"] != -1 and len(args) != spec["args_count"]:
            raise TypeError(f"{func} ожидает {spec['args_count']} аргументов, получено {len(args)}")

        for i, (arg, expected) in enumerate(zip(args, spec["arg_types"])):
            arg_type = infer_type(arg)
            if arg_type != expected:
                raise TypeError(f"{func}: аргумент #{i + 1} должен быть {expected}, но {arg_type}")

        return spec["return_type"]


    elif ast["type"] == "unary":
        return infer_type(ast["operand"])
    elif ast["type"] == "ternary":
        then_type = infer_type(ast["then"])
        else_type = infer_type(ast["else"])
        return then_type if then_type == else_type else "Unknown"

    return "Unknown"


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


    elif value_ast["type"] == "method_call":
        caller = format_value(value_ast["caller"])
        method = value_ast["method"]
        args = [format_value(arg) for arg in value_ast["arguments"]]
        if method in method_map:
            info = method_map[method]
            if len(args) != info["args_count"]:
                raise ValueError(
                    f"Неверное кол-во аргументов {method}, ожидалось {info['args_count']}, получено {len(args)}")

            # типы аргументов
            for i, (arg_ast, expected_type) in enumerate(zip(value_ast["arguments"], info.get("arg_types", []))):
                actual_type = infer_type(arg_ast)
                if actual_type != expected_type:
                    raise TypeError(f"Аргумент {i} функции {method} имеет тип {actual_type}, ожидался {expected_type}")

            # проверка caller - вызыв. функции/объект
            expected_caller_type = info.get("caller_type")
            actual_caller_type = infer_type(value_ast["caller"])
            if expected_caller_type and actual_caller_type != expected_caller_type:
                raise TypeError(
                    f"Метод {method} ожидает вызов на типе {expected_caller_type}, получен {actual_caller_type}")
            mapped = info["name"]
            args_wrapped = " ".join(f"({arg})" for arg in [caller] + args)
            return f"{mapped} {args_wrapped}"

        else:
            return f"-- unsupported method: {method}"

    #т.к. функция статическая, её не могут вызывать в цепочке
    elif value_ast["type"] == "function_call":
        func = value_ast["name"]
        args = [f"({format_value(arg)})" for arg in value_ast["arguments"]]
        info = function_map.get(func)

        if func == "asList":
            return f"Base.list [{', '.join(args)}]"
        elif info:
            if len(args) != info["args_count"]:
                raise ValueError(
                    f"Неверное кол-во аргументов {func}, ожидалось {info['args_count']}, получено {len(args)}")

            for i, (arg_ast, expected_type) in enumerate(zip(value_ast["arguments"], info.get("arg_types", []))):
                actual_type = infer_type(arg_ast)
                if actual_type != expected_type:
                    raise TypeError(f"Аргумент {i} функции {func} имеет тип {actual_type}, ожидался {expected_type}")

            mapped = info["name"]
            return f"{mapped}{''.join(args)}"
        else:
            return f"-- unsupported func: {func}"


    elif value_ast["type"] == "binary":
        left = format_value(value_ast["left"])
        right = format_value(value_ast["right"])
        op = value_ast["operator"]
        if op in op_map:
            return f'{op_map[op]["name"]} ({left}) ({right})'
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

    elif value_ast["type"] == "ternary":
        cond = format_value(value_ast["condition"])
        then_expr = format_value(value_ast["then"])
        else_expr = format_value(value_ast["else"])
        return f"Logic.ifElse({cond}) ({then_expr}) ({else_expr})"

    return "-- unsupported expression"

def generate_my_module(data):
    module_name = "myModule"
    namespace = None
    elements = []
    blocks = []

    for unit in data:
        for item in unit:
            if item["type"] == "interface" and not namespace:
                namespace = item["name"].lower()

    if not namespace:
        namespace = "default"

    ns_line = f'Namespace "hydra.{namespace}"'

    for unit in data:
        for item in unit:
            if item["type"] == "class":
                for cls in [item]:
                    fields = []
                    for f in cls["elements"]:
                        field_type = hydra_type(f["field_type"])
                        fields.append(f'  "{f["name"]}" Types.>: {field_type}')
                    fields_block = indent(",\n".join(fields), 3)
                    el = f'  def "{cls["name"]}" $ \n      A.doc "This type generated from Java" $ \n      Types.record [\n{fields_block}\n  ]'
                    elements.append(el)

            elif item["type"] == "interface":
                for el in item["elements"]:
                    if el["type"] == "const":
                        field_type = hydra_type(el["field_type"])
                        field_type_TElement = hydra_type_TElement(el["field_type"])
                        name = el["name"]

                        if "value_ast" in el:
                            val_expr = format_value(el["value_ast"])
                            real_type = infer_type(el["value_ast"])
                            if el["value_ast"]["type"] == "literal":
                                val_expr = f'Base.{field_type.split(".")[-1]} {val_expr}'
                        else:
                            val_expr = f'{format_value(el["value"])}'
                            #print(val_expr)
                            real_type = infer_type(el["value"])
                            #print(real_type)


                        if real_type != field_type_TElement:
                            raise TypeError(
                                f'TElement тип "{field_type_TElement}" не соответствует типу значения "{real_type}" для "{name}"')

                        block = f'''{name}Def :: TElement {field_type_TElement}
{name}Def = definitionInModule {module_name} "{name}" $
  {val_expr}'''


                    elif el["type"] == "function":
                        arg_type = hydra_type_TElement(el["parameter_type"])
                        ret_type = hydra_type_TElement(el["return_type"])
                        body = format_value(el["return_statement"])
                        block = f'''{el["name"]}Def :: TElement ({arg_type} -> {ret_type})
{el["name"]}Def = definitionInModule {module_name} "{el["name"]}" $
  Base.lambda "{el["parameter_name"]}" $
    {body}'''

                    else:
                        continue

                    elements.append(f'el {el["name"]}Def')
                    blocks.append(block)

    elements_block = indent(",\n".join(elements), 2)
    blocks_code = "\n\n".join(blocks)

    return f'''{imports}
{module_name} :: Module
{module_name} = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just "{module_name} generated code"
  where
    ns = {ns_line}
    def = datatype ns
    core = typeref $ moduleNamespace hydraCoreModule
    testing = typeref ns

    elements = [
{elements_block}
    ]

{blocks_code}
'''

def generate(data):
    return generate_my_module(data)