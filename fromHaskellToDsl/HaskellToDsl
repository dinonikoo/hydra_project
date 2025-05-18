import re
import traceback

from lark import Lark, Transformer

# ======== GRAMMAR =========
haskell_grammar = r"""
?start: toplevel_declarations

toplevel_declarations: (function_decl (_NEWLINE | WS_INLINE)* function_def)+

function_decl: NAME "::" type
function_def: NAME var_list? "=" expression
var_list: NAME+



type: type_atom "->" type     -> function_type
    | type_atom               -> base_type

?type_atom: list_type
          | NAME

list_type: "[" NAME "]"       -> list_type

?expression: if_expr

?if_expr: "if" expression "then" expression "else" expression -> if_else
        | or_expr

?or_expr: or_expr "||" and_expr         -> or_
        | and_expr

?and_expr: and_expr "&&" not_expr       -> and_
         | not_expr

?not_expr: "not" not_expr               -> not_
         | comparison

?comparison: arithmetic "==" arithmetic   -> eq
           | arithmetic "/=" arithmetic   -> noteq
           | arithmetic ">" arithmetic    -> gt
           | arithmetic "<" arithmetic    -> lt
           | arithmetic ">=" arithmetic   -> gte
           | arithmetic "<=" arithmetic   -> lte
           | arithmetic

?arithmetic: arithmetic "++" term -> concat2
           |arithmetic "+" term   -> add
           | arithmetic "-" term   -> sub
           | term

?term: term "!!" factor             -> at
     | factor ":" term             -> cons
     | term "*" factor             -> mul
     | term "/" factor             -> div
     | term "`mod`" factor         -> mod
     | term "`rem`" factor         -> rem     
     | factor


       
?factor: ESCAPED_STRING     -> string
       | "True"             -> true
       | "False"            -> false
       | "negate" factor    -> neg
       | NUMBER             -> number
       | list_literal       -> list_literal
       | "map" NAME factor  -> map_function
       | NAME factor+       -> function_call
       | NAME               -> variable
       | "(" expression ")"

list_literal: "[" [expression ("," expression)*] "]"


%import common.CNAME -> NAME
%import common.NUMBER
%import common.ESCAPED_STRING
%import common.WS_INLINE
%import common.NEWLINE
%ignore WS_INLINE
_NEWLINE: /(\r?\n)+/
%ignore _NEWLINE
COMMENT: /--.*/
%ignore COMMENT
ML_COMMENT: /\{\-[^(\-\})]*\-\}/
%ignore ML_COMMENT
IMPORT_LINE: "import" /[^\n]*/    
%ignore IMPORT_LINE               
"""

# ======== PARSER =========
haskell_parser = Lark(haskell_grammar, start="start")
typed_builtin_functions = {
    "null": "String",
    "length": "String",
    "toUpper": "String",
    "toLower": "String",
    "toList": "String",
    "fromList": "[Int]",
    "concat": "[String]",
    "isUpper": "Int",
    "isLower": "Int",
}


def infer_return_type(expr: str) -> str:
    expr_stripped = expr.strip()
    expr_nospace = re.sub(r"\s+", "", expr_stripped)

    print(f"📥 expr_nospace: {expr_nospace}")

    # Используем re.search вместо re.match
    match = re.search(r"([A-Za-z0-9_\.]+)[\(\[]", expr_nospace)
    print(f"🔍 match: {match}")

    if not match:
        print("⚠️Соотвествия не найдуткет!")
        return "Unknown"

    func = match.group(1)
    print(f"[DEBUG] Обнаружена функция : {func}")

    if func == "Strings.cat2":
        if "Base.int32" in expr:
            return "[Int]"
        if "Base.string" in expr:
            return "[String]"
        return "[a]"

    if func == "Lists.at":
        return "a"
    if func == "Lists.head":
        return "a"
    if func == "Lists.last":
        return "a"
    if func == "Lists.length":
        return "Int"
    if func == "Strings.length":
        return "Int"
    if func.startswith("Logic."):
        return "Bool"
    if func.startswith("Equality."):
        return "Bool"
    if func.startswith("Math.") or func == "Base.int32":
        return "Int"
    if func.startswith("Strings.isEmpty"):
        return "Bool"
    if (
        func.startswith("Strings.cat")
        or func.startswith("Strings.toUpper")
        or func.startswith("Strings.toLower")
    ):
        return "String"
    if func == "Strings.splitOn":
        return "[String]"
    if func == "Strings.toList":
        return "[Int]"
    if func == "Strings.fromList":
        return "String"
    if func == "Base.string":
        return "String"
    if func == "Base.list":
        if "Base.int32" in expr:
            return "[Int]"
        if "Base.string" in expr:
            return "[String]"
        return "[Unknown]"
    if func.startswith("Lists."):
        if "null" in func:
            return "Bool"
        return "[a]"

    return "Unknown"


class HydraTransformer(Transformer):
    def __init__(self, module_name="myModule"):
        self.definitions = []
        self.module_name = module_name
        self.current_arg_types = []
        self.arg_name_to_type = {}
        self.arg_names = []

    def toplevel_declarations(self, items):
        element_list = ",\n      ".join(
            [f"el {item[0]}Def" for item in self.definitions]
        )
        function_defs = "\n\n".join(item[1] for item in self.definitions)
        return f"""elements = [
      {element_list}]

{function_defs}"""

    def function_decl(self, items):
        name = str(items[0])
        type_str = str(items[1])
        self.arg_names = []
        self.arg_types = []
        self.arg_name_to_type = {}
        self.current_arg_types = []

        self.arg_types = type_str.split("->")[:-1]
        if "Char" in type_str:
            raise ValueError(f"❌ Тип 'Char' не поддерживается: {name} :: {type_str}")
        self.last_decl = (name, type_str)
        return "ok"

    def function_def(self, items):
        name = str(items[0])
        if len(items) == 3 and hasattr(items[1], "children"):
            args = [str(arg) for arg in items[1].children]
            expr = items[2]
        else:
            args = []
            expr = items[1]

        if not hasattr(self, "last_decl") or self.last_decl[0] != name:
            raise ValueError(f"Ошибка: тип функции '{name}' не был объявлен")

        type_str = self.last_decl[1]
        type_parts = [t.strip() for t in type_str.split("->")]

        # ✅ Сначала заполняем текущие типы аргументов
        self.arg_names = args
        normalized_arg_types = [t.replace("Integer", "Int") for t in self.arg_types]
        self.arg_name_to_type = {
            k.strip(): v.strip() for k, v in zip(self.arg_names, normalized_arg_types)
        }

        self.current_arg_types = [t.replace("Integer", "Int") for t in type_parts[:-1]]

        print(f"[DEBUG] function_def '{name}':")
        print(f"  args = {args}")
        print(f"  type_parts = {type_parts}")
        print(f"  arg_name_to_type = {self.arg_name_to_type}")

        expected_arg_count = len(type_parts) - 1
        actual_arg_count = len(args)

        if actual_arg_count != expected_arg_count:
            raise ValueError(
                f"Несоответствие типов: функция '{name}' принимает {actual_arg_count} аргументов, но тип говорит о {expected_arg_count}"
            )

        expected_return_type = type_parts[-1].replace("Integer", "Int")
        if isinstance(expr, str) and expr.startswith("Base.list [Base.list"):
            expr = (
                expr.replace("Base.list [Base.list", "  Base.list", 1).rstrip("]") + "]"
            )

        dsl_expr = expr

        def guess_type(expr: str) -> str:
            expr = expr.strip()

            if expr[0] == "(" and expr[-1] != ")":
                expr = expr + ")"
            print("1111111", expr)

            if expr.startswith("(Base.string "):
                return "String"
            if expr.startswith("(Base.int32 "):
                return "Int"
            print(expr.startswith("Base.var"))

            if expr.startswith("(Base.var") or expr.startswith("Base.var"):
                var_match = re.search(r'\(Base\.var\s+"([^"]+)"\)', expr)
                if not var_match:
                    # Пытаемся найти без скобок
                    var_match = re.search(r'Base\.var\s+"([^"]+)"', expr)

                print("gess", var_match)
                if var_match:
                    var = var_match.group(1)
                    typ = self.arg_name_to_type.get(var, "Unknown").strip()
                    print(f"[DEBUG] 📌 var '{var}' → тип '{typ}'")
                    if typ in ["a", "[a]"]:
                        return typ

                    return typ

            if expr.startswith("Chars.toUpper") or expr.startswith("Chars.toLower"):
                return "Int"
            if expr.startswith("=Strings.toUpper") or expr.startswith(
                "=Strings.toLower"
            ):
                return "String"
            if expr.startswith("Strings.") or "Base.string" in expr:
                return "String"
            return "Unknown"

        def replace_eq_exprs(match):
            lhs = match.group(1)
            rhs = match.group(2)
            type_l = guess_type(lhs)
            type_r = guess_type(rhs)

            if type_l != type_r:
                raise ValueError(
                    f"❌ Несовместимые типы в выражении равенства: {lhs} ({type_l}) == {rhs} ({type_r})"
                )

            if type_l == "String":
                return f"Equality.equalString ({lhs}) ({rhs})"
            else:
                return f"Equality.equalInt32 ({lhs}) ({rhs})"

        dsl_expr = re.sub(
            r"Equality\.equalInt32\s*\((.*?)\)\s*\((.*?)\)",
            replace_eq_exprs,
            dsl_expr,
        )

        # Функция Null
        null_calls = re.findall(
            r"Lists\.null\s*\(\(Base\.var\s+\"([a-zA-Z_][a-zA-Z0-9_]*)\"\)\)", dsl_expr
        )
        for var in null_calls:
            typ = self.arg_name_to_type.get(var, "").strip()
            if typ == "String":
                old = f'Lists.null ((Base.var "{var}"))'
                new = f'Strings.isEmpty ((Base.var "{var}"))'
                print(
                    f"[DEBUG] ⏩ Replacing null for string var '{var}': {old} -> {new}"
                )
                dsl_expr = dsl_expr.replace(old, new)

        length_calls = re.findall(
            r"Strings\.length\s*\(\(Base\.var\s+\"([a-zA-Z_][a-zA-Z0-9_]*)\"\)\)",
            dsl_expr,
        )
        for var in length_calls:
            typ = self.arg_name_to_type.get(var, "").strip()
            if typ != "String":
                old = f'Strings.length ((Base.var "{var}"))'
                new = f'Lists.length ((Base.var "{var}"))'
                print(
                    f"[DEBUG] 🔄 Replacing length for non-string var '{var}': {old} -> {new}"
                )
                dsl_expr = dsl_expr.replace(old, new)

        # Функция для ++
        cat2_calls = re.findall(
            r"Strings\.cat2\s*\(\((.*?)\)\)\s*\(\((.*?)\)\)", dsl_expr
        )

        for lhs, rhs in cat2_calls:
            lhs_type = guess_type(lhs)
            rhs_type = guess_type(rhs)
            print(lhs)

            if lhs_type == rhs_type == "String":
                continue  # всё верно
            elif lhs_type.startswith("[") and rhs_type.startswith("["):
                old = f"Strings.cat2 (({lhs})) (({rhs}))"
                new = f"Lists.concat2 (({lhs})) (({rhs}))"
                print(f"[DEBUG] 🔄 Replacing cat2 for list: {old} → {new}")
                dsl_expr = dsl_expr.replace(old, new)
            else:
                raise ValueError(
                    f"❌ Несовместимые типы в '++': {lhs_type} и {rhs_type} — ожидается либо String+String, либо [a]+[a]"
                )

        inferred = infer_return_type(dsl_expr)

        # Сравнение с ожидаемым типом
        print(inferred, expected_return_type, dsl_expr)
        if inferred == "a":
            # Найдём переменную, переданную в head или at и т.п.
            match_var = re.search(r"\(Base\.var\s+\"([^\"]+)\"\)", dsl_expr)
            if match_var:
                var_name = match_var.group(1)
                arg_typ = self.arg_name_to_type.get(var_name, "")
                print(
                    f"[DEBUG] 🧠 Пытаемся уточнить 'a' → {arg_typ} по переменной '{var_name}'"
                )
                if arg_typ.startswith("[") and arg_typ.endswith("]"):
                    # если [Int] → значит head возвращает Int
                    element_type = arg_typ[1:-1]
                    inferred = element_type

        elif expected_return_type == "[a]":
            if not (inferred.startswith("[") and inferred.endswith("]")):
                raise ValueError(
                    f"❌ Тип '[a]' должен быть списком: получен '{inferred}'"
                )
        elif inferred != expected_return_type:
            # [a] считается совместимым с любым конкретным списочным типом
            if (
                inferred == "[a]"
                and expected_return_type.startswith("[")
                and expected_return_type != "[a]"
            ):
                print(
                    f"[INFO] ✅ '[a]' интерпретирован как допустимый тип для {expected_return_type}"
                )
                pass
            else:
                raise ValueError(
                    f"❌ Несоответствие возвращаемого типа в функции '{name}': из сигнатуры ожидается '{expected_return_type}', но получается '{inferred}'"
                )

        for arg in reversed(args):
            dsl_expr = f'  Base.lambda "{arg}" $ {dsl_expr}'

        arg_types = " -> ".join(t.replace("Integer", "Int") for t in type_parts)

        function_def = (
            f"{name}Def :: TElement ({arg_types})\n"
            f'{name}Def = definitionInModule {self.module_name} "{name}" $\n'
            f"{dsl_expr}"
        )

        self.definitions.append((name, function_def))
        return ""

    def function_type(self, items):
        return f"{items[0]} -> {items[1]}"

    def base_type(self, items):
        return str(items[0])

    def list_type(self, items):
        return f"[{items[0]}]"

    def _check_no_unsupported(self, *args):
        for arg in args:
            if "Base.char" in str(arg):
                raise ValueError("❌ Char не поддерживается")

    def add(self, items):
        self._check_no_unsupported(*items)
        return f"Math.add ({items[0]}) ({items[1]})"

    def sub(self, items):
        self._check_no_unsupported(*items)
        return f"Math.sub ({items[0]}) ({items[1]})"

    def mul(self, items):
        self._check_no_unsupported(*items)
        return f"Math.mul ({items[0]}) ({items[1]})"

    def div(self, items):
        self._check_no_unsupported(*items)
        return f"Math.div ({items[0]}) ({items[1]})"

    def mod(self, items):
        self._check_no_unsupported(*items)
        return f"Math.mod ({items[0]}) ({items[1]})"

    def rem(self, items):
        self._check_no_unsupported(*items)
        return f"Math.rem ({items[0]}) ({items[1]})"

    def neg(self, items):
        self._check_no_unsupported(*items)
        return f"Math.neg ({items[0]})"

    def eq(self, items):
        self._check_no_unsupported(*items)
        return f"Equality.equalInt32 ({items[0]}) ({items[1]})"

    def noteq(self, items):
        self._check_no_unsupported(*items)
        equality_expr = self.eq(items)
        return f"Logic.not ({equality_expr})"

    def lt(self, items):
        self._check_no_unsupported(*items)
        return f"Equality.ltInt32 ({items[0]}) ({items[1]})"

    def gt(self, items):
        self._check_no_unsupported(*items)
        return f"Equality.gtInt32 ({items[0]}) ({items[1]})"

    def lte(self, items):
        self._check_no_unsupported(*items)
        return f"Equality.lteInt32 ({items[0]}) ({items[1]})"

    def gte(self, items):
        self._check_no_unsupported(*items)
        return f"Equality.gteInt32 ({items[0]}) ({items[1]})"

    def number(self, items):
        return f"(Base.int32 {items[0]})"

    def string(self, items):
        return f"(Base.string {items[0]})"

    def true(self, _):
        return "Base.true"

    def false(self, _):
        return "Base.false"

    def variable(self, items):
        return f'(Base.var "{items[0]}")'

    def and_(self, items):
        return f"Logic.and ({items[0]}) ({items[1]})"

    def or_(self, items):
        return f"Logic.or ({items[0]}) ({items[1]})"

    def not_(self, items):
        return f"Logic.not ({items[0]})"

    def if_else(self, items):
        return f"Logic.ifElse ({items[0]}) ({items[1]}) ({items[2]})"

    def concat2(self, items):
        return f"Strings.cat2 ({items[0]}) ({items[1]})"

    def at(self, items):
        return f"Lists.at ({items[1]}) ({items[0]})"  # i, xs → xs !! i

    def cons(self, items):
        return f"Lists.cons ({items[0]}) ({items[1]})"  # x : xs

    def map_function(self, items):
        func = str(items[0])
        arg = items[1]

        if func == "toUpper":
            return f"Strings.toUpper ({arg})"
        elif func == "toLower":
            return f"Strings.toLower ({arg})"
        elif func == "ord":
            return f"Strings.toList ({arg})"
        elif func == "chr":
            return f"Strings.fromList ({arg})"
        else:
            raise ValueError(
                f"❌ map с функцией '{func}' не поддерживается. Используйте map только с toUpper/toLower."
            )

    def function_call(self, items):
        func = str(items[0])
        args = items[1:]

        if func == "isUpper":
            return f"Chars.isUpper ({args[0]})"
        if func == "isLower":
            return f"Chars.isLower ({args[0]})"
        if func == "toUpper":
            return f"Chars.toUpper ({args[0]})"
        if func == "toLower":
            return f"Chars.toLower ({args[0]})"

        print(
            f"123::::[DEBUG] function_call: func='{func}' args={args}, {self.arg_name_to_type}"
        )

        if func == "null":
            varname = ""
            if hasattr(args[0], "type") and args[0].type == "NAME":
                varname = str(args[0])
            elif isinstance(args[0], str) and args[0].startswith('(Base.var "'):
                varname = args[0].split('"')[1]

            print(f"[DEBUG] function_call: func='null' varname='{varname}' args={args}")
            print(f"[DEBUG] available types: {self.arg_name_to_type}")

            if varname and varname in self.arg_name_to_type:
                arg_type = self.arg_name_to_type[varname].strip()
                if arg_type == "String":
                    return f"Strings.isEmpty ({args[0]})"
                else:
                    return f"Lists.null ({args[0]})"
            else:

                return f"Lists.null ({args[0]})"

        if func == "length":
            return f"Strings.length ({args[0]})"

        if func == "fromList":
            return f"Strings.fromList ({args[0]})"
        if func == "concat":
            return f"Strings.cat ({args[0]})"

        if func == "zipWith":
            return f"Lists.zipWith ({args[0]}) ({args[1]}) ({args[2]})"
        if func == "concat2":
            return f"Lists.concat2 ({args[0]}) ({args[1]})"
        if func == "filter":
            return f"Lists.filter ({args[0]}) ({args[1]})"

        if func == "head":
            return f"Lists.head ({args[0]})"
        if func == "tail":
            return f"Lists.tail ({args[0]})"
        if func == "last":
            return f"Lists.last ({args[0]})"
        if func == "length":
            return f"Lists.length ({args[0]})"

        if func == "reverse":
            return f"Lists.reverse ({args[0]})"

        raise ValueError(f"❌ Неизвестная функция: {func}")

    def list_literal(self, items):
        if (
            len(items) == 1
            and isinstance(items[0], str)
            and items[0].startswith("  Base.list")
        ):
            print("💡 Detected already wrapped Base.list, returning as-is")
            return items[0]

        elements = [str(item) for item in items]

        for e in elements:
            print("444", e)

        if all("Base.int32" in e for e in elements):
            self.last_list_type = "[Int]"
            print(f"12345 Base.list [{', '.join(elements)}]")
            return f"  Base.list [{', '.join(elements)}]"

        elif all("Base.string" in e for e in elements):
            self.last_list_type = "[String]"
            print(f"12345 Base.list [{', '.join(elements)}]")
            return f"  Base.list [{', '.join(elements)}]"

        raise ValueError("❌ Поддерживаются только списки Int или String")


# ======== CONVERTER FUNCTION =========
def process_haskell_to_hydra(
    haskell_code, file_name="MyMath", save_name="hydra.test", module_name="myModule"
):

    tree = haskell_parser.parse(haskell_code)
    transformer = HydraTransformer(module_name=module_name)
    elements_block = transformer.transform(tree)
    uses_char_funcs = any(
        func in haskell_code
        for func in ["toUpper", "toLower", "isUpper", "isLower", "ord", "chr"]
    )
    has_import = "import Data.Char" in haskell_code

    if uses_char_funcs and not has_import:
        raise ValueError(
            "❌ Используются функции из Data.Char toUpper/toLower/isUpper/isLower, но отсутствует 'import Data.Char'"
        )

    imports = "\n".join(
        [
            "import           Hydra.Dsl.Annotations\n"
            "import           Hydra.Dsl.Bootstrap\n"
            "import qualified Hydra.Dsl.Terms       as Terms\n"
            "import           Hydra.Dsl.Types       as Types\n"
            "import           Hydra.Sources.Tier0.Core\n"
            "import qualified Data.List             as L\n"
            "import qualified Data.Map              as M\n"
            "import qualified Data.Set              as S\n"
            "import qualified Data.Maybe            as Y\n"
            "import           Hydra.Sources.Tier1.Mantle\n"
            "import           Hydra.Dsl.Base as Base\n"
            "import qualified Hydra.Dsl.Lib.Math as Math\n"
            "import qualified Hydra.Dsl.Module as Module\n"
            "import qualified Hydra.Dsl.Lib.Logic as Logic\n"
            "import qualified Hydra.Dsl.Lib.Equality as Equality\n"
            "import qualified Hydra.Dsl.Lib.Strings as Strings\n"
            "import qualified Hydra.Dsl.Lib.Lists as Lists\n"
        ]
    )

    core = "hydraCoreModule"

    return f"""-- This file was auto-generated from Haskell

module Hydra.Sources.{file_name} where

{imports}

{module_name} :: Module
{module_name} = Module (Namespace "{save_name.lower()}") elements [{core}] [{core}] (Just "Test functions")
  where
    {elements_block}
"""


# ======== I/O =========
if __name__ == "__main__":
    INPUT_PATH = "C:/2 курс/курсовая/test1.hs"
    OUTPUT_PATH = "C:/2 курс/курсовая/gen-test.hs"
    FILE_NAME = "MyMath"
    SAVED_NAME = "hydra.test"
    MODULE_NAME = "myModuleTest"
    EXCEPTION_FILE = "exceptions.txt"  # Файл для записи исключений

    try:
        with open(INPUT_PATH, "r", encoding="utf-8") as f:
            haskell_code = f.read()

        hydra_code = process_haskell_to_hydra(
            haskell_code,
            file_name=FILE_NAME,
            save_name=SAVED_NAME,
            module_name=MODULE_NAME,
        )

        with open(OUTPUT_PATH, "w", encoding="utf-8") as f:
            f.write(hydra_code)

        print(f"✅ DSL-модуль записан в: {OUTPUT_PATH}")
    except Exception as e:
        print(f"\n❌ Ошибка при генерации DSL-модуля:\n{e}")

        # Записываем исключение в файл
        with open(EXCEPTION_FILE, "a", encoding="utf-8") as f:
            f.write(f"\n❌ Ошибка при генерации DSL-модуля:\n{e}")

        exit(1)


"""
-Игнорирование комментариев (--,  {- - }  ) пока не поддерживает встроенные комментарии (типа один в одном (не знаю, насколько это нужно))
-Проверка количества входных параметров -> исключ Несоответствие типов: функция 'stringCatList' принимает 2 аргументов, но тип говорит о 1
-Проверка имени функции -> исключ
-Проверка выходного параметра (если человек пишет, что выходит Bool, по выражнеию получается Int)(проблема с функциями типа addMe :: String ->String  addMe x = x, хоть они и бесполезны)
--Добавить сравнение equal.string
--В гидре чаров нет - задание через код буквы!
--неравно это not equal =>  /=
--Контроль того, что в char есть импорт + импорт для Spliton
--Добавлена поддержка импортов
--2 аргумента для SplitOn 
SPLITON Не поддерживается 

функции без введённых параметров 

cat + 

поддержка констант

чёткое указание функции 
--поддержка литералов списков

--добавлены несовместимые типы в равенстве 

--пока нет вложенных массивов
--добавлен вывод ошибок к файл
--нужен конкретный тип массва
--проверка параметризации для массивов добавлена


--TODO: вложенные листы? фильтр
--вставить классы

--добавить сохранение имён функций, потоум что 2 с одним названием нельзя 

--файл очищать добавить
"""
