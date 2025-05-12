from lark import Lark, Transformer

# ======== GRAMMAR =========
haskell_grammar = r"""
?start: toplevel_declarations

toplevel_declarations: (function_decl (_NEWLINE | WS_INLINE)* function_def)+

function_decl: NAME "::" type
function_def: NAME var_list "=" expression

var_list: NAME+

type: NAME "->" type     -> function_type
    | NAME               -> base_type

?expression: comparison

?comparison: arithmetic "==" arithmetic   -> eq
           | arithmetic ">" arithmetic    -> gt
           | arithmetic "<" arithmetic    -> lt
           | arithmetic ">=" arithmetic   -> gte
           | arithmetic "<=" arithmetic   -> lte
           | arithmetic

?arithmetic: arithmetic "+" term   -> add
           | arithmetic "-" term   -> sub
           | term

?term: term "*" factor             -> mul
     | term "/" factor             -> div
     | term "`mod`" factor         -> mod
     | term "`rem`" factor         -> rem
     | factor

?factor: ESCAPED_STRING     -> string
       | "True"             -> true
       | "False"            -> false
       | "negate" factor        -> neg
       | NUMBER             -> number
       | NAME               -> variable
       | "(" expression ")"

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
"""

# ======== PARSER =========
haskell_parser = Lark(haskell_grammar, start="start")


def infer_return_type(expr: str) -> str:
    """
    Определяет тип возвращаемого значения выражения."""

    if "Base.string" in expr:
        return "String"  # Строки имеют приоритет

    bool_operators = ["equal", "lt", "gt", "lte", "gte", "Base.true", "Base.false"]
    for op in bool_operators:
        if op in expr:
            return "Bool"

    int_functions = [
        "Math.add",
        "Math.sub",
        "Math.mul",
        "Math.div",
        "Math.mod",
        "Math.rem",
        "Base.int32",
    ]
    for func in int_functions:
        if func in expr:
            return "Int"

    return "Unknown"


class HydraTransformer(Transformer):
    def __init__(self, module_name="myModule"):
        self.definitions = []
        self.module_name = module_name

    def toplevel_declarations(self, items):
        element_list = ",\n      ".join(
            [f"el {item[0]}Def" for item in self.definitions]
        )
        function_defs = "\n\n".join(item[1] for item in self.definitions)
        return f"""elements = [
      {element_list}
    ]

{function_defs}"""

    def function_decl(self, items):
        self.last_decl = (str(items[0]), str(items[1]))
        return "ok"

    def function_def(self, items):
        name = str(items[0])
        args = [str(arg) for arg in items[1].children]
        expr = items[2]

        if not hasattr(self, "last_decl") or self.last_decl[0] != name:
            raise ValueError(f"Ошибка: тип функции '{name}' не был объявлен")

        type_str = self.last_decl[1]
        type_parts = [t.strip() for t in type_str.split("->")]

        expected_arg_count = len(type_parts) - 1
        actual_arg_count = len(args)
        if actual_arg_count != expected_arg_count:
            raise ValueError(
                f"Несоответствие типов: функция '{name}' принимает {actual_arg_count} аргументов, но тип говорит о {expected_arg_count}"
            )

        expected_return_type = type_parts[-1].replace("Integer", "Int")
        inferred = infer_return_type(expr)
        if inferred != expected_return_type:
            raise ValueError(
                f"Несоответствие возвращаемого типа в функции '{name}': из вашей сигнатуры Haskell ожидается '{expected_return_type}', но получается '{inferred}'"
            )

        dsl_expr = expr
        for arg in reversed(args):
            dsl_expr = f'  Base.lambda "{arg}" $ {dsl_expr}'

        arg_types = " -> ".join(t.replace("Integer", "Int") for t in type_parts)

        function_def = f"""{name}Def :: TElement ({arg_types})
    {name}Def = definitionInModule {self.module_name} \"{name}\" $
    {dsl_expr}"""

        self.definitions.append((name, function_def))
        return ""

    def function_type(self, items):
        return f"{items[0]} -> {items[1]}"

    def base_type(self, items):
        return str(items[0])

    def add(self, items):
        return f"Math.add ({items[0]}) ({items[1]})"

    def sub(self, items):
        return f"Math.sub ({items[0]}) ({items[1]})"

    def mul(self, items):
        return f"Math.mul ({items[0]}) ({items[1]})"

    def div(self, items):
        return f"Math.div ({items[0]}) ({items[1]})"

    def mod(self, items):
        return f"Math.mod ({items[0]}) ({items[1]})"

    def rem(self, items):
        return f"Math.rem ({items[0]}) ({items[1]})"

    def neg(self, items):
        return f"Math.neg ({items[0]})"

    def eq(self, items):
        return f"Equality.equalInt32 ({items[0]}) ({items[1]})"

    def lt(self, items):
        return f"Equality.ltInt32 ({items[0]}) ({items[1]})"

    def gt(self, items):
        return f"Equality.gtInt32 ({items[0]}) ({items[1]})"

    def lte(self, items):
        return f"Equality.lteInt32 ({items[0]}) ({items[1]})"

    def gte(self, items):
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


# ======== CONVERTER FUNCTION =========
def process_haskell_to_hydra(
    haskell_code, file_name="MyMath", save_name="hydra.test", module_name="myModule"
):
    tree = haskell_parser.parse(haskell_code)
    transformer = HydraTransformer(module_name=module_name)
    elements_block = transformer.transform(tree)

    imports = "\n".join(
        [
            "import Hydra.Dsl.Base as Base",
            "import Hydra.Dsl.Types as Types",
            "import Hydra.Dsl.Terms as Terms",
            "import qualified Hydra.Dsl.Lib.Math as Math",
            "import qualified Hydra.Dsl.Lib.Equality as Equality",
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
    # === CONFIG ===
    INPUT_PATH = "C:/2 курс/курсовая/test1.hs"
    OUTPUT_PATH = "C:/2 курс/курсовая/gen-test.hs"
    FILE_NAME = "MyMath"
    SAVED_NAME = "hydra.test"
    MODULE_NAME = "myModuleTest"
    # ==============

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
        exit(1)


"""
-Игнорирование комментариев (--,  {- - }  ) пока не поддерживает встроенные комментарии (типа один в одном (не знаю, насколько это нужно))
-Проверка количества входных параметров -> исключ
-Проверка имени функции -> исключ
-Проверка выходного параметра (если человек пишет, что выходит Bool, по выражнеию получается Int)(проблема с функциями типа addMe :: String ->String  addMe x = x, хоть они и бесполезны)
--Добавить сравнение equal.string
"""
