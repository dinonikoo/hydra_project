import ast

# === DSL Mappings ===
dsl_map = {
    'list':  ('Strings.toList',     'String -> [Int]'),
    'len':   ('String.length',      'String -> Int'),
    'upper': ('Strings.toUpper',    'String -> String'),
    'lower': ('Strings.toLower',    'String -> String'),
}

bin_op_map = {
    ast.Mod:      ('Math.mod', 'Int -> Int -> Int'),
    ast.Add:      ('Math.add', 'Int -> Int -> Int'),
    ast.Sub:      ('Math.sub', 'Int -> Int -> Int'),
    ast.Mult:     ('Math.mul', 'Int -> Int -> Int'),
    ast.FloorDiv: ('Math.div', 'Int -> Int -> Int'),
}

compare_map = {
    ast.Lt: ('Equality.ltInt32',    'Int -> Int -> Bool'),
    ast.LtE: ('Equality.lteInt32',    'Int -> Int -> Bool'),
    ast.Gt: ('Equality.gtInt32',    'Int -> Int -> Bool'),
    ast.GtE: ('Equality.gteInt32',    'Int -> Int -> Bool'),
}

bool_op_map = {
    ast.And: 'Logic.and',
    ast.Or:  'Logic.or',
    ast.Not: 'Logic.not',
}

# === Type Inference ===
def infer_sig(node):
    if isinstance(node, ast.Call):
        f = node.func
        if isinstance(f, ast.Attribute) and f.attr in dsl_map:
            return dsl_map[f.attr][1]
        if isinstance(f, ast.Name) and f.id in dsl_map:
            return dsl_map[f.id][1]
    if isinstance(node, ast.BinOp):
        t = bin_op_map.get(type(node.op))
        if t: return t[1].split('->')[-1].strip()
    if isinstance(node, ast.Compare):
        if isinstance(node.ops[0], ast.Eq):
            return 'Bool'
        t = compare_map.get(type(node.ops[0]))
        if t: return t[1]
    if isinstance(node, ast.BoolOp):
        return 'Bool'
    if isinstance(node, ast.UnaryOp):
        if isinstance(node.op, ast.Not):
            return 'Bool'
        if isinstance(node.op, ast.USub):
            return 'Int'
    if isinstance(node, ast.IfExp):
        return infer_sig(node.body) 
    if isinstance(node, ast.Constant):
        v = node.value
        if isinstance(v, bool): return 'Bool'
        if isinstance(v, int): return 'Int'
        if isinstance(v, str): return 'String'
    if isinstance(node, ast.List):
        return '[Int]'
    return 'Int'

# === Expression Translation ===
def to_haskell_expr(node):
    if isinstance(node, ast.Call):
        if isinstance(node.func, ast.Attribute):
            obj = to_haskell_expr(node.func.value)
            func = dsl_map.get(node.func.attr, ('-- unsupported function', ))[0]
            return f"{func} ({obj})"
        elif isinstance(node.func, ast.Name):
            arg = to_haskell_expr(node.args[0])
            func = dsl_map.get(node.func.id, ('-- unsupported function', ))[0]
            return f"{func} ({arg})"

    if isinstance(node, ast.BinOp):
        left = to_haskell_expr(node.left)
        right = to_haskell_expr(node.right)
        op = bin_op_map.get(type(node.op), ('-- unsupported operation', ))[0]
        return f"{op} ({left}) ({right})"

    if isinstance(node, ast.Compare):
        left = to_haskell_expr(node.left)
        right = to_haskell_expr(node.comparators[0])
        left_type = infer_sig(node.left)
        right_type = infer_sig(node.comparators[0])
        if isinstance(node.ops[0], ast.Eq):
            if 'String' in (left_type, right_type):
                return f"Equality.equalString ({left}) ({right})"
            if 'Bool' in (left_type, right_type):
                return f"Equality.equalBool ({left}) ({right})"
            return f"Equality.equalInt32 ({left}) ({right})"
        else:
            op = compare_map.get(type(node.ops[0]), ('--', ))[0]
            return f"{op} ({left}) ({right})"

    if isinstance(node, ast.BoolOp):
        left = to_haskell_expr(node.values[0])
        right = to_haskell_expr(node.values[1])
        op = bool_op_map.get(type(node.op), '--')
        return f"{op} ({left}) ({right})"

    if isinstance(node, ast.UnaryOp):
        if isinstance(node.op, ast.USub):
            return f"Math.neg ({to_haskell_expr(node.operand)})"
        if isinstance(node.op, ast.Not):
            return f"{bool_op_map[ast.Not]} ({to_haskell_expr(node.operand)})"

    if isinstance(node, ast.IfExp):
        return (f"Logic.ifElse ({to_haskell_expr(node.test)}) "
                f"({to_haskell_expr(node.body)}) "
                f"({to_haskell_expr(node.orelse)})")

    if isinstance(node, ast.Name):
        return f"Base.var \"{node.id}\""

    if isinstance(node, ast.Constant):
        v = node.value
        if isinstance(v, bool): return f"Base.bool {str(v)}"
        if isinstance(v, int): return f"Base.int32 {v}"
        if isinstance(v, str): return f"Base.string \"{v}\""

    if isinstance(node, ast.List):
        return f"Base.list [{', '.join(to_haskell_expr(elt) for elt in node.elts)}]"

    if isinstance(node, ast.Subscript):
        target = to_haskell_expr(node.value)
        index_node = node.slice
        index = None
        if isinstance(index_node, ast.Constant):  # arr[0], arr[-1]
            index = index_node.value
        elif isinstance(index_node, ast.UnaryOp) and isinstance(index_node.op, ast.USub) and isinstance(index_node.operand, ast.Constant):
            index = -index_node.operand.value
        if index == 0:
            return f"Lists.head ({target})"
        elif index == -1:
            return f"Lists.last ({target})"
        else:
            return "-- unsupported subscript"
    return "-- unknown"

def annotate_parents(tree):
    for parent in ast.walk(tree):
        for child in ast.iter_child_nodes(parent):
            child.parent = parent


def infer_arg_type(arg_name, func_body):
    for node in ast.walk(func_body):
        if isinstance(node, ast.Name) and node.id == arg_name:
            context = node.parent
            if isinstance(context, ast.Compare):
                return infer_sig(context.left if context.left.id != arg_name else context.comparators[0])
            if isinstance(context, ast.BinOp):
                return 'Int'
            if isinstance(context, ast.Call):
                return infer_sig(context.args[0])
            if isinstance(context, ast.Subscript):
                return '[Int]'
    return


# === Function generation ===
def to_haskell_function(func):
    arg = func.args.args[0].arg
    expr = func.body[0].value if func.body and isinstance(func.body[0], ast.Return) else None
    ret_t = infer_sig(expr)
    arg_t = infer_arg_type(arg, func)
    body = to_haskell_expr(expr)
    return (f"{func.name}Def :: TElement ({arg_t} -> {ret_t})\n"
            f"{func.name}Def = definitionInModule generatedModule \"{func.name}\" $\n"
            f"  Base.lambda \"{arg}\" $\n"
            f"    {body}\n")

# === Class generation ===
def to_haskell_record(cls):
    fields = []
    for stmt in cls.body:
        if isinstance(stmt, ast.Assign):
            for tgt in stmt.targets:
                if isinstance(tgt, ast.Name):
                    typ = infer_sig(stmt.value)
                    if typ == 'Int': typ = 'int32'
                    if typ == 'String': typ = 'string'
                    if typ == 'Bool': typ = 'boolean'
                    fields.append((tgt.id, typ))
    fields_s = ',\n          '.join(f"\"{n}\" Types.>: Types.{t}" for n, t in fields)
    return (f"      def \"{cls.name}\" $\n"
            f"        Types.record [\n"
            f"          {fields_s}\n"
            f"        ]")

# === Variable generation ===
def to_haskell_variable(name, node):
    sig = infer_sig(node)
    parts = [t.strip() for t in sig.split('->')]
    if len(parts) == 2:
        arg_t, ret_t = parts
    else: arg_t, ret_t = parts[0], parts[-1]
    if isinstance(node, ast.Call) and node.args and isinstance(node.args[0], ast.Name):
        arg_name = node.args[0].id
    else: arg_name = name
    body = to_haskell_expr(node)
    if len(parts) == 2:
        return (f"{name}Def :: TElement ({arg_t} -> {ret_t})\n"
                f"{name}Def = definitionInModule generatedModule \"{name}\" $\n"
                f"  Base.lambda \"{arg_name}\" $\n"
                f"    {body}\n")
    return (f"{name}Def :: TElement {ret_t}\n"
            f"{name}Def = definitionInModule generatedModule \"{name}\" $\n"
            f"  {body}\n")

# === Module assembler ===
def to_haskell_module(source):
    tree = ast.parse(source)
    annotate_parents(tree)
    classes, funcs, var = [], [], []
    for node in tree.body:
        if isinstance(node, ast.ClassDef):
            classes.append(to_haskell_record(node))
        elif isinstance(node, ast.FunctionDef):
            funcs.append(to_haskell_function(node))
        elif isinstance(node, ast.Assign):
            name = node.targets[0].id
            var.append(to_haskell_variable(name, node.value))

    elems = []
    elems += classes
    elems += [f"        Base.el {n.split('Def')[0]}Def" for n in var + funcs]

    header = """{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Generated where

import           Hydra.Dsl.Annotations
import qualified Hydra.Dsl.Bootstrap   as Bootstrap
import qualified Hydra.Dsl.Terms       as Terms
import           Hydra.Dsl.Types       as Types
import           Hydra.Sources.Tier0.Core
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Maybe            as Y
import           Hydra.Sources.Tier1.Mantle
import           Hydra.Dsl.Base        as Base
import qualified Hydra.Dsl.Lib.Math    as Math
import qualified Hydra.Dsl.Module      as Module
import qualified Hydra.Dsl.Lib.Logic   as Logic
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Lib.Lists   as Lists

generatedModule :: Module
generatedModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $ (Just "Generated")
  where
    ns = Namespace "hydra.examples"
    def = Bootstrap.datatype ns

    elements = [
""" + ',\n'.join(elems) + "\n      ]\n\n"

    return header + '\n'.join(var + funcs)

# === Example ===
python_code = '''
class Person:
    name = "Daria"
    age = 20

x = 890    

arr = [4, 5, 6]
def func1(arr):
    return arr[0] == arr[-1]

str1 = "Wonderful"
str2 = upper(str1)
eq = "awesome" == "awful"
dlina = len(str1)

def isEven(y):
    return True if y % 2 == 0 else False

def func2(x):
    return (x >= 10) and (x < 100)

'''

print(to_haskell_module(python_code))
