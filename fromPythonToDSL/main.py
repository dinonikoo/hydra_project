import ast

# === DSL Mappings ===
dsl_map = {
    'list':  ('Strings.toList',     'String -> [Int]'),
    'len':   ('Strings.length',     'String -> Int'),
}

dsl_method_map = {
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

type_map = {
            'str': 'String',
            'int': 'Int',
            'bool': 'Bool',
            'list': '[Int]'
}

# === Type Inference ===
def infer_sig(node):
    if isinstance(node, ast.Call):
        f = node.func
        if isinstance(f, ast.Attribute) and f.attr in dsl_method_map:
            return dsl_method_map[f.attr][1].split('->')[-1].strip()
        if isinstance(f, ast.Name) and f.id in dsl_map:
            return dsl_map[f.id][1].split('->')[-1].strip()
    if isinstance(node, ast.BinOp):
        return 'Int'
    if isinstance(node, ast.Compare) or isinstance(node, ast.BoolOp):
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
            method = node.func.attr
            if method in dsl_method_map:
                func = dsl_method_map[method][0]
                return f"{func} ({obj})"
            else:
                return "-- unknown method"
        elif isinstance(node.func, ast.Name):
            arg = to_haskell_expr(node.args[0])
            func = dsl_map.get(node.func.id, ('-- unsupported function', ))[0]
            return f"{func} ({arg})"

    if isinstance(node, ast.BinOp):
        left = to_haskell_expr(node.left)
        right = to_haskell_expr(node.right)
        op = bin_op_map.get(type(node.op), ('-- unsupported binary operation', ))[0]
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
            op = compare_map.get(type(node.ops[0]), ('-- unsupported compare', ))[0]
            return f"{op} ({left}) ({right})"

    if isinstance(node, ast.BoolOp):
        left = to_haskell_expr(node.values[0])
        right = to_haskell_expr(node.values[1])
        op = bool_op_map.get(type(node.op), '-- unsupported bool operation')
        return f"{op} ({left}) ({right})"

    if isinstance(node, ast.UnaryOp):
        if isinstance(node.op, ast.USub):
            return f"Math.neg ({to_haskell_expr(node.operand)})"
        if isinstance(node.op, ast.Not):
            return f"{bool_op_map[ast.Not]} ({to_haskell_expr(node.operand)})"
        return "-- unsupported unary operation"

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
        return "-- unsupported type of constant"

    if isinstance(node, ast.List):
        return f"Base.list [{', '.join(to_haskell_expr(elt) for elt in node.elts)}]"

    if isinstance(node, ast.Subscript):
        target = to_haskell_expr(node.value)
        index_node = node.slice
        index = None
        if isinstance(index_node, ast.Constant):
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
            if isinstance(context, ast.Call) and context.func.id in dsl_map:
                return dsl_map[context.func.id][1].split('->')[0].strip()
            if isinstance(context, ast.Attribute) and context.attr in dsl_method_map:
                return dsl_method_map[context.attr][1].split('->')[0].strip()
            if isinstance(context, ast.Compare):
                if context.left.id != arg_name:
                    other = context.left
                else: other = context.comparators[0]
                return infer_sig(other)
            if isinstance(context, ast.BinOp):
                return 'Int'
            if isinstance(context, ast.Subscript):
                return '[Int]'
    return 

# === Function generation ===
def to_haskell_function(func):
    args = []
    arg_types = []
    for arg in func.args.args:
        arg_name = arg.arg
        args.append(arg_name)
        if arg.annotation and isinstance(arg.annotation, ast.Name):
            py_type = arg.annotation.id
            hs_type = type_map.get(py_type, 'Int')
            arg_types.append(hs_type)
        else:
            arg_types.append(infer_arg_type(arg_name, func.body[0]))

    if func.returns and isinstance(func.returns, ast.Name):
        py_ret_type = func.returns.id
        ret_t = type_map.get(py_ret_type, 'Int')
    elif func.body and isinstance(func.body[0], ast.Return):
        ret_t = infer_sig(func.body[0].value)
    else: 'Int'

    arg_t = " -> ".join(arg_types + [ret_t])
    if func.body and isinstance(func.body[0], ast.Return):
        expr = func.body[0].value 
        body = to_haskell_expr(expr) 
    else: "-- empty function"

    lambda_chain = body
    for arg in reversed(args):
        lambda_chain = f"Base.lambda \"{arg}\" $ {lambda_chain}"
    return (f"{func.name}Def :: TElement ({arg_t})\n"
            f"{func.name}Def = definitionInModule mainModule \"{func.name}\" $\n"
            f"  {lambda_chain}\n")

# === Class generation ===
def to_haskell_record(cls):
    fields = []
    for stmt in cls.body:
        if isinstance(stmt, ast.AnnAssign): 
            if isinstance(stmt.target, ast.Name):
                type_annotation = stmt.annotation
                if isinstance(type_annotation, ast.Name):
                    py_type = type_annotation.id
                    hs_type = {
                        'str': 'string',
                        'int': 'int32',
                        'bool': 'boolean',
                        'list': '[int32]' 
                    }.get(py_type, 'unknown')
                    fields.append((stmt.target.id, hs_type))
                        
    fields_s = ',\n          '.join(f"\"{n}\" Types.>: Types.{t}" for n, t in fields)
    return (f"      def \"{cls.name}\" $\n"
            f"        Types.record [\n"
            f"          {fields_s}\n"
            f"        ]")

# === Variable generation ===
def to_haskell_variable(name, node):
    body = to_haskell_expr(node)
    if body.strip().startswith("--"):
        return f"-- {name}: {body}"
    sig = infer_sig(node)
    parts = [t.strip() for t in sig.split('->')]
    if len(parts) == 2:
        arg_t, ret_t = parts
    else:
        arg_t, ret_t = parts[0], parts[-1]

    if isinstance(node, ast.Call) and node.args:
        arg = node.args[0]
        if isinstance(arg, ast.Constant):
            return (f"{name}Def :: TElement {ret_t}\n"
                    f"{name}Def = definitionInModule mainModule \"{name}\" $\n"
                    f"  {body}\n")
        elif isinstance(arg, ast.Name):
            return (f"{name}Def :: TElement ({arg_t} -> {ret_t})\n"
                    f"{name}Def = definitionInModule mainModule \"{name}\" $\n"
                    f"  Base.lambda \"{arg.id}\" $\n"
                    f"    {body}\n")

    if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute):
        obj = node.func.value
        if isinstance(obj, ast.Name):
            return (f"{name}Def :: TElement ({arg_t} -> {ret_t})\n"
                    f"{name}Def = definitionInModule mainModule \"{name}\" $\n"
                    f"  Base.lambda \"{obj.id}\" $\n"
                    f"    {body}\n")

    return (f"{name}Def :: TElement {ret_t}\n"
            f"{name}Def = definitionInModule mainModule \"{name}\" $\n"
            f"  {body}\n")


# === Module assembler ===
def to_haskell_module(source):
    tree = ast.parse(source)
    annotate_parents(tree)
    classes, funcs, var, comments = [], [], [], []
    for node in tree.body:
        if isinstance(node, ast.ClassDef):
            class_code = to_haskell_record(node)
            if class_code.strip().startswith("--"):
                comments.append(class_code)
            else:
                classes.append(class_code)
        elif isinstance(node, ast.FunctionDef):
            func_code = to_haskell_function(node)
            if func_code.strip().startswith("--"):
                comments.append(func_code)
            else:
                funcs.append(func_code)
        elif isinstance(node, ast.Assign):
            name = node.targets[0].id
            var_code = to_haskell_variable(name, node.value)
            if var_code.strip().startswith("--"):
                comments.append(var_code)
            else:
                var.append(var_code)

    elems = []
    for cls in classes:
        elems.append(cls)
    for n in var + funcs:
        elems.append(f"        Base.el {n.split('Def')[0]}Def")
    
    
    comments_section = "\n".join(comments) + "\n\n" if comments else ""
    
    header =  """{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Main where

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

mainModule :: Module
mainModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $ (Just "Generated")
  where
    ns = Namespace "hydra.examples"
    def = Bootstrap.datatype ns

    elements = [
""" + ',\n'.join(elems) + "\n      ]\n\n"

    return header + '\n'.join(var + funcs) + comments_section 

# === Example ===
python_code = '''
class Person:
    name: str
    age: int
    married: bool

x = 890
z = - 56   
w = + z

arr = [4, 5, 6]
def func1(arr):
    return not (arr[0] == arr[-1])

str1 = "Wonderful"
str2 = str1.upper()
eq = "awesome" == "awful"

def isEven(y: int)-> bool:
    return True if y % 2 == 0 else False

def func2(x, z):
    return (x >= 10) and (z < 100)


'''

print(to_haskell_module(python_code))
