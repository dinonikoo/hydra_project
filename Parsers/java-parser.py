import sys
import re
import os
from typing import List, Optional, Tuple, Match

class JavaParser:
    def __init__(self):
        self.common_transforms = self._build_common_transforms()
        
    def parse_hydra_java(self, input_path: str, output_path: str) -> None:
        try:
            content = self._read_file(input_path)
            content = self._apply_processing_pipeline(content)
            self._write_file(output_path, content)
            print(f"File successfully converted: {output_path}")
        except Exception as e:
            print(f"Error processing file: {str(e)}")

    def _read_file(self, path: str) -> str:
        with open(path, 'r', encoding='utf-8') as f:
            return f.read()

    def _write_file(self, path: str, content: str) -> None:
        output_dir = os.path.dirname(path)
        if output_dir:
            os.makedirs(output_dir, exist_ok=True)
        with open(path, 'w', encoding='utf-8') as f:
            f.write(content)

    def _apply_processing_pipeline(self, content: str) -> str:
        processing_steps = [
            self.remove_comments,
            self.apply_transforms_recursively,
            self.fix_integer_declarations,
            self.fix_boolean_declarations,
            self.add_standard_imports,
            self.final_formatting
        ]
        for step in processing_steps:
            content = step(content)
        return content

    def remove_comments(self, content: str) -> str:
        content = re.sub(r'//.*?$', '', content, flags=re.MULTILINE)
        content = re.sub(r'/\*.*?\*/', '', content, flags=re.DOTALL)
        return content

    def _build_common_transforms(self) -> List[Tuple[str, str, Optional[int]]]:
        return [
            (r'hydra\.lib\.strings\.IsEmpty\.apply\(\s*([^)]+)\s*\)', r'(\1).isEmpty()'),
            (r'hydra\.lib\.strings\.Length\.apply\(\s*([^)]+)\s*\)', r'(\1).length()'),
            (r'hydra\.lib\.strings\.ToLower\.apply\(\s*([^)]+)\s*\)', r'(\1).toLowerCase()'),
            (r'hydra\.lib\.strings\.ToUpper\.apply\(\s*([^)]+)\s*\)', r'(\1).toUpperCase()'),
            (r'hydra\.lib\.strings\.FromList\.apply\(\s*([^)]+)\s*\)', r'new String(\1)'),
            (r'hydra\.lib\.strings\.Cat\.apply\(\s*([^)]+)\s*\)', r'String.join("", \1)'),
            (r'hydra\.lib\.strings\.Cat2\.apply\((.*?),\s*(.*?)\)', r'\1 + \2'),
            (r'hydra\.lib\.strings\.Intercalate\.apply\((.*?),\s*(.*?)\)', r'String.join(\1, \2)'),
            (r'hydra\.lib\.strings\.SplitOn\.apply\((.*?),\s*(.*?)\)', r'\1.split(\2)'),          




            (r'hydra\.lib\.chars\.(ToUpper|ToLower|IsUpper|IsLower)\.apply\((\d+)\)',
             r'Character.\1((char)\2)'),
            (r'hydra\.lib\.chars\.(ToUpper|ToLower|IsUpper|IsLower)\.apply\(\'([^\'])\'\)',
             r'Character.\1(\'\2\')'),
            (r'hydra\.lib\.chars\.(ToUpper|ToLower|IsUpper|IsLower)\.apply\("([^"])"\)',
             r'Character.\1(\'\2\'[0])'),
            (r'hydra\.lib\.chars\.(ToUpper|ToLower|IsUpper|IsLower)\.apply\((.*)\)',
             r'Character.\1(\2)'),
            

            (r'hydra\.lib\.math\.Neg\.apply\((\d+)\)', r'-\1'),
            (r'hydra\.lib\.math\.Add\.apply\(([^,]+),\s*([^)]+)\)', r'(\1 + \2)'),
            (r'hydra\.lib\.math\.Sub\.apply\(([^,]+),\s*([^)]+)\)', r'(\1 - \2)'),
            (r'hydra\.lib\.math\.Mul\.apply\(([^,]+),\s*([^)]+)\)', r'(\1 * \2)'),
            (r'hydra\.lib\.math\.Div\.apply\(([^,]+),\s*([^)]+)\)', r'(\1 / \2)'),
            (r'hydra\.lib\.math\.Rem\.apply\(([^,]+),\s*([^)]+)\)', r'(\1 % \2)'),
            (r'hydra\.lib\.math\.Mod\.apply\(([^,]+),\s*([^)]+)\)', r'Math.floorMod(\1, \2)'),

            (r'hydra\.lib\.equality\.EqualString\.apply\(([^,]+),\s*([^)]+)\)', r'\1.equals(\2)'),
            (r'hydra\.lib\.equality\.EqualInt32\.apply\(([^,]+),\s*([^)]+)\)', r'\1 == \2'),
            (r'hydra\.lib\.equality\.GtInt32\.apply\(([^,]+),\s*([^)]+)\)', r'\1 > \2'),
            (r'hydra\.lib\.equality\.GteInt32\.apply\(([^,]+),\s*([^)]+)\)', r'\1 >= \2'),
            (r'hydra\.lib\.equality\.LtInt32\.apply\(([^,]+),\s*([^)]+)\)', r'\1 < \2'),
            (r'hydra\.lib\.equality\.LteInt32\.apply\(([^,]+),\s*([^)]+)\)', r'\1 <= \2'), 
            

            (r'hydra\.lib\.logic\.IfElse\.apply\(([^,]+),\s*([^,]+),\s*([^)]+)\)', r'(\1) ? \2 : \3'),

        ]


    def _transform_expression(self, expr: str) -> str:
        while True:
            transformed = re.sub(
                r'hydra\.lib\.[a-z]+\.\w+\.apply\(([^()]*(?:\(.*?\)[^()]*)*)\)',
                lambda m: self._transform_single_level(m.group(0)),
                expr
            )
            if transformed == expr:
                break
            expr = transformed
        return expr

    def _transform_single_level(self, match: str) -> str:
        transformed = match
        for pattern, replacement in self.common_transforms:
            if callable(replacement):
                transformed = re.sub(pattern, replacement, transformed)
            else:
                transformed = re.sub(pattern, replacement, transformed)
        return transformed

    def apply_transforms_recursively(self, content: str) -> str:
        content = self._transform_nested_calls(content)
        for pattern, replacement in self.common_transforms:
            if callable(replacement):
                content = re.sub(pattern, replacement, content)
            else:
                content = self._process_pattern(content, pattern, replacement)
        return content

    def _transform_nested_calls(self, content: str) -> str:
        pattern = r'hydra\.lib\.[a-z]+\.\w+\.apply\(([^()]*(?:\(.*?\)[^()]*)*)\)'
        return re.sub(
            pattern,
            lambda m: self._transform_single_level(m.group(0)),
            content
        )

    def _process_pattern(self, content: str, pattern: str, replacement: str) -> str:
        while True:
            new_content = re.sub(pattern, replacement, content)
            if new_content == content:
                break
            content = new_content
        return content

    def fix_integer_declarations(self, content: str) -> str:
        content = re.sub(r'Integer\s+(\w+\s*[=;])', r'int \1', content)
        content = re.sub(r'\(Integer\)\s+(\w+)', r'(int) \1', content)
        return content

    def fix_boolean_declarations(self, content: str) -> str:
        content = re.sub(r'Boolean\s+(\w+\s*[=;])', r'boolean \1', content)
        content = re.sub(r'\(Boolean\)\s+(\w+)', r'(boolean) \1', content)
        return content

    def add_standard_imports(self, content: str) -> str:
        imports = [
            "import java.util.*;",
            "import java.lang.Character;",
            "import java.lang.Math;"
        ]
        package_match = re.search(r'package\s+[\w.]+;', content)
        if package_match:
            pos = package_match.end()
            content = content[:pos] + '\n\n' + '\n'.join(imports) + content[pos:]
        else:
            content = '\n'.join(imports) + '\n\n' + content
        return content

    def final_formatting(self, content: str) -> str:
        content = re.sub(r'\(\((.*?)\)\)', r'(\1)', content)
        content = re.sub(r'\((\w+)\)\.', r'\1.', content)
        content = re.sub(r'\((".*?")\)\.', r'\1.', content)
        content = re.sub(r'\((\((.*?\?.*?:.*?)\))\)\.', r'(\2).', content)
        content = re.sub(r'\n{3,}', '\n\n', content)
        content = re.sub(r'[ \t]+\n', '\n', content)
        content = re.sub(r'(\S)\s+([=+\-*/%<>!])\s+(\S)', r'\1 \2 \3', content)
        content = re.sub(r'\(\s+', '(', content)
        content = re.sub(r'\s+\)', ')', content)
        content = re.sub(r'\s+,', ',', content)
        content = re.sub(r'\s+;', ';', content)
        content = re.sub(r',\s*\n\s*', ', ', content)
        content = re.sub(r'=\s*\n\s*', '= ', content)
        return content.strip() + '\n'

if __name__ == "__main__":    
    parser = JavaParser()
    parser.parse_hydra_java(sys.argv[1], sys.argv[2])
