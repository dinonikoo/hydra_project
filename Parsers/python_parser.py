import sys
import re
import os
from functools import partial
from typing import List, Tuple, Optional

class PythonParser:
    def __init__(self):
        self.common_transforms = self._build_common_transforms()
        
    def parse_hydra_python(self, input_path: str, output_path: str) -> None:
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
            self.remove_hydra_imports,
            self.fix_function_declarations,
            self.fix_tuple_declarations,
            self.fix_conditional_expressions,
            self.fix_array_indexing,
            partial(self.apply_transforms_recursively, max_depth=20),
            self.add_standard_imports,
            self.remove_duplicate_imports,
            self.final_formatting
        ]
        for step in processing_steps:
            content = step(content)
        return content

    def remove_comments(self, content: str) -> str:
        content = re.sub(r'#[^\n]*', '', content)
        content = re.sub(r'""".*?"""', '', content, flags=re.DOTALL)
        return content

    def remove_hydra_imports(self, content: str) -> str:
        patterns = [
            r'from hydra\..*? import .*?(\n|$)',
            r'import hydra\..*?(\n|$)',
            r'from __future__ import annotations\n'
        ]
        for pattern in patterns:
            content = re.sub(pattern, '', content, flags=re.MULTILINE)
        return content.strip()

    def _build_common_transforms(self) -> List[Tuple[str, str, Optional[int]]]:
        return [
            (r'hydra\.lib\.equality\.equal_string\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 == \2)'),
            (r'hydra\.lib\.equality\.equal_int32\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 == \2)'),
            (r'hydra\.lib\.equality\.gt_int32\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 > \2)'),
            (r'hydra\.lib\.equality\.gte_int32\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 >= \2)'),
            (r'hydra\.lib\.equality\.lt_int32\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 < \2)'),
            (r'hydra\.lib\.equality\.lte_int32\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 <= \2)'),

            
            (r'hydra\.lib\.strings\.is_empty\s*\(\s*(.+?)\s*\)', r'(not \1)'),
            (r'hydra\.lib\.strings\.to_upper\s*\(\s*(.+?)\s*\)', r'\1.upper()'),
            (r'hydra\.lib\.strings\.to_lower\s*\(\s*(.+?)\s*\)', r'\1.lower()'),

            
            (r'hydra\.lib\.math\.add\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 + \2)'),
            (r'hydra\.lib\.math\.sub\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 - \2)'),
            (r'hydra\.lib\.math\.mul\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 * \2)'),
            (r'hydra\.lib\.math\.div\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 // \2)'),
            (r'hydra\.lib\.math\.mod\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 % \2)'),

            
            (r'hydra\.lib\.logic\.not_\s*\(\s*(.+?)\s*\)', r'(not \1)'),
            (r'hydra\.lib\.logic\.and_\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 and \2)'),
            (r'hydra\.lib\.logic\.or_\s*\(\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\1 or \2)'),
            (r'hydra\.lib\.logic\.if_else\s*\(\s*(.+?)\s*,\s*(.+?)\s*,\s*(.+?)\s*\)', r'(\2 if \1 else \3)'),

            
            (r'hydra\.lib\.lists\.head\s*\(\s*(.+?)\s*\)', r'\1[0]'),
            (r'hydra\.lib\.lists\.last\s*\(\s*(.+?)\s*\)', r'\1[-1]'),
            (r'hydra\.lib\.lists\.is_empty\s*\(\s*(.+?)\s*\)', r'(not \1)'),
            
            
            (r'frozenlist\[', r'list[', re.IGNORECASE),
        ]

    def apply_transforms_recursively(self, content: str, max_depth: int = 10) -> str:
        for _ in range(max_depth):
            new_content = content
            for transform in self.common_transforms:
                if len(transform) == 2:
                    pattern, replacement = transform
                    flags = 0
                else:
                    pattern, replacement, flags = transform
                new_content = re.sub(pattern, replacement, new_content, flags=flags)
            
            if new_content == content:
                break
            content = new_content            
        return content

    def fix_function_declarations(self, content: str) -> str:
        content = re.sub(r'def\s+(\w+)\s*\((.*?)\)\s*-\s*>\s*([^:\n]+)\s*:', r'def \1(\2) -> \3:', content)
        content = re.sub(r'def\s+(\w+)\s*\((.*?)\)\s*-\s*>', r'def \1(\2) ->', content)
        content = re.sub(r'(\w+)\s*::\s*\n\s*(.*?)\n\s*(\w+)\s*=', r'\1 :: \2\n\1 =', content)
        return content

    def fix_tuple_declarations(self, content: str) -> str:
        content = re.sub(r'tuple\s*\(\s*\[\s*((?:.*?,)*.*?)\s*\]\s*\)', r'[\1]', content)
        return content

    def fix_conditional_expressions(self, content: str) -> str:
        content = re.sub(r'([^=])\s*=\s*=\s*([^=])', r'\1 == \2', content)
        content = re.sub(r'return\s+([^ ]+)\s*%\s*([^ ]+)\s*==\s*True\s*if\s*([^ ]+)\s*else\s*False',
                        r'return \3 % \1 == \2', content)
        return content

    def fix_array_indexing(self, content: str) -> str:
        content = re.sub(r'(\w+)\s*\[\s*(\d+)\s*\]\s*\[\s*(-?\d+)\s*\]', r'\1[\2][\3]', content)
        content = re.sub(r'(\w+)\s*==\s*(\w+)\s*\[\s*(-?\d+)\s*\]\s*\[\s*(-?\d+)\s*\]', 
                        r'\1 == \2[\3][\4]', content)
        content = re.sub(r'not\s*\(\s*(\w+)\s*==\s*(\w+)\s*\[\s*(-?\d+)\s*\]\s*\[\s*(-?\d+)\s*\]\s*\)',
                        r'not (\1 == \2[\3][\4])', content)
        return content

    def add_standard_imports(self, content: str) -> str:
        standard_imports = [
            "from dataclasses import dataclass",
            ""
        ]
        return '\n'.join(standard_imports) + '\n' + content

    def remove_duplicate_imports(self, content: str) -> str:
        lines = content.split('\n')
        seen = set()
        result = []
        for line in lines:
            stripped = line.strip()
            if stripped.startswith(('from ', 'import ')) and stripped in seen:
                continue
            if stripped.startswith(('from ', 'import ')):
                seen.add(stripped)
            result.append(line)
        return '\n'.join(result)

    def final_formatting(self, content: str) -> str:
        content = re.sub(r'[ \t]+\n', '\n', content)
        content = re.sub(r'\n{3,}', '\n\n', content)
        content = re.sub(r'\[\s*-\s*1\]', '[-1]', content)
        content = re.sub(r'(\w+)\s*([=!<>]+)\s*(\w+)', r'\1 \2 \3', content)
        content = re.sub(r'not\s*\(\s*([^)]+)\s*\)', r'not (\1)', content)
        return content.strip() + '\n'

def parseHydraPython(path_to_file_inp,path_to_file_out):
    parser = PythonParser()
    parser.parse_hydra_python(path_to_file_inp,path_to_file_out)
