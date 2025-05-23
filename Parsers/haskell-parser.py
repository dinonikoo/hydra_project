import sys
import re
import os
from functools import partial
from typing import List, Optional, Tuple

class HaskellParser:
    def __init__(self):
        self.common_transforms = self._build_common_transforms()
        
    def parse_hydra_haskell(self, input_path: str, output_path: str) -> None:
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
            self.remove_module_headers,
            self.transform_module_declaration,
            self.remove_hydra_imports,
            partial(self.apply_transforms_recursively, max_depth=20),
            self.compact_lists,
            self.fix_function_declarations,
            self.add_standard_imports,
            self.final_formatting
        ]
        for step in processing_steps:
            content = step(content)
        return content
    

    def remove_comments(self, content: str) -> str:
        content = re.sub(r'--.*?$', '', content, flags=re.MULTILINE)
        content = re.sub(r'\{-(?:(?!-\}).)*-\}', '', content, flags=re.DOTALL)
        return content

    def remove_module_headers(self, content: str) -> str:
        return re.sub(r'-- \|.*?\n', '', content)

    def transform_module_declaration(self, content: str) -> str:
        return re.sub(
            r'module\s+Hydra\.(\w+)\s+where',
            r'module \1 where',
            content
        )

    def remove_hydra_imports(self, content: str) -> str:
        patterns = [
            r'import\s+(qualified\s+)?Hydra\..*?(\n|$)',
            r'import\s+(qualified\s+)?Data\.(Int|List|Map|Set)\..*?(\n|$)'
        ]
        for pattern in patterns:
            content = re.sub(pattern, '', content, flags=re.MULTILINE)
        return content

    def _build_common_transforms(self) -> List[Tuple[str, str, Optional[int]]]:
        return [
            (r'Logic\.ifElse\s+\((.*?)\)\s+([^\s]+)\s+\((.*?)\)', r'if \1 then \2 else (\3)'),


            (r'Equality\.equalInt32\s+(.+?)\s+(.+?)', r'\1 == \2'),
            (r'Equality\.equalString\s+(.+?)\s+(.+?)', r'\1 == \2'),
            (r'Equality\.gtInt32\s+(.+?)\s+(.+?)', r'\1 > \2'),
            (r'Equality\.gteInt32\s+(.+?)\s+(.+?)', r'\1 >= \2'),
            (r'Equality\.ltInt32\s+(.+?)\s+(.+?)', r'\1 < \2'),
            (r'Equality\.lteInt32\s+(.+?)\s+(.+?)', r'\1 <= \2'),
            (r'Equality.equalBoolean\s+(.+?)\s+(.+?)', r'\1 == \2'),


            (r'Strings\.cat\s+(.+?)', r'concat \1'),
            (r'Strings\.cat2\s+(.+?)\s+(.+?)', r'\1 ++ \2'),
            (r'Strings\.fromList\s+(.+?)', r'map chr \1'),
            (r'Strings\.intercalate\s+(.+?)\s+(.+?)', r'intercalate \1 \2'),
            (r'Strings\.isEmpty\s+(.+?)', r'null \1'),
            (r'Strings\.length\s+(.+?)', r'length \1'),
            (r'Strings\.splitOn\s+(.+?)\s+(.+?)', r'splitOn \1 \2'),
            (r'Strings\.toList\s+(.+?)', r'map ord \1'),
            (r'Strings\.toLower\s+(.+?)', r'map toLower \1'),
            (r'Strings\.toUpper\s+(.+?)', r'map toUpper \1'),


            (r'Math\.add\s+(\([^()]*\)|\w+)\s+(\([^()]*\)|\w+)', r'\1 + \2'),
            (r'Math\.sub\s+(\([^()]*\)|\w+)\s+(\([^()]*\)|\w+)', r'\1 - \2'),
            (r'Math\.mul\s+(\([^()]*\)|\w+)\s+(\([^()]*\)|\w+)', r'\1 * \2'),
            (r'Math\.div\s+(\([^()]*\)|\w+)\s+(\([^()]*\)|\w+)', r'\1 div \2'),
            (r'Math\.mod\s+(\([^()]*\)|\w+)\s+(\([^()]*\)|\w+)', r'\1 mod \2'),
            (r'Math\.rem\s+(\([^()]*\)|\w+)\s+(\([^()]*\)|\w+)', r'\1 rem \2'),


            (r'Logic\.not\s+(.+?)', r'not \1'),
            (r'Logic\.and\s+(\([^()]*\)|\w+)\s+(\([^()]*\)|\w+)', r'\1 && \2'),
            (r'Logic\.or\s+(\([^()]*\)|\w+)\s+(\([^()]*\)|\w+)', r'\1 || \2'),
            (r'Logic\.ifElse\s+([^\s\(]+)\s+([^\s\(]+)\s+([^\s\(]+)', r'if \1 then \2 else \3'),
            (r'Logic\.ifElse\s+\(([^\)]+)\)\s+([^\s\(]+)\s+([^\s\(]+)', r'if (\1) then \2 else \3'),
            (r'Logic\.ifElse\s+([^\s\(]+)\s+([^\s\(]+)\s+\(([^\)]*Logic\.ifElse[^\)]*)\)', r'if \1 then \2 else (\3)'),
            (r'Logic\.ifElse\s+\(([^\)]+)\)\s+([^\s\(]+)\s+\(([^\)]*Logic\.ifElse[^\)]*)\)', r'if (\1) then \2 else (\3)'),


            (r'Lists\.concat\s+(\w+)', r'concat \1'),
            (r'Lists\.concat2\s+(.+?)\s+(.+?)', r'\1 ++ \2'),
            (r'Lists\.cons\s+(\w+)\s+(\w+)', r'\1 : \2'),
            (r'Lists\.filter\s+(\w+)\s+(\w+)', r'filter \1 \2'),
            (r'Lists\.foldl\s+(\w+)\s+(\w+)\s+(\w+)', r'foldl \1 \2 \3'),
            (r'Lists\.head\s+(\w+)', r'head \1'),
            (r'Lists\.intercalate\s+(\w+)\s+(\w+)', r'intercalate \1 \2'),
            (r'Lists\.length\s+(\w+)', r'length \1'),
            (r'Lists\.map\s+(\w+)\s+(\w+)', r'map \1 \2'),
            (r'Lists\.null\s+(\w+)', r'null \1'),
            (r'Lists\.reverse\s+(\w+)', r'reverse \1'),
            (r'Lists\.at\s+(\([^()]*\)|\w+)\s+(\([^()]*\)|\w+)', r'\2 !! \1'),
            (r'Lists\.at\s+(\w+)\s+\[([^\]]+)\]', r'[\2] !! \1', re.DOTALL),


            (r'Data\.Int\.Int16', r'Int16'),
            (r'Data\.Int\.Int64', r'Int64'),
            (r'I\.Int16', r'Int16'),
            (r'I\.Int64', r'Int64'),


            (r'\((\w+)\)\s*=', r'\1 ='),
            (r'(\w+)\s*::\s*(.*?)\n(\w+)\s*=', r'\1 :: \2\n\1 ='),
            (r'\((\([^()]+\))\)', r'\1'),
            (r'\((\w+)\)', r'\1'),           
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
    def compact_lists(self, content: str, max_line_length: int = 80) -> str:
        def replace_match(match):
            list_items = re.findall(r'\b[\w\"]+\b', match.group(1))
            compact_repr = f"[{', '.join(list_items)}]"
            if len(compact_repr) <= max_line_length:
                return compact_repr
            return match.group(0)
        pattern = r'\[\s*((?:\s*\b[\w\"]+\b\s*,\s*)*\s*\b[\w\"]+\b\s*)\s*\]'
        return re.sub(pattern, replace_match, content, flags=re.MULTILINE)


    def fix_function_declarations(self, content: str) -> str:
        content = re.sub(r'(\w+)\s*::\s*\n\s*(.*?)\n\s*(\w+)\s*=', r'\1 :: \2\n\1 =', content)
        content = re.sub(r'(\w+)\s+(\(.*?\)|\w+)\s*=\s*', r'\1 \2 = ', content)
        return content

    def add_standard_imports(self, content: str) -> str:
        imports = [
            "import Data.Char (chr, ord, isLower, isUpper, toLower, toUpper)",
            "import Data.List (intercalate, intersperse, splitOn, null, length,",
            "                 foldl, filter, map, concat, head, last, reverse,",
            "                 tail, zip, zipWith, (!!))",
            "import Data.Maybe (listToMaybe)",
            "import qualified Data.List as List",
            "import qualified Data.Text as Text"
        ]
        module_match = re.search(r'module\s+\w+\s+where', content)
        if module_match:
            pos = module_match.end()
            content = content[:pos] + '\n\n' + '\n'.join(imports) + content[pos:]
        return content

    def final_formatting(self, content: str) -> str:
        content = re.sub(r'[ \t]+\n', '\n', content)
        content = re.sub(r'\n{3,}', '\n\n', content)
        content = re.sub(r'(\S)\s*([+\-*/])\s*(\S)', r'\1 \2 \3', content)
        return content.strip() + '\n'


if __name__ == "__main__":    
    parser = HaskellParser()
    parser.parse_hydra_haskell(sys.argv[1], sys.argv[2])
