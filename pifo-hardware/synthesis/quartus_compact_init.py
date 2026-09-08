"""Create a Quartus-only RTL view with equivalent compact zero-filled MIFs.

Canonical Verilog/.bin files remain the simulator and Vivado inputs. Refuse
unknown initialization syntax or nonzero/malformed data instead of changing it.
"""
import hashlib
import json
from pathlib import Path
import re
import shutil


def create_view(build: Path) -> dict:
    source = build / "rtl"
    target = build / "quartus-rtl"
    if target.exists():
        shutil.rmtree(target)
    target.mkdir()
    records = []
    initial = re.compile(r'  initial begin\s*\$readmemb\("([^"/]+)",\s*(\w+)\);\s*end')

    def convert_module(match):
        module = match.group(0)
        for init in list(initial.finditer(module)):
            filename, ram = init.groups()
            declaration = re.compile(r'(  reg \[(\d+):0\] ' + re.escape(ram) + r' \[0:(\d+)\];)')
            decl = declaration.search(module)
            if not decl:
                raise ValueError(f"Unknown RAM declaration for {filename}: {ram}")
            width, depth = int(decl[2]) + 1, int(decl[3]) + 1
            binary = source / filename
            digest = hashlib.sha256()
            # Check exact row width, row count, and zero contents without
            # creating millions of Python objects or loading whole files.
            row = b"0" * width + b"\n"
            expected = row * min(depth, 65536)
            size = 0
            with binary.open("rb") as stream:
                while chunk := stream.read(len(expected)):
                    if chunk != expected[:len(chunk)] or len(chunk) % len(row):
                        raise ValueError(f"Expected fixed-width zero rows in {binary}")
                    digest.update(chunk)
                    size += len(chunk)
            if size != depth * len(row):
                raise ValueError(f"Wrong initialization depth in {binary}")
            mif = target / (filename + ".mif")
            mif.write_text(f"WIDTH={width};\nDEPTH={depth};\nADDRESS_RADIX=HEX;\nDATA_RADIX=HEX;\n"
                           f"CONTENT BEGIN\n  [0..{depth - 1:X}] : 0;\nEND;\n")
            module = declaration.sub(lambda d: f'  (* ram_init_file = "{mif}" *)\n' + d[1], module, count=1)
            module = module.replace(init[0], "  // synthesis translate_off\n" + init[0] +
                                    "\n  // synthesis translate_on", 1)
            records.append({"binary": filename, "binary_sha256": digest.hexdigest(),
                            "width": width, "depth": depth, "contents": "all_zero",
                            "mif": mif.name})
        return module

    for rtl in sorted(source.iterdir()):
        if rtl.suffix not in (".v", ".sv"):
            continue
        contents = rtl.read_text()
        expected_count = contents.count("$readmemb(")
        before = len(records)
        converted = re.sub(r"\bmodule\s+\w+\b.*?\bendmodule\b", convert_module, contents, flags=re.S)
        if len(records) - before != expected_count:
            raise ValueError(f"Unhandled readmemb syntax in {rtl}")
        (target / rtl.name).write_text(converted)
    result = {"format": "quartus-compact-zero-mif-v1", "canonical_rtl_unchanged": True,
              "memories": records, "derived_sha256": {
                  p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(target.iterdir())}}
    (build / "quartus-initialization.json").write_text(json.dumps(result, indent=2) + "\n")
    return result
