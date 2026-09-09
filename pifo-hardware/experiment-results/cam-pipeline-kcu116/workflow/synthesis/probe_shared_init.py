#!/usr/bin/env python3
"""Resynthesize a separate Quartus build with shared, identical zero MIFs."""
import argparse
from copy import deepcopy
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import time

from run import run

PROJECT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT / "hw/python"))
from pifo_hardware_overhead import resources


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument("build", type=Path)
    args = parser.parse_args()
    source, build = args.source.resolve(), args.build.resolve()
    reference = json.loads((source / "manifest.json").read_text())
    init = reference["quartus_initialization"]
    assert reference["tool"] == "quartus" and init["canonical_rtl_unchanged"]
    assert init["format"] == "quartus-compact-zero-mif-v1"
    old = source / "quartus-rtl"
    for name, expected in init["derived_sha256"].items():
        assert digest(old / name) == expected, name
    build.mkdir(parents=True, exist_ok=False)
    (build / "rtl").symlink_to(source / "rtl", target_is_directory=True)
    view = build / "quartus-rtl"
    view.mkdir()
    replacements = {}
    records = []
    for record in init["memories"]:
        width, depth = record["width"], record["depth"]
        assert record["contents"] == "all_zero"
        expected = (f"WIDTH={width};\nDEPTH={depth};\nADDRESS_RADIX=HEX;\nDATA_RADIX=HEX;\n"
                    f"CONTENT BEGIN\n  [0..{depth - 1:X}] : 0;\nEND;\n")
        assert (old / record["mif"]).read_text() == expected
        name = f"zero-w{width}-d{depth}.mif"
        (view / name).write_text(expected)
        replacements[str(old / record["mif"])] = str(view / name)
        records.append({**record, "mif": name})
    for rtl in old.iterdir():
        if rtl.suffix not in (".v", ".sv"):
            continue
        text = rtl.read_text()
        for before, after in replacements.items():
            text = text.replace(before, after)
        assert str(old) not in text
        (view / rtl.name).write_text(text)
    initialization = {**init, "memories": records, "shared_identical_zero_files": True,
                      "derived_sha256": {p.name: digest(p) for p in sorted(view.iterdir())}}
    (build / "quartus-initialization.json").write_text(json.dumps(initialization, indent=2) + "\n")
    manifest = deepcopy(reference)
    for key in ("completed_utc", "elapsed_seconds"):
        manifest.pop(key, None)
    manifest.update(status="prepared", timestamp_utc=datetime.now(timezone.utc).isoformat(),
                    rtl_reused_from=str(source), rtl_source_manifest_sha256=digest(source / "manifest.json"),
                    quartus_initialization=initialization, shared_init_reference=str(source),
                    workflow_sha256={"probe_shared_init.py": digest(Path(__file__)),
                                     "create_project.tcl": digest(PROJECT / "synthesis/create_project.tcl")})
    output = build / "manifest.json"
    output.write_text(json.dumps(manifest, indent=2) + "\n")
    root = Path(manifest["quartus_root"])
    env = os.environ.copy()
    env.update(QUARTUS_ROOTDIR=str(root), LM_LICENSE_FILE=manifest["license_source"])
    env["PATH"] = str(root / "bin") + os.pathsep + env["PATH"]
    started = time.monotonic()
    try:
        run([root / "bin/quartus_sh", "--version"], build, build / "quartus-version.txt", env)
        assert (build / "quartus-version.txt").read_text().strip() == manifest["tool_version"].strip()
        run([root / "bin/quartus_sh", "-t", PROJECT / "synthesis/create_project.tcl", build,
             manifest["part"], str(manifest["threads"]), str(1000 / manifest["clock_target_mhz"]), view],
            build, build / "project.log", env)
        manifest["status"] = "synthesis_running"
        output.write_text(json.dumps(manifest, indent=2) + "\n")
        run([root / "bin/quartus_syn", "pifo"], build, build / "synthesis.log", env)
        measured, parsed = resources(build, "quartus")
        comparison = {"resources": measured, "source": str(source), "canonical_rtl_unchanged": True,
                      "individual_mif_files": len(replacements), "shared_mif_files": len(set(replacements.values()))}
        if reference["status"] == "synthesis_complete":
            previous, _ = resources(source, "quartus")
            comparison.update(reference_resources=previous, identical_resource_counts=measured == previous)
        (build / "resource-summary.json").write_text(json.dumps(parsed, indent=2) + "\n")
        (build / "shared-init-comparison.json").write_text(json.dumps(comparison, indent=2) + "\n")
        manifest["status"] = "synthesis_complete"
        print(json.dumps(comparison, indent=2), flush=True)
    except (subprocess.CalledProcessError, AssertionError, ValueError) as error:
        manifest.update(status="synthesis_failed", error=str(error))
        raise
    finally:
        manifest.update(elapsed_seconds=round(time.monotonic() - started, 3),
                        completed_utc=datetime.now(timezone.utc).isoformat())
        output.write_text(json.dumps(manifest, indent=2) + "\n")


if __name__ == "__main__":
    main()
