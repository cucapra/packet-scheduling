#!/usr/bin/env python3
"""Synthesize an unchanged replay journal/core, optionally assigning its RAM to M20K."""
import argparse
from copy import deepcopy
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import re
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
    parser.add_argument("--isolated", action="store_true")
    parser.add_argument("--force-m20k", action="store_true")
    args = parser.parse_args()
    source, build = args.source.resolve(), args.build.resolve()
    reference = json.loads((source / "manifest.json").read_text())
    assert reference["tool"] == "quartus"
    assert reference["hardware"]["configuration"] == "replay"
    rtl = source / "rtl/PifoMesh.v"
    depth = reference["hardware"]["replay_log_depth"]
    modules = re.findall(r"^module (\w+)\b.*?^endmodule", rtl.read_text(), re.M | re.S)
    candidates = []
    for module in modules:
        body = re.search(rf"^module {module}\b.*?^endmodule", rtl.read_text(), re.M | re.S).group()
        memory = re.search(rf"reg\s+\[(\d+):0\]\s+logic_ram\s+\[0:{depth - 1}\];", body)
        if memory and "io_push_payload_post" in body and "io_push_payload_engineId" in body:
            candidates.append((module, body, int(memory[1]) + 1))
    assert len(candidates) == 1, [c[0] for c in candidates]
    module, body, width = candidates[0]
    top = re.search(r"^module PifoMesh\b.*?^endmodule", rtl.read_text(), re.M | re.S).group()
    instances = re.findall(rf"\b{module}\s+(\w+)\s*\(", top)
    assert len(instances) == 1, instances
    build.mkdir(parents=True, exist_ok=False)
    if args.isolated:
        view = build / "rtl"
        view.mkdir()
        (view / "PifoMesh.v").write_text(body.replace(f"module {module} (", "module PifoMesh (", 1) + "\n")
        target_entity = "PifoMesh"
    else:
        init = reference["quartus_initialization"]
        assert init["canonical_rtl_unchanged"]
        view = source / "quartus-rtl"
        for name, expected in init["derived_sha256"].items():
            assert digest(view / name) == expected, name
        (build / "rtl").symlink_to(source / "rtl", target_is_directory=True)
        (build / "quartus-rtl").symlink_to(view, target_is_directory=True)
        target_entity = module
    assignment = (f"set_instance_assignment -name RAMSTYLE_ATTRIBUTE M20K "
                  f"-entity {target_entity} -to logic_ram\n")
    manifest = deepcopy(reference)
    for key in ("completed_utc", "elapsed_seconds", "error"):
        manifest.pop(key, None)
    manifest.update(status="prepared", timestamp_utc=datetime.now(timezone.utc).isoformat(),
                    rtl_reused_from=str(source), rtl_source_manifest_sha256=digest(source / "manifest.json"),
                    workflow_sha256={"probe_replay_journal.py": digest(Path(__file__)),
                                     "create_project.tcl": digest(PROJECT / "synthesis/create_project.tcl")},
                    journal_probe={"scope": "isolated_journal" if args.isolated else "full_core",
                                   "force_m20k": args.force_m20k, "source_module": module,
                                   "source_instance": instances[0], "width": width, "depth": depth,
                                   "source_rtl_sha256": digest(rtl),
                                   "module_sha256": hashlib.sha256(body.encode()).hexdigest(),
                                   "qsf_assignment": assignment.strip() if args.force_m20k else None,
                                   "canonical_rtl_unchanged": not args.isolated,
                                   "read_during_write_semantics_unchanged": True})
    output = build / "manifest.json"
    output.write_text(json.dumps(manifest, indent=2) + "\n")
    root = Path(reference["quartus_root"])
    env = os.environ.copy()
    env.update(QUARTUS_ROOTDIR=str(root), LM_LICENSE_FILE=reference["license_source"])
    env["PATH"] = str(root / "bin") + os.pathsep + env["PATH"]
    started = time.monotonic()
    try:
        run([root / "bin/quartus_sh", "--version"], build, build / "quartus-version.txt", env)
        assert (build / "quartus-version.txt").read_text().strip() == reference["tool_version"].strip()
        run([root / "bin/quartus_sh", "-t", PROJECT / "synthesis/create_project.tcl", build,
             reference["part"], str(reference["threads"]), str(1000 / reference["clock_target_mhz"]), view],
            build, build / "project.log", env)
        if args.force_m20k:
            with (build / "pifo.qsf").open("a") as stream:
                stream.write(assignment)
        manifest["status"] = "synthesis_running"
        output.write_text(json.dumps(manifest, indent=2) + "\n")
        run([root / "bin/quartus_syn", "pifo"], build, build / "synthesis.log", env)
        measured, parsed = resources(build, "quartus")
        (build / "resource-summary.json").write_text(json.dumps(parsed, indent=2) + "\n")
        (build / "journal-probe-result.json").write_text(json.dumps(measured, indent=2) + "\n")
        manifest["status"] = "synthesis_complete"
        print(json.dumps(measured, indent=2), flush=True)
    except Exception as error:
        manifest.update(status="synthesis_failed", error=str(error))
        raise
    finally:
        manifest.update(elapsed_seconds=round(time.monotonic() - started, 3),
                        completed_utc=datetime.now(timezone.utc).isoformat())
        output.write_text(json.dumps(manifest, indent=2) + "\n")


if __name__ == "__main__":
    main()
