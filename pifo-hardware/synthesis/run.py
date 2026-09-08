#!/usr/bin/env python3
"""Generate the complete PIFO mesh and run Quartus or Vivado synthesis only."""

import argparse
import datetime
import hashlib
import json
import math
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import time
import urllib.request
import xml.etree.ElementTree as ET

HERE = Path(__file__).resolve().parent
PROJECT = HERE.parent
BOARD = "agilex_f_series_development_kit"


def quartus_root(override):
    candidates = [override, os.environ.get("QUARTUS_ROOTDIR")]
    executable = shutil.which("quartus_sh")
    if executable:
        candidates.append(str(Path(executable).resolve().parent.parent))
    candidates.append("/data/work/quartus/quartus")
    for candidate in candidates:
        if candidate and (Path(candidate) / "bin/quartus_sh").is_file():
            return Path(candidate).resolve()
    raise ValueError("Quartus not found; set QUARTUS_ROOTDIR or --quartus-root")


def board_info(root):
    path = root / f"common/devkits/{BOARD}/{BOARD}.devkit_info"
    if not path.exists():
        return None
    xml = ET.parse(path).getroot()
    return {"definition": str(path), **{
        key: xml.findtext(key) for key in ("name", "version", "part", "design")
    }}


def version_key(path):
    return tuple(int(value) for value in re.findall(r"\d+", str(path)))


def vivado_root(override):
    candidates = [override, os.environ.get("XILINX_VIVADO")]
    executable = shutil.which("vivado")
    if executable:
        candidates.append(str(Path(executable).resolve().parent.parent))
    for base, pattern in (("/data/work/vivado", "*/Vivado"),
                          ("/opt/Xilinx/Vivado", "*"),
                          ("/tools/Xilinx/Vivado", "*")):
        candidates.extend(sorted(Path(base).glob(pattern), key=version_key, reverse=True))
    for candidate in candidates:
        if candidate and (Path(candidate) / "bin/vivado").is_file():
            return Path(candidate).resolve()
    raise ValueError("Vivado not found; set XILINX_VIVADO or --vivado-root")


def vivado_board_info(root, name):
    candidates = []
    for data in (root / "data", root.parent / "data"):
        for relative in ("xhub/boards/XilinxBoardStore/boards/Xilinx", "boards/board_files"):
            candidates.extend((data / relative / name).glob("*/board.xml"))
    for path in sorted(set(candidates), key=lambda p: version_key(p.parent.name), reverse=True):
        xml = ET.parse(path).getroot()
        component = next((c for c in xml.iter("component")
                          if c.get("type") == "fpga"), None)
        if component is not None:
            return {"definition": str(path), "name": xml.get("display_name"),
                    "board_name": xml.get("name"), "version": xml.findtext("file_version"),
                    "part": component.get("part_name")}
    return None


def license_source(root, override):
    if override:
        return override
    for key in ("LM_LICENSE_FILE", "ALTERAD_LICENSE_FILE"):
        if os.environ.get(key):
            return os.environ[key]
    files = []
    for folder in (root.parent / "Intel_lic", root.parent / "licenses"):
        files.extend(str(path) for path in sorted(folder.glob("*.dat")))
    return os.pathsep.join(files)


def run(command, cwd, log, env):
    print(f"Running {shlex.join(map(str, command))}\n  log: {log}", flush=True)
    with log.open("w") as stream:
        result = subprocess.run(command, cwd=cwd, env=env, stdout=stream,
                                stderr=subprocess.STDOUT)
    if result.returncode:
        print("\n".join(log.read_text(errors="replace").splitlines()[-20:]), file=sys.stderr)
        raise subprocess.CalledProcessError(result.returncode, command)


def sbt_launcher():
    version = re.search(r"sbt.version=(\S+)",
                        (PROJECT / "project/build.properties").read_text()).group(1)
    jar = HERE / f".tools/sbt-launch-{version}.jar"
    if not jar.exists():
        jar.parent.mkdir(parents=True, exist_ok=True)
        url = f"https://repo.maven.apache.org/maven2/org/scala-sbt/sbt-launch/{version}/{jar.name}"
        print(f"Downloading sbt {version} launcher from Maven Central", flush=True)
        data = urllib.request.urlopen(url, timeout=60).read()
        expected = urllib.request.urlopen(url + ".sha1", timeout=30).read().decode().strip()
        if hashlib.sha1(data).hexdigest() != expected:
            raise ValueError("sbt launcher checksum mismatch")
        jar.write_bytes(data)
    return jar


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--tool", choices=("quartus", "vivado"), default="quartus")
    parser.add_argument("--configuration", choices=("replay", "static", "dynamic"), default="replay",
                        help="replay (default), ordinary tables with ignored commits (static), or legacy read/copy (dynamic)")
    parser.add_argument("--control-queue-depth", type=int, default=4,
                        help="Shared control FIFO entries for all configurations; replay reserves one for commit")
    parser.add_argument("--pifo-backend", choices=("house", "stock", "external"), default="house",
                        help="house/stock implementation, or external to expose PIFO ports and measure RIO alone")
    parser.add_argument("--name", help="Build name; replay defaults use baseline-replay or baseline-replay-vivado")
    parser.add_argument("--build-root", type=Path, default=HERE / "build",
                        help="Parent of build directories; override for large synthesis sweeps")
    parser.add_argument("--engines", type=int, default=2)
    parser.add_argument("--vpifos", type=int, default=32,
                        help="vPIFO IDs per PE and global flow-ID capacity (currently coupled)")
    parser.add_argument("--entries-per-pe", type=int, default=1024,
                        help="Shared sorted entries in each PE, divisible by --vpifos")
    parser.add_argument("--priority-bits", type=int, default=8)
    parser.add_argument("--clock-mhz", type=float, default=100)
    parser.add_argument("--threads", type=int, default=8)
    parser.add_argument("--part", help="Override the part read from the installed board definition")
    parser.add_argument("--quartus-root", help="Directory containing bin/quartus_sh")
    parser.add_argument("--quartus-compact-init", action="store_true",
                        help="Use a verified equivalent zero-filled MIF view for Quartus RAM initialization")
    parser.add_argument("--vivado-root", help="Directory containing bin/vivado")
    parser.add_argument("--vivado-directive", default="default",
                        choices=("default", "RuntimeOptimized", "AreaOptimized_high", "AreaOptimized_medium"),
                        help="Vivado synthesis optimization directive, recorded in the manifest")
    parser.add_argument("--vivado-allow-over-capacity", action="store_true",
                        help="Estimate oversized netlists by skipping Vivado's pre-mapping capacity check; never implies a device fit")
    parser.add_argument("--board", default="kcu116", help="Vivado board directory name (default kcu116)")
    parser.add_argument("--license", help="License path(s) or port@server; defaults to environment/local files")
    parser.add_argument("--prepare-only", action="store_true", help="Generate RTL and constraints without synthesis")
    parser.add_argument("--generate-only", action="store_true", help="Generate RTL and manifest only; do not invoke synthesis or project creation")
    reuse = parser.add_mutually_exclusive_group()
    reuse.add_argument("--reuse-rtl", action="store_true", help="Reuse verified RTL in this build")
    reuse.add_argument("--rtl-from", type=Path, help="Copy verified RTL from another build with matching parameters")
    args = parser.parse_args()
    if args.control_queue_depth < 2 or args.control_queue_depth & (args.control_queue_depth - 1):
        parser.error("--control-queue-depth must be a power of two >= 2")
    args.name = args.name or (("baseline-replay" if args.tool == "quartus" else "baseline-replay-vivado")
                              if args.configuration == "replay" else
                              ("baseline" if args.tool == "quartus" else "baseline-vivado"))
    if args.pifo_backend == "stock" and args.name in (
            "baseline", "baseline-vivado", "baseline-replay", "baseline-replay-vivado"):
        args.name = "stock-pifo-replay" if args.configuration == "replay" else "stock-pifo"
        if args.tool == "vivado":
            args.name += "-vivado"
    if not re.fullmatch(r"[A-Za-z0-9_-]+", args.name):
        parser.error("--name may contain only letters, digits, underscores, and hyphens")
    if args.engines < 1 or args.vpifos < 2 or args.vpifos & (args.vpifos - 1):
        parser.error("--engines must be positive and --vpifos a power of two >= 2")
    if args.entries_per_pe < args.vpifos or args.entries_per_pe % args.vpifos:
        parser.error("--entries-per-pe must be a positive multiple of --vpifos")
    if args.entries_per_pe & (args.entries_per_pe - 1):
        parser.error("--entries-per-pe must be a power of two for the priority encoder")
    if (not 5 <= args.priority_bits <= 30 or not math.isfinite(args.clock_mhz)
            or args.clock_mhz <= 0 or args.threads < 1):
        parser.error("require 5 <= priority-bits <= 30, positive clock and thread count")
    if not re.fullmatch(r"[A-Za-z0-9_-]+", args.board):
        parser.error("--board must be an installed board directory name")
    root = (quartus_root(args.quartus_root) if args.tool == "quartus"
            else vivado_root(args.vivado_root))
    board = board_info(root) if args.tool == "quartus" else vivado_board_info(root, args.board)
    part = args.part or (board and board["part"])
    if not part:
        parser.error("No matching installed board definition; specify --part")
    build = args.build_root.resolve() / args.name
    build.mkdir(parents=True, exist_ok=True)
    env = os.environ.copy()
    env["QUARTUS_ROOTDIR" if args.tool == "quartus" else "XILINX_VIVADO"] = str(root)
    env["PATH"] = str(root / "bin") + os.pathsep + env["PATH"]
    if args.tool == "quartus":
        license_path = license_source(root, args.license)
        if license_path:
            env["LM_LICENSE_FILE"] = license_path
    else:
        license_path = args.license or env.get("XILINXD_LICENSE_FILE") or env.get("LM_LICENSE_FILE")
        if args.license:
            env["XILINXD_LICENSE_FILE"] = args.license
    v = (args.vpifos - 1).bit_length()
    e = args.engines.bit_length()
    token_bits = v + e
    pair_depth = 1 << (v + token_bits)
    hardware = {
        "configuration": args.configuration,
        "mapper_banks": 1 if args.configuration == "static" else 2,
        "pifo_backend": args.pifo_backend,
        "num_engines": args.engines, "num_vpifos_per_pe": args.vpifos,
        "global_flow_id_capacity": args.vpifos,
        "fifo_depth_parameter": args.entries_per_pe // args.vpifos,
        "shared_entries_per_pe": args.entries_per_pe,
        "total_shared_entries": args.engines * args.entries_per_pe,
        "priority_bits": args.priority_bits, "vpifo_id_bits": v,
        "engine_id_bits": e, "token_bits": token_bits,
        "brain_state_bits": 32, "flow_state_bits": 32, "commit_queue_depth": args.control_queue_depth,
        "flow_token_address_space": 1 << token_bits,
        "vpifo_token_pairs_per_pe": pair_depth,
    }
    if args.configuration == "replay":
        hardware.update(mapper_sync="shared_control_fifo_replay", replay_storage="control_queue",
                        separate_replay_journal=False, replay_max_retained_commands=args.control_queue_depth - 1)
    external_rtl = PROJECT / "hw/verilog" / (
        "pifo.sv" if args.pifo_backend == "stock" else "priority_encode_log.v")
    sources = sorted((PROJECT / "hw/spinal/rio").glob("*.scala")) + [
        external_rtl, PROJECT / "build.sbt",
        PROJECT / "project/build.properties", PROJECT / "project/plugins.sbt",
    ]
    source_hashes = {str(path.relative_to(PROJECT)): hashlib.sha256(path.read_bytes()).hexdigest()
                     for path in sources}
    manifest_path = build / "manifest.json"
    reuse_build = (args.rtl_from.resolve() if args.rtl_from else build) if (args.reuse_rtl or args.rtl_from) else None
    if reuse_build is not None:
        reuse_manifest = reuse_build / "manifest.json"
        previous = json.loads(reuse_manifest.read_text()) if reuse_manifest.exists() else {}
        if previous.get("hardware") != hardware or previous.get("source_sha256") != source_hashes:
            parser.error("RTL reuse requires a manifest with identical hardware parameters and sources")
        if not previous.get("rtl_complete"):
            parser.error("RTL reuse: previous RTL generation did not complete")
        if "PifoMesh.v" not in previous.get("rtl_sha256", {}):
            parser.error("RTL reuse: manifest has no PifoMesh.v hash")
        for filename, expected in previous["rtl_sha256"].items():
            if Path(filename).name != filename:
                parser.error("RTL reuse: expected plain generated filenames in the manifest")
            path = reuse_build / "rtl" / filename
            if not path.exists() or hashlib.sha256(path.read_bytes()).hexdigest() != expected:
                parser.error(f"RTL reuse: generated file changed or is missing: {filename}")
    metadata = {
        "timestamp_utc": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "tool": args.tool, f"{args.tool}_root": str(root), "part": part, "board_reference": board,
        "board_part_matches_target": bool(board and board["part"].lower() == part.lower()),
        "license_source": license_path, "hardware": hardware,
        "source_sha256": source_hashes,
        "rtl_complete": False,
        "clock_target_mhz": args.clock_mhz,
        "threads": args.threads,
        "scope": ("RIO only: all PEs, crossbar and configuration/commit controller; explicit external PIFO interfaces"
                  if args.pifo_backend == "external" else "PifoMesh, all PEs, crossbar, configuration/commit controller"),
        "synthesis_mode": "virtual data pins" if args.tool == "quartus" else "out_of_context",
        "implementation_run": False,
        "excluded": ["simulation request controller", "packet payload/per-flow packet queues",
                     "MAC/PCIe/DDR and board test system"] +
                    (["PIFO cores: storage, sorter, occupancy and drain detection"] if args.pifo_backend == "external" else []),
        "logical_storage_bits": {
            "sorted_entry_registers": args.engines * args.entries_per_pe * (v + args.priority_bits + token_bits) if args.pifo_backend != "external" else 0,
            "flow_state_tables": args.engines * pair_depth * 32,
            "post_mapper_banks": args.engines * pair_depth * token_bits * hardware["mapper_banks"],
            "pre_mapper_banks": args.engines * args.vpifos * v * hardware["mapper_banks"],
            "brain_policy_tables": args.engines * args.vpifos * 2,
            "last_virtual_time_tables": args.engines * args.vpifos * args.priority_bits,
            "brain_state_tables": args.engines * args.vpifos * 32,
            "front_rewrite_registers": args.engines * args.vpifos * (v + 4) if args.configuration != "static" else 0,
            "control_queue_payload": args.control_queue_depth * (3 + e + v + token_bits + 32),
            "crossbar_fifo_payload": (args.engines + 1) * 8 * token_bits,
        },
        "status": "preparing",
    }
    if reuse_build is not None:
        metadata["rtl_reused_from"] = str(reuse_build)
        metadata["rtl_source_manifest_sha256"] = hashlib.sha256(reuse_manifest.read_bytes()).hexdigest()
    metadata["workflow_sha256"] = {
        path.name: hashlib.sha256(path.read_bytes()).hexdigest()
        for path in (Path(__file__), HERE / ("create_project.tcl" if args.tool == "quartus"
                                            else "vivado_synth.tcl"))
    }
    started = time.monotonic()
    def save():
        metadata["elapsed_seconds"] = round(time.monotonic() - started, 3)
        manifest_path.write_text(json.dumps(metadata, indent=2) + "\n")
    save()
    try:
        if args.tool == "quartus":
            version_command = [root / "bin/quartus_sh", "--version"]
        else:
            version_command = [root / "bin/vivado", "-version"]
        version_file = build / f"{args.tool}-version.txt"
        run(version_command, build, version_file, env)
        metadata["tool_version"] = version_file.read_text().strip()
        if reuse_build is not None and reuse_build != build:
            rtl = build / "rtl"
            if rtl.exists():
                shutil.rmtree(rtl)
            rtl.mkdir()
            for filename in previous["rtl_sha256"]:
                shutil.copy2(reuse_build / "rtl" / filename, rtl / filename)
        elif reuse_build is None:
            rtl = build / "rtl"
            if rtl.exists():
                shutil.rmtree(rtl)
            # sbt parses its own command string; quote the path for its parser.
            command = (f'runMain rio.GeneratePifoMesh "{rtl}" {args.engines} '
                       f'{args.vpifos} {hardware["fifo_depth_parameter"]} {args.priority_bits} '
                       f'{args.pifo_backend} {args.configuration} {args.control_queue_depth}')
            run(["java", f"-XX:ActiveProcessorCount={args.threads}", "-Xmx4G",
                 f"-Dsbt.repository.config={HERE / 'repositories'}", "-Dsbt.override.build.repos=true",
                 "-Dsbt.supershell=false", "-Dsbt.log.noformat=true", "-jar", sbt_launcher(), command],
                PROJECT, build / "generate.log", env)
            # Snapshot the selected core dependency so the manifest covers exactly
            # what synthesis reads. The stock file includes its own encoder.
            shutil.copy2(external_rtl, rtl / external_rtl.name)
        metadata["rtl_sha256"] = {
            path.name: hashlib.sha256(path.read_bytes()).hexdigest()
            for path in sorted((build / "rtl").iterdir())
            if path.is_file() and path.suffix in (".v", ".sv", ".bin")
        }
        if "PifoMesh.v" not in metadata["rtl_sha256"]:
            raise ValueError("RTL generation did not produce PifoMesh.v")
        if reuse_build is not None and metadata["rtl_sha256"] != previous["rtl_sha256"]:
            raise ValueError("Copied RTL does not exactly match the source manifest")
        metadata["rtl_complete"] = True
        save()
        if args.generate_only:
            metadata["status"] = "generated"
            metadata["synthesis_run"] = False
            metadata["completed_utc"] = datetime.datetime.now(datetime.timezone.utc).isoformat()
            save()
            print(f'generated: {manifest_path}')
            return 0
        if args.tool == "quartus":
            quartus_rtl = build / "rtl"
            if args.quartus_compact_init:
                from quartus_compact_init import create_view
                metadata["quartus_initialization"] = create_view(build)
                quartus_rtl = build / "quartus-rtl"
            run([root / "bin/quartus_sh", "-t", HERE / "create_project.tcl", build, part,
                 str(args.threads), str(1000 / args.clock_mhz), quartus_rtl], build, build / "project.log", env)
            metadata["status"] = "prepared"
            save()
            if not args.prepare_only:
                metadata["status"] = "synthesis_running"
                save()
                run([root / "bin/quartus_syn", "pifo"], build, build / "synthesis.log", env)
                from summarize_quartus import summarize
                summarize(build / "output_files/pifo.syn.rpt")
                metadata["status"] = "synthesis_complete"
        else:
            marker = build / "synthesis_complete.txt"
            marker.unlink(missing_ok=True)
            metadata["vivado_max_threads"] = min(args.threads, 8)
            metadata["vivado_directive"] = args.vivado_directive
            metadata["vivado_capacity_check"] = "disabled_for_estimation" if args.vivado_allow_over_capacity else "enforced"
            metadata["status"] = "preparing" if args.prepare_only else "synthesis_running"
            save()
            run([root / "bin/vivado", "-mode", "batch", "-nojournal", "-log", build / "vivado.log",
                 "-source", HERE / "vivado_synth.tcl", "-tclargs", build, part,
                 str(metadata["vivado_max_threads"]), str(1000 / args.clock_mhz),
                 "1" if args.prepare_only else "0", args.vivado_directive,
                 "1" if args.vivado_allow_over_capacity else "0"],
                build, build / "synthesis.log", env)
            if not args.prepare_only and not marker.is_file():
                raise ValueError("Vivado exited without a successful synthesis/report completion marker")
            metadata["status"] = "prepared" if args.prepare_only else "synthesis_complete"
        metadata["completed_utc"] = datetime.datetime.now(datetime.timezone.utc).isoformat()
        save()
        print(f'{metadata["status"]}: {manifest_path}')
    except (subprocess.CalledProcessError, ValueError, OSError, KeyboardInterrupt) as error:
        metadata["status"] += "_failed"
        metadata["error"] = str(error) or type(error).__name__
        if isinstance(error, subprocess.CalledProcessError):
            metadata["failed_command"] = list(map(str, error.cmd))
            metadata["exit_code"] = error.returncode
        save()
        print(metadata["error"], file=sys.stderr)
        return error.returncode if isinstance(error, subprocess.CalledProcessError) else 1
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (ValueError, OSError) as error:
        sys.exit(str(error))
