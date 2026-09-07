#!/usr/bin/env python3
"""Synthesize an isolated lookup at two latencies using the R1/R2 tool settings."""
import argparse
import csv
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

from pifo_hardware_overhead import PROJECT, RESOURCE_NAMES, archive, resources, count_text
sys.path.insert(0, str(PROJECT / "synthesis"))
from run import quartus_root, vivado_root, board_info, vivado_board_info, run
from quartus_compact_init import create_view


def write_csv(path, rows, fields):
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def rtl(address_bits, data_bits, latency):
    depth = 1 << address_bits
    return f'''`timescale 1ns/1ps
// Isolated ordinary lookup; throughput one request/cycle, latency {latency} cycles.
module PifoMesh (
  input wire clk, input wire reset,
  input wire write_valid, input wire [{address_bits-1}:0] write_address,
  input wire [{data_bits-1}:0] write_data,
  input wire read_valid, input wire [{address_bits-1}:0] read_address,
  output wire response_valid, output wire [{data_bits-1}:0] response_data
);
  reg [{data_bits-1}:0] ram [0:{depth-1}];
  reg [{data_bits-1}:0] data_pipe [0:{latency-1}];
  reg [{latency-1}:0] valid_pipe;
  initial begin
    $readmemb("lookup-zero.bin",ram);
  end
  always @(posedge clk) begin
    if (write_valid) ram[write_address] <= write_data;
    if (read_valid) data_pipe[0] <= ram[read_address];
    for (integer k=1; k<{latency}; k=k+1) data_pipe[k] <= data_pipe[k-1];
    if (reset) valid_pipe <= 0;
    else begin
      valid_pipe[0] <= read_valid;
      for (integer k=1; k<{latency}; k=k+1) valid_pipe[k] <= valid_pipe[k-1];
    end
  end
  assign response_valid = valid_pipe[{latency-1}];
  assign response_data = data_pipe[{latency-1}];
endmodule
'''


def execute_case(config, platform, latency, root, output):
    name = f"lookup-a{config['address_bits']}-d{config['data_bits']}-l{latency}-{platform}"
    build = root / name
    (build / "rtl").mkdir(parents=True, exist_ok=True)
    source = build / "rtl/PifoMesh.v"
    source.write_text(rtl(config["address_bits"], config["data_bits"], latency))
    binary = build / "rtl/lookup-zero.bin"
    binary.write_bytes((b"0" * config["data_bits"] + b"\n") * (1 << config["address_bits"]))
    tool = quartus_root(None) if platform == "quartus" else vivado_root(None)
    board = board_info(tool) if platform == "quartus" else vivado_board_info(tool, "kcu116")
    env = os.environ.copy()
    env["PATH"] = str(tool / "bin") + os.pathsep + env["PATH"]
    if platform == "quartus":
        env["QUARTUS_ROOTDIR"] = str(tool)
        env["LM_LICENSE_FILE"] = "/data/work/quartus/licenses/LR-187458_License.dat"
    manifest = {"status": "preparing", "tool": platform, "part": board["part"],
                "board_reference": board, "scope": config["scope"],
                "clock_target_mhz": config["clock_mhz"], "threads": config["threads"],
                "synthesis_mode": "virtual data pins" if platform == "quartus" else "out_of_context",
                "implementation_run": False, "vivado_directive": "RuntimeOptimized",
                "vivado_capacity_check": "enforced", "source_sha256": {
                    str(Path(__file__).relative_to(PROJECT)): hashlib.sha256(Path(__file__).read_bytes()).hexdigest()},
                "hardware": {"component": "ordinary_lookup", "address_bits": config["address_bits"],
                             "data_bits": config["data_bits"], "read_latency_cycles": latency,
                             "initiation_interval_cycles": 1},
                "rtl_sha256": {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in (source, binary)}}
    mp = build / "manifest.json"
    try:
        command = [tool / ("bin/quartus_sh" if platform == "quartus" else "bin/vivado"),
                   "--version" if platform == "quartus" else "-version"]
        version = build / f"{platform}-version.txt"
        run(command, build, version, env)
        manifest["tool_version"] = version.read_text().strip()
        if platform == "quartus":
            manifest["quartus_initialization"] = create_view(build)
            run([tool / "bin/quartus_sh", "-t", PROJECT / "synthesis/create_project.tcl", build,
                 board["part"], str(config["threads"]), str(1000 / config["clock_mhz"]), build / "quartus-rtl"],
                build, build / "project.log", env)
        manifest["status"] = "synthesis_running"
        mp.write_text(json.dumps(manifest, indent=2) + "\n")
        if platform == "quartus":
            command = [tool / "bin/quartus_syn", "pifo"]
        else:
            command = [tool / "bin/vivado", "-mode", "batch", "-nojournal", "-log", build / "vivado.log",
                       "-source", PROJECT / "synthesis/vivado_synth.tcl", "-tclargs", build, board["part"],
                       str(config["threads"]), str(1000 / config["clock_mhz"]), "0", "RuntimeOptimized", "0"]
        run(command, build, build / "synthesis.log", env)
        manifest["status"] = "synthesis_complete"
        mp.write_text(json.dumps(manifest, indent=2) + "\n")
        resources(build, platform)
    except (subprocess.CalledProcessError, OSError, ValueError) as error:
        manifest["status"] = "synthesis_failed"
        manifest["error"] = str(error)
    mp.write_text(json.dumps(manifest, indent=2) + "\n")
    result = archive(build, output / "runs" / name, platform)
    shutil.copy2(source, output / "runs" / name / "lookup.v")
    return {"latency_cycles": latency, **result}


def validate(config, root, output):
    work = root / "validation"
    work.mkdir(parents=True, exist_ok=True)
    short, long = min(config["read_latencies"]), max(config["read_latencies"])
    a, d = config["address_bits"], config["data_bits"]
    for name, latency in (("LookupShort", short), ("LookupLong", long)):
        (work / f"{name}.v").write_text(rtl(a, d, latency).replace("module PifoMesh", f"module {name}"))
    (work / "lookup-zero.bin").write_bytes((b"0" * d + b"\n") * (1 << a))
    source = PROJECT / "synthesis/lookup_pipeline_tb.sv"
    (work / source.name).write_text(f"`define LOOKUP_ADDRESS_BITS {a}\n`define LOOKUP_DATA_BITS {d}\n"
                                  f"`define LOOKUP_SHORT_LATENCY {short}\n`define LOOKUP_LONG_LATENCY {long}\n" + source.read_text())
    tool = vivado_root(None)
    commands = [("compile", [tool / "bin/xvlog", "-sv", "LookupShort.v", "LookupLong.v", source.name]),
                ("elaborate", [tool / "bin/xelab", "lookup_pipeline_tb", "-s", "lookup_pipeline", "--mt", "4"]),
                ("simulate", [tool / "bin/xsim", "lookup_pipeline", "-runall"])]
    for stage, command in commands:
        with (work / f"{stage}.log").open("w") as stream:
            subprocess.run(command, cwd=work, stdout=stream, stderr=subprocess.STDOUT, check=True, timeout=120)
    log = (work / "simulate.log").read_text()
    assert "LOOKUP_PIPELINE_PASS" in log and "LOOKUP_PIPELINE_FAIL" not in log
    record = {"status": "passed", "evidence": [line for line in log.splitlines() if "LOOKUP_PIPELINE_" in line],
              "source_sha256": {p.name: hashlib.sha256(p.read_bytes()).hexdigest()
                                for p in work.iterdir() if p.suffix in (".v", ".sv", ".bin")}}
    (work / "validation.json").write_text(json.dumps(record, indent=2) + "\n")
    destination = output / "validation"
    destination.mkdir(parents=True, exist_ok=True)
    for name in ("compile.log", "elaborate.log", "simulate.log", "validation.json", "LookupShort.v", "LookupLong.v", source.name):
        shutil.copy2(work / name, destination / name)


def render(output, config):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    plt.rcParams.update({"svg.fonttype": "none", "svg.hashsalt": "rio-lookup-pipeline-v1"})
    results = json.loads((output / "run-status.json").read_text())
    short, long = min(config["read_latencies"]), max(config["read_latencies"])
    comparisons = []
    for platform in config["platforms"]:
        pair = {r["latency_cycles"]: r["resources"] for r in results
                if r["platform"] == platform and r["status"] == "synthesis_complete"}
        if short not in pair or long not in pair:
            continue
        for resource, baseline in pair[short].items():
            pipelined = pair[long][resource]
            comparisons.append({"platform": platform, "resource": resource,
                                "short_latency": short, "long_latency": long,
                                "short_resource": baseline, "long_resource": pipelined,
                                "absolute_change": pipelined - baseline})
    fields = ["platform", "resource", "short_latency", "long_latency", "short_resource", "long_resource", "absolute_change"]
    write_csv(output / "comparison.csv", comparisons, fields)
    cells = []
    for row in comparisons:
        values = [row["platform"].capitalize(), RESOURCE_NAMES[row["resource"]][0],
                  count_text(row["short_resource"]), count_text(row["long_resource"]),
                  count_text(row["absolute_change"], True)]
        if row["resource"] not in ("mlab_memory_bits", "dsp_blocks", "uram288_blocks", "lutram_luts"):
            cells.append(values)
    (output / "report.md").unlink(missing_ok=True)
    folder = output / "figures/resource-table"
    folder.mkdir(parents=True, exist_ok=True)
    shutil.copy2(output / "comparison.csv", folder / "data.csv")
    fig, ax = plt.subplots(figsize=(12, max(3, 1.7 + .38 * len(cells))))
    ax.axis("off")
    if cells:
        table = ax.table(cellText=cells, colLabels=["Platform", "Resource", f"{short} cycle(s)", f"{long} cycles", "Difference"],
                         colWidths=[.12, .34, .18, .18, .18], cellLoc="right", loc="center")
        table.auto_set_font_size(False); table.set_fontsize(10); table.scale(1, 1.7)
        for (r, c), cell in table.get_celld().items():
            cell.set_edgecolor("#d1d5db")
            if r == 0: cell.set_facecolor("#17324d"); cell.set_text_props(color="white", weight="bold")
            elif r % 2 == 0: cell.set_facecolor("#f0f4f8")
            if c < 2: cell.set_text_props(ha="left")
    ax.set_title(f'Isolated lookup pipeline exploration\n{1 << config["address_bits"]:,} words × {config["data_bits"]} bits · one request per cycle', pad=22, fontweight="bold")
    fig.text(.02, .01, "Synthesis only. Separate component probe; pipeline latency is not integrated into RIO.", fontsize=9)
    for suffix in ("png", "svg"):
        fig.savefig(folder / f"figure.{suffix}", dpi=180, bbox_inches="tight",
                    metadata={"Date": None} if suffix == "svg" else None)
    svg = folder / "figure.svg"
    svg.write_text("\n".join(line.rstrip() for line in svg.read_text().splitlines()) + "\n")
    plt.close(fig)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", type=Path, default=PROJECT / "experiments/hardware-overhead/r3-lookup-pipeline.json")
    parser.add_argument("--build-root", type=Path, default=PROJECT / "synthesis/build/lookup-pipeline")
    parser.add_argument("--render-only", action="store_true")
    args = parser.parse_args()
    config = json.loads(args.config.read_text())
    assert config["schema"] == "rio-lookup-pipeline-v1"
    root = args.build_root.resolve()
    output = PROJECT / config["output_dir"]
    output.mkdir(parents=True, exist_ok=True)
    if args.render_only:
        render(output, json.loads((output / "experiment-config.json").read_text()))
        return
    (output / "experiment-config.json").write_text(json.dumps(config, indent=2) + "\n")
    (output / "execution.json").write_text(json.dumps({"build_root": str(root), "argv": sys.argv}, indent=2) + "\n")
    validate(config, root, output)
    with ThreadPoolExecutor(max_workers=4) as pool:
        futures = [pool.submit(execute_case, config, platform, latency, root, output)
                   for platform in config["platforms"] for latency in config["read_latencies"]]
        results = [f.result() for f in futures]
    (output / "run-status.json").write_text(json.dumps(results, indent=2) + "\n")
    rows = [{"platform": r["platform"], "latency_cycles": r["latency_cycles"], "resource": key, "value": value}
            for r in results for key, value in r["resources"].items()]
    write_csv(output / "resources.csv", rows, ["platform", "latency_cycles", "resource", "value"])
    render(output, config)
    if any(r["status"] != "synthesis_complete" for r in results):
        raise SystemExit("Incomplete lookup probe; see archived logs")
    print(output)


if __name__ == "__main__":
    main()
