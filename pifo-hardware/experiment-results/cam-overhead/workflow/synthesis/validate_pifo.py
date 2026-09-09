#!/usr/bin/env python3
"""Check the generated PIFO plus adapter against a queue model with Vivado XSim.

The stock RTL is unmodified for Quartus. XSim requires three existing next-state
array declarations to precede their first use; only the simulation copy moves
these declarations. A missing pass marker or timeout is a failed validation.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import sys

from run import vivado_root


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("build", type=Path)
    parser.add_argument("--vivado-root")
    args = parser.parse_args()
    build = args.build.resolve()
    manifest = json.loads((build / "manifest.json").read_text())
    hardware = manifest["hardware"]
    backend = hardware.get("pifo_backend", "house")
    root = vivado_root(args.vivado_root)
    work = build / "functional"
    work.mkdir(exist_ok=True)
    source = build / "rtl" / ("pifo.sv" if backend == "stock" else "priority_encode_log.v")
    content = source.read_text()
    note = "No changes"
    if backend == "stock":
        declarations = ("reg [BITDATA-1:0] pf_data_nxt [0:NUMPIFO-1];\n"
                        "reg [BITPRIO-1:0] pf_prio_nxt [0:NUMPIFO-1];\n"
                        "reg [BITPORT-1:0] pf_port_nxt [0:NUMPIFO-1];")
        assert content.count(declarations) == 1
        content = content.replace(declarations, "")
        content = content.replace("reg [BITPIFO  :0] pf_cnt;",
                                  "reg [BITPIFO  :0] pf_cnt;\n" + declarations, 1)
        note = "Move pf_data_nxt/pf_prio_nxt/pf_port_nxt declarations before first use; no logic changes"
    sim_source = work / "core.sv"
    sim_source.write_text(content)
    definitions = {
        "PIFO_MODULE": "StockPifoRTL" if backend == "stock" else "ConcurrentPifoRTL",
        "PIFO_CAPACITY": hardware["shared_entries_per_pe"],
        "PIFO_PORT_BITS": hardware["vpifo_id_bits"],
        "PIFO_RANK_BITS": hardware["priority_bits"],
        "PIFO_DATA_BITS": hardware["token_bits"],
    }
    bench = Path(__file__).with_name("pifo_contract_tb.sv")
    tb = work / "pifo_contract_tb.sv"
    tb.write_text("".join(f"`define {key} {value}\n" for key, value in definitions.items()) + bench.read_text())
    result = {
        "backend": backend, "hardware": hardware, "simulator": str(root / "bin/xsim"),
        "source_sha256": hashlib.sha256(source.read_bytes()).hexdigest(),
        "simulation_source_sha256": hashlib.sha256(sim_source.read_bytes()).hexdigest(),
        "testbench_sha256": hashlib.sha256(bench.read_bytes()).hexdigest(),
        "simulation_only_change": note, "status": "running",
    }
    try:
        commands = [
            ("compile", [root / "bin/xvlog", "-sv", build / "rtl/PifoMesh.v", sim_source, tb]),
            ("elaborate", [root / "bin/xelab", "pifo_contract_tb", "-s", "pifo_contract", "--mt", "4"]),
            ("simulate", [root / "bin/xsim", "pifo_contract", "-runall"]),
        ]
        for stage, command in commands:
            print(f"{backend}: {stage}; {work / (stage + '.log')}", flush=True)
            result["stage"] = stage
            with (work / f"{stage}.log").open("w") as log:
                subprocess.run(command, cwd=work, stdout=log, stderr=subprocess.STDOUT,
                               check=True, timeout=180)
        log = (work / "simulate.log").read_text()
        result["status"] = "passed" if "PIFO_CONTRACT_PASS cycles=" in log and "CONTRACT_FAIL" not in log else "failed"
        result["evidence"] = [line for line in log.splitlines()
                              if "CONTRACT_" in line or "Fatal:" in line]
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as error:
        result["status"] = "failed"
        result["error"] = str(error)
    (work / "validation.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2))
    return 0 if result["status"] == "passed" else 1


if __name__ == "__main__":
    sys.exit(main())
