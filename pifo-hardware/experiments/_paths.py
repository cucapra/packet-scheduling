"""Shared paths and subprocess invocation for experiment scripts."""

import subprocess
import sys
from pathlib import Path

HARDWARE_ROOT = Path(__file__).resolve().parent.parent
CORE_PYTHON = HARDWARE_ROOT / "hw/python"
sys.path.insert(0, str(CORE_PYTHON))


def run_script(script: Path, *arguments: object) -> None:
    subprocess.run(
        [sys.executable, str(script), *(str(argument) for argument in arguments)],
        cwd=HARDWARE_ROOT,
        check=True,
    )
