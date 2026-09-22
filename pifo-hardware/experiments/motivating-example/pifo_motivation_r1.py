#!/usr/bin/env python3
import sys
from pathlib import Path

# Shared experiment helpers and hardware tools.
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import _paths

from pifo_motivation_common import case_main


if __name__ == "__main__":
    case_main("r1-add")
