"""Scope a Quartus M20K assignment to the controller instruction journal only."""
import re


def assignment(rtl, depth):
    matches = []
    for name, body in re.findall(r'^module (\w+)\b(.*?)^endmodule', rtl, re.M | re.S):
        if (re.search(rf'\breg\s+\[\d+:0\]\s+logic_ram\s+\[0:{depth - 1}\];', body)
                and 'io_push_payload_post' in body and 'io_push_payload_engineId' in body):
            matches.append(name)
    if len(matches) != 1:
        raise ValueError(f'Expected exactly one replay journal, found {matches}')
    return f'set_instance_assignment -name RAMSTYLE_ATTRIBUTE M20K -entity {matches[0]} -to logic_ram'
