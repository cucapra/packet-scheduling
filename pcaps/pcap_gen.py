import os
from scapy.all import Ether, Raw, wrpcap

a = "10:10:10:10:10:10"
b = "20:20:20:20:20:20"
c = "30:30:30:30:30:30"
d = "40:40:40:40:40:40"
e = "50:50:50:50:50:50"
f = "60:60:60:60:60:60"
g = "70:70:70:70:70:70"
dummy = "1:1:1:1:1:1"


def append_path_prefix(file):
    path_to_script = os.path.dirname(__file__)
    path_to_file = os.path.join(path_to_script, file)
    return path_to_file


def three_flows():
    packets = []
    srcs = [a, b, c] * 3 + [a]
    for i, src in enumerate(srcs):
        pkt = Ether(src=src, dst=dummy)
        pkt.time = i / 10
        packets += pkt
    return packets


def three_flows_bursty():
    packets = []
    srcs = [a, b, c] * 16 + [a, b]
    for i, src in enumerate(srcs):
        pkt = Ether(src=src, dst=dummy)
        pkt.time = (i / 10) + int(i / 5)
        packets += pkt
    return packets


def two_then_three():
    packets = []
    srcs = [b, c] * 7 + [a] * 6 + [a, b, c] * 10
    for i, src in enumerate(srcs):
        pkt = Ether(src=src, dst=dummy)
        pkt.time = i / 10
        packets += pkt
    return packets


def four_flows():
    packets = []
    srcs = [a, b, c, d] * 12 + [a, b]
    for i, src in enumerate(srcs):
        pkt = Ether(src=src, dst=dummy)
        pkt.time = i / 10
        packets += pkt
    return packets


def five_flows():
    packets = []
    srcs = [a, b, c, d, e] * 10
    for i, src in enumerate(srcs):
        pkt = Ether(src=src, dst=dummy)
        pkt.time = i / 10
        packets += pkt
    return packets


def seven_flows():
    packets = []
    srcs = [a, b, c, d, e, f, g] * 7 + [a]
    for i, src in enumerate(srcs):
        pkt = Ether(src=src, dst=dummy)
        pkt.time = i / 10
        packets += pkt
    return packets


def generate_pcaps():
    pcaps = [
        ("three_flows.pcap", three_flows()),
        ("three_flows_bursty.pcap", three_flows_bursty()),
        ("two_then_three.pcap", two_then_three()),
        ("four_flows.pcap", four_flows()),
        ("five_flows.pcap", five_flows()),
        ("seven_flows.pcap", seven_flows())
    ]

    for name, pcap in pcaps:
        name = append_path_prefix(name)
        wrpcap(name, pcap)
        print(f"Generated {name}")


if __name__ == "__main__":
    generate_pcaps()
