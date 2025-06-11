from scapy.all import Ether, IP, wrpcap

a = "10:10:10:10:10:10"
b = "20:20:20:20:20:20"
c = "30:30:30:30:30:30"
d = "40:40:40:40:40:40"
e = "50:50:50:50:50:50"
f = "60:60:60:60:60:60"
g = "70:70:70:70:70:70"
dummy = "1:1:1:1:1:1"


def three_flows():
    packets = []
    srcs = [a, b, c] * 3 + [a]
    for src in srcs:
        packets += Ether(src=src, dst=dummy) / IP(src="1.1.1.1", dst="1.1.1.1")
    for i, p in enumerate(packets):
        p.time = i / 10
    return packets


def three_flows_bursty():
    packets = []
    srcs = [a, b, c] * 16 + [a, b]
    for src in srcs:
        packets += Ether(src=src, dst=dummy) / IP(src="1.1.1.1", dst="1.1.1.1")
    time = 0
    for i, p in enumerate(packets):
        p.time = (i / 10) + int(i / 5)
    return packets


def two_then_three():
    packets = []
    srcs = [b, c] * 7 + [a] * 6 + [a, b, c] * 10
    for src in srcs:
        packets += Ether(src=src, dst=dummy) / IP(src="1.1.1.1", dst="1.1.1.1")
    for i, p in enumerate(packets):
        p.time = i / 10
    return packets


def four_flows():
    packets = []
    srcs = [a, b, c, d] * 12 + [a, b]
    for src in srcs:
        packets += Ether(src=src, dst=dummy) / IP(src="1.1.1.1", dst="1.1.1.1")
    for i, p in enumerate(packets):
        p.time = i / 10
    return packets


def five_flows():
    packets = []
    srcs = [a, b, c, d, e] * 10
    for src in srcs:
        packets += Ether(src=src, dst=dummy) / IP(src="1.1.1.1", dst="1.1.1.1")
    for i, p in enumerate(packets):
        p.time = i / 10
    return packets


def seven_flows():
    packets = []
    srcs = [a, b, c, d, e, f, g] * 7 + [a]
    for src in srcs:
        packets += Ether(src=src, dst=dummy) / IP(src="1.1.1.1", dst="1.1.1.1")
    for i, p in enumerate(packets):
        p.time = i / 10
    return packets


def generate_pcaps():
    wrpcap("./pcaps/three_flows.pcap", three_flows())
    wrpcap("./pcaps/three_flows_bursty.pcap", three_flows_bursty())
    wrpcap("./pcaps/two_then_three.pcap", two_then_three())
    wrpcap("./pcaps/four_flows.pcap", four_flows())
    wrpcap("./pcaps/five_flows.pcap", five_flows())
    wrpcap("./pcaps/seven_flows.pcap", seven_flows())


if __name__ == "__main__":
    generate_pcaps()
