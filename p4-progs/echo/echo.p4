// (c) Copyright 2023 Advanced Micro Devices, Inc. All rights reserved.
//
// This file contains confidential and proprietary information
// of AMD and is protected under U.S. and international copyright
// and other intellectual property laws.
//
// DISCLAIMER
// This disclaimer is not a license and does not grant any
// rights to the materials distributed herewith. Except as
// otherwise provided in a valid license issued to you by
// AMD, and to the maximum extent permitted by applicable
// law: (1) THESE MATERIALS ARE MADE AVAILABLE "AS IS" AND
// WITH ALL FAULTS, AND AMD HEREBY DISCLAIMS ALL WARRANTIES
// AND CONDITIONS, EXPRESS, IMPLIED, OR STATUTORY, INCLUDING
// BUT NOT LIMITED TO WARRANTIES OF MERCHANTABILITY, NON-
// INFRINGEMENT, OR FITNESS FOR ANY PARTICULAR PURPOSE; and
// (2) AMD shall not be liable (whether in contract or tort,
// including negligence, or under any other theory of
// liability) for any loss or damage of any kind or nature
// related to, arising under or in connection with these
// materials, including for any direct, or any indirect,
// special, incidental, or consequential loss or damage
// (including loss of data, profits, goodwill, or any type of
// loss or damage suffered as a result of any action brought
// by a third party) even if such damage or loss was
// reasonably foreseeable or AMD had been advised of the
// possibility of the same.
//
// CRITICAL APPLICATIONS
// AMD products are not designed or intended to be fail-
// safe, or for use in any application requiring fail-safe
// performance, such as life-support or safety devices or
// systems, Class III medical devices, nuclear facilities,
// applications related to the deployment of airbags, or any
// other applications that could lead to death, personal
// injury, or severe property or environmental damage
// (individually and collectively, "Critical
// Applications"). Customer assumes the sole risk and
// liability of any use of AMD products in Critical
// Applications, subject only to applicable laws and
// regulations governing limitations on product liability.
//
// THIS COPYRIGHT NOTICE AND DISCLAIMER MUST BE RETAINED AS
// PART OF THIS FILE AT ALL TIMES.
////////////////////////////////////////////////////////////

#include <core.p4>
#include <xsa.p4>

/*
 * UDP Echo Server:
 *
 * The Echo example is like a UDP echo server. It does nothing more than sending 
 * back whatever packets are sent to it, no control plane software is required. 
 * The UDP port is setup to listen to port specified with metadata field echo_port. 
 * Packets containing a different UDP destination port will remain unmodified. 
 *
 */

typedef bit<48>  MacAddr;
typedef bit<32>  IPv4Addr;
typedef bit<16>  UdpPort;

const bit<16> VLAN_TYPE = 0x8100;
const bit<16> IPV4_TYPE = 0x0800;
const bit<8>  UDP_PROT  = 0x11;

// ****************************************************************************** //
// *************************** H E A D E R S  *********************************** //
// ****************************************************************************** //

header eth_mac_t {
    MacAddr dmac; // Destination MAC address
    MacAddr smac; // Source MAC address
    bit<16> type; // Tag Protocol Identifier
}

header vlan_t {
    bit<3>  pcp;  // Priority code point
    bit<1>  cfi;  // Drop eligible indicator
    bit<12> vid;  // VLAN identifier
    bit<16> tpid; // Tag protocol identifier
}

header ipv4_t {
    bit<4>   version;  // Version (4 for IPv4)
    bit<4>   hdr_len;  // Header length in 32b words
    bit<8>   tos;      // Type of Service
    bit<16>  length;   // Packet length in 32b words
    bit<16>  id;       // Identification
    bit<3>   flags;    // Flags
    bit<13>  offset;   // Fragment offset
    bit<8>   ttl;      // Time to live
    bit<8>   protocol; // Next protocol
    bit<16>  hdr_chk;  // Header checksum
    IPv4Addr src;      // Source address
    IPv4Addr dst;      // Destination address
}

header ipv4_opt_t {
    varbit<320> options; // IPv4 options - length = (ipv4.hdr_len - 5) * 32
}

header udp_t {
    UdpPort src_port;  // Source port
    UdpPort dst_port;  // Destination port
    bit<16> length;    // UDP length
    bit<16> checksum;  // UDP checksum
}

// ****************************************************************************** //
// ************************* S T R U C T U R E S  ******************************* //
// ****************************************************************************** //

// header structure
struct headers {
    eth_mac_t    eth;
    vlan_t[2]    vlan;
    ipv4_t       ipv4;
    ipv4_opt_t   ipv4opt;
    udp_t        udp;
}

// User metadata structure
struct metadata {
    UdpPort echo_port;
}

// User-defined errors 
error {
    InvalidIPpacket
}

// ****************************************************************************** //
// *************************** P A R S E R  ************************************* //
// ****************************************************************************** //

parser MyParser(packet_in packet, 
                out headers hdr, 
                inout metadata meta, 
                inout standard_metadata_t smeta) {
    
    state start {
        transition parse_eth;
    }
    
    state parse_eth {
        packet.extract(hdr.eth);
        transition select(hdr.eth.type) {
            VLAN_TYPE : parse_vlan;
            IPV4_TYPE : parse_ipv4;
            default   : accept; 
        }
    }
    
    state parse_vlan {
        packet.extract(hdr.vlan.next);
        transition select(hdr.vlan.last.tpid) {
            VLAN_TYPE : parse_vlan;
            IPV4_TYPE : parse_ipv4;
            default   : accept; 
        }
    }
    
    state parse_ipv4 {
        packet.extract(hdr.ipv4);
        verify(hdr.ipv4.version == 4 && hdr.ipv4.hdr_len >= 5, error.InvalidIPpacket);
        packet.extract(hdr.ipv4opt, (((bit<32>)hdr.ipv4.hdr_len - 5) * 32));
        transition select(hdr.ipv4.protocol) {
            UDP_PROT  : parse_udp;
            default   : accept; 
        }
    }

    state parse_udp {
        packet.extract(hdr.udp);
        transition accept;
    }
}

// ****************************************************************************** //
// **************************  P R O C E S S I N G   **************************** //
// ****************************************************************************** //

control MyProcessing(inout headers hdr, 
                     inout metadata meta, 
                     inout standard_metadata_t smeta) {

    MacAddr  tmp_eth_addr;
    IPv4Addr tmp_ip_addr;
    UdpPort  tmp_udp_port;
    
    action swap_eth_address() {
        tmp_eth_addr = hdr.eth.dmac;
        hdr.eth.dmac = hdr.eth.smac;
        hdr.eth.smac = tmp_eth_addr;
    }
    
    action swap_ip_address() {
        tmp_ip_addr  = hdr.ipv4.dst;
        hdr.ipv4.dst = hdr.ipv4.src;
        hdr.ipv4.src = tmp_ip_addr;
    }
    
    action swap_udp_address() {
        tmp_udp_port     = hdr.udp.dst_port;
        hdr.udp.dst_port = hdr.udp.src_port;
        hdr.udp.src_port = tmp_udp_port;
    }
    
    action echo_packet() {
        swap_eth_address();
        swap_ip_address();
        swap_udp_address();  
    }

    apply {
        if (hdr.udp.isValid()) {
            if (hdr.udp.dst_port == meta.echo_port) {
                echo_packet();
            }
        }
    }
} 

// ****************************************************************************** //
// ***************************  D E P A R S E R  ******************************** //
// ****************************************************************************** //

control MyDeparser(packet_out packet, 
                   in headers hdr,
                   inout metadata meta, 
                   inout standard_metadata_t smeta) {
    apply {
        packet.emit(hdr.eth);
        packet.emit(hdr.vlan);
        packet.emit(hdr.ipv4);
        packet.emit(hdr.ipv4opt);
        packet.emit(hdr.udp);
    }
}

// ****************************************************************************** //
// *******************************  M A I N  ************************************ //
// ****************************************************************************** //

XilinxPipeline(
    MyParser(), 
    MyProcessing(), 
    MyDeparser()
) main;
