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
 * Forward Switch:
 * 
 * The forward design exemplifies the implementation the core of an IPv4/IPv6
 * network switch. IP destination address is used to perform an LPM search to 
 * determine the port where the packet needs to be redirected to. The IPv6 
 * table is setup to be implemented with an ternary CAM and the IPv4 table 
 * with a semi-ternary CAM.
 *
 */

typedef bit<48>  MacAddr;
typedef bit<32>  IPv4Addr;
typedef bit<128> IPv6Addr;

const bit<16> VLAN_TYPE  = 0x8100;
const bit<16> IPV4_TYPE  = 0x0800;
const bit<16> IPV6_TYPE  = 0x86DD;

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

header ipv6_t {
    bit<4>   version;    // Version = 6
    bit<8>   priority;   // Traffic class
    bit<20>  flow_label; // Flow label
    bit<16>  length;     // Payload length
    bit<8>   protocol;   // Next protocol
    bit<8>   hop_limit;  // Hop limit
    IPv6Addr src;        // Source address
    IPv6Addr dst;        // Destination address
}

// ****************************************************************************** //
// ************************* S T R U C T U R E S  ******************************* //
// ****************************************************************************** //

// header structure
struct headers {
    eth_mac_t    eth;
    vlan_t[4]    vlan;
    ipv4_t       ipv4;
    ipv4_opt_t   ipv4opt;
    ipv6_t       ipv6;
}

// User metadata structure
struct metadata {
    bit<9> port;
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
            IPV6_TYPE : parse_ipv6;
            default   : accept; 
        }
    }
    
    state parse_vlan {
        packet.extract(hdr.vlan.next);
        transition select(hdr.vlan.last.tpid) {
            VLAN_TYPE : parse_vlan;
            IPV4_TYPE : parse_ipv4;
            IPV6_TYPE : parse_ipv6;
            default   : accept; 
        }
    }
    
    state parse_ipv4 {
        packet.extract(hdr.ipv4);
        verify(hdr.ipv4.version == 4 && hdr.ipv4.hdr_len >= 5, error.InvalidIPpacket);
        packet.extract(hdr.ipv4opt, (((bit<32>)hdr.ipv4.hdr_len - 5) * 32));
        transition accept;
    }
    
    state parse_ipv6 {
        packet.extract(hdr.ipv6);
        verify(hdr.ipv6.version == 6, error.InvalidIPpacket);
        transition accept;
    }
}

// ****************************************************************************** //
// **************************  P R O C E S S I N G   **************************** //
// ****************************************************************************** //

control MyProcessing(inout headers hdr, 
                     inout metadata meta, 
                     inout standard_metadata_t smeta) {

    bit<16> ipv4_hdr_chk = 0;
    bit<8>  ipv4_ttl = 0;

    Checksum<bit<16>>(HashAlgorithm_t.ONES_COMPLEMENT16) IPv4ChkVerify;
    Checksum<bit<16>>(HashAlgorithm_t.ONES_COMPLEMENT16) IPv4ChkUpdate;
             
    action forwardPacket(bit<9> port) {
        meta.port = port;
    }
    
    action dropPacket() {
        smeta.drop = 1;
    }

    table forwardIPv4 {
        key             = { hdr.ipv4.dst : lpm; }
        actions         = { forwardPacket; 
                            NoAction; }
#if defined(USE_TINY_CAM)
        size            = 32;
#else
        size            = 1024;
        num_masks       = 64;
#endif
        default_action  = NoAction;
    }

    table forwardIPv6 {
        key             = { hdr.ipv6.dst : lpm; }
        actions         = { forwardPacket; 
                            NoAction; }
#if defined(USE_TINY_CAM)
        size            = 32;
#else
        size            = 1024;
#endif
        default_action  = NoAction;
    }

    apply {
        
        if (smeta.parser_error == error.NoError) {

           /*
            * IPv4 forwarding.
            * NOTE1: IPv4 processing requires checksum verification and
            *        update after updating ttl field value.
            * NOTE2: As per described in RFC-1624 (https://www.rfc-editor.org/rfc/rfc1624)
            *        the checksum can be updated without recomputing 
            *        all IPv4 fields.
            */
            if (hdr.ipv4.isValid()) {
    
                IPv4ChkVerify.apply(
                    { 
                        hdr.ipv4.version,
                        hdr.ipv4.hdr_len,
                        hdr.ipv4.tos,   
                        hdr.ipv4.length,
                        hdr.ipv4.id,
                        hdr.ipv4.flags,
                        hdr.ipv4.offset,
                        hdr.ipv4.ttl,
                        hdr.ipv4.protocol,
                        hdr.ipv4.src,
                        hdr.ipv4.dst 
                    }, 
                    ipv4_hdr_chk
                );
    
                if (ipv4_hdr_chk == hdr.ipv4.hdr_chk && hdr.ipv4.ttl > 0) {
                    if (forwardIPv4.apply().hit) {
                        ipv4_ttl = hdr.ipv4.ttl - 1;
                        IPv4ChkUpdate.apply({~hdr.ipv4.hdr_chk, ~hdr.ipv4.ttl, ipv4_ttl}, ipv4_hdr_chk);
                        hdr.ipv4.hdr_chk = ~ipv4_hdr_chk;
                        hdr.ipv4.ttl = ipv4_ttl;
                        return;
                    }
                }
            
           /*
            * IPv6 Forwarding.
            * NOTE: IPv6 processing is less compute intense since
            *       it does not require to update the checksum after 
            *       decreasing the hop limit field value.
            */
            } else if (hdr.ipv6.isValid()) {
                if (hdr.ipv6.hop_limit > 0) {
                    if (forwardIPv6.apply().hit) {
                        hdr.ipv6.hop_limit = hdr.ipv6.hop_limit - 1;
                        return;
                    }
                }
            } 
        } 

       /* 
        * Drop invalid packets. Including:
        *   - Parsing errors as per defined in the parser
        *   - Non-IP or incomplete packets
        *   - IPv4 packets with invalid checksum
        *   - IPv4/6 packets with null hop limit
        *   - IPv4/6 packets with unsupported destination address
        */
        dropPacket();
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
        packet.emit(hdr.ipv6);
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
