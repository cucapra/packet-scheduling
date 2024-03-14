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
 * P4 Advanced Calculator:
 *
 * Same behavior as the calculator example but with two additional 
 * operations implemented with user externs: arithmetic division and square-root. 
 *
 */

const bit<16> P4CALC_ETYPE = 0x1234; // custom value
const bit<8>  P4CALC_P     = 0x50;   // 'P'
const bit<8>  P4CALC_4     = 0x34;   // '4'
const bit<8>  P4CALC_VER   = 0x01;   // v0.1

// ****************************************************************************** //
// *************************** H E A D E R S  *********************************** //
// ****************************************************************************** //

header eth_mac_t {
    bit<48> dstAddr;     // Destination MAC address
    bit<48> srcAddr;     // Source MAC address
    bit<16> etherType;   // Tag Protocol Identifier
}

header p4calc_t {
    bit<8>  p;           // P is an ASCII Letter 'P'
    bit<8>  four;        // 4 is an ASCII Letter '4'
    bit<8>  version;     // Version is currently 0.1
    bit<8>  operation;   // Op is an operation to Perform
    bit<32> operand_a;   // A operand (left)
    bit<32> operand_b;   // B operand (right)
    bit<32> result;      // Result of the operation
}

// ****************************************************************************** //
// ************************* S T R U C T U R E S  ******************************* //
// ****************************************************************************** //

struct divider_input {
    bit<32> divisor;
    bit<32> dividend;
}

struct divider_output {
    bit<32> remainder;
    bit<32> quotient;
}

// header structure
struct headers {
    eth_mac_t    eth;
    p4calc_t     p4calc;
}

// User metadata structure
struct metadata {
    // empty
}

// ****************************************************************************** //
// *************************** P A R S E R  ************************************* //
// ****************************************************************************** //

parser MyParser(packet_in packet, 
                out headers hdr, 
                inout metadata meta, 
                inout standard_metadata_t smeta) {
    
    state start {
        packet.extract(hdr.eth);
        transition select(hdr.eth.etherType) {
            P4CALC_ETYPE : parse_p4calc;
            default      : drop;
        }
    }

    state parse_p4calc {
        packet.extract(hdr.p4calc);
        transition select(hdr.p4calc.p, hdr.p4calc.four, hdr.p4calc.version) {
            (P4CALC_P, P4CALC_4, P4CALC_VER) : accept;
            default                          : drop;
        }
    }
    
    state drop {
        smeta.drop = 1;
        transition accept;
    }
}

// ****************************************************************************** //
// **************************  P R O C E S S I N G   **************************** //
// ****************************************************************************** //

control MyProcessing(inout headers hdr, 
                     inout metadata meta, 
                     inout standard_metadata_t smeta) {
                        
    UserExtern<divider_input, divider_output>(34) calc_divide;
    UserExtern<bit<32>, bit<24>>(17) calc_square_root;
    
    divider_input div_in;
    divider_output div_out;
    
    bit<24> square_root_result;
      
    action send_back(bit<32> result) {
        bit<48> tmp       = hdr.eth.dstAddr;
        hdr.eth.dstAddr   = hdr.eth.srcAddr;
        hdr.eth.srcAddr   = tmp;
        hdr.p4calc.result = result;
    }
    
    action operation_add() {
        send_back(hdr.p4calc.operand_a + hdr.p4calc.operand_b);
    }
    
    action operation_sub() {
        send_back(hdr.p4calc.operand_a - hdr.p4calc.operand_b);
    }
    
    action operation_mult() {
        send_back(hdr.p4calc.operand_a * hdr.p4calc.operand_b);
    }
    
    action operation_div() {
        // Prepare input structure to User Extern
        div_in.dividend = hdr.p4calc.operand_a;
        div_in.divisor = hdr.p4calc.operand_b;
        // Apply the User Extern
        calc_divide.apply(div_in, div_out);
        send_back(div_out.quotient);
    }
    
    action operation_sqrt() {
        calc_square_root.apply(hdr.p4calc.operand_a, square_root_result);
        send_back((bit<32>)square_root_result);
    }
    
    action operation_and() {
        send_back(hdr.p4calc.operand_a & hdr.p4calc.operand_b);
    }
    
    action operation_or() {
        send_back(hdr.p4calc.operand_a | hdr.p4calc.operand_b);
    }

    action operation_xor() {
        send_back(hdr.p4calc.operand_a ^ hdr.p4calc.operand_b);
    }

    action operation_drop() {
        smeta.drop = 1;
    }
    
    table calculate {
        key = {
            hdr.p4calc.operation : exact;
        }
        actions = {
            operation_add;
            operation_sub;
            operation_mult;
            operation_div;
            operation_sqrt;
            operation_and;
            operation_or;
            operation_xor;
            operation_drop;
        }
        direct_match = true;
        default_action = operation_drop();
    }

    apply {
        if (hdr.p4calc.isValid()) {
            calculate.apply();
        } else {
            operation_drop();
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
        packet.emit(hdr.p4calc);
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
