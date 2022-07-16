#![allow(unused)]
use crate::eighty_eighty_emulator::ConditionFlags;
use lazy_static::lazy_static;
use regex::Regex;
use std::io;
use std::io::prelude::*;

use crate::{eighty_eighty_emulator::ProcessorState, machine::MachineState};
#[allow(unused)]

pub fn pause() {
    let mut stdin = io::stdin();
    let _ = stdin.read(&mut [0u8]).unwrap();
}

pub fn debug_console_print(print_this: &String) {
    if cfg!(target_arch = "wasm32") {
        web_sys::console::log_1(&print_this.into());
    }
    if cfg!(target_arch = "x86_64") {
        println!("{}", &print_this);
    }
}

pub fn opcode_printer(state: &MachineState) {
    // 1a35 INX    D   .sp..  A $00 B $af C $00 D $1b E $52 H $20 L $52 SP 23fe
    // print the address in hex, print the instruction and source/dest, flags, registers
    // A B C D E H L SP

    let (opcode_mnem, opcode_len, _) =
        debug_print_op_code(state.mem_map[state.processor_state.prog_counter]);
    let opcode_message = match opcode_len {
        1 => format!("{} ", opcode_mnem),
        2 => format!(
            "{} {:#04x} ",
            opcode_mnem,
            state.mem_map[state.processor_state.prog_counter + 1],
        ),
        3 => format!(
            "{} {:#04x} {:#04x} ",
            opcode_mnem,
            state.mem_map[state.processor_state.prog_counter + 1],
            state.mem_map[state.processor_state.prog_counter + 2],
        ),
        _ => panic!(),
    };

    // mnemonic, length, message
    let (opcode_mnem, _, _) =
        debug_print_op_code(state.mem_map[state.processor_state.prog_counter]);
    print!(
        "{:04x} {} ",
        state.processor_state.prog_counter, opcode_mnem,
    )
}

pub fn processor_state_printer(state: &MachineState) {
    print!(
        "{}",
        if state.processor_state.flags.contains(ConditionFlags::Z) {
            "z"
        } else {
            "."
        }
    );
    print!(
        "{}",
        if state.processor_state.flags.contains(ConditionFlags::S) {
            "s"
        } else {
            "."
        }
    );
    print!(
        "{}",
        if state.processor_state.flags.contains(ConditionFlags::P) {
            "p"
        } else {
            "."
        }
    );
    print!(
        "{}",
        if state.processor_state.flags.contains(ConditionFlags::CY) {
            "c"
        } else {
            "."
        }
    );
    print!(
        "{}",
        if state.processor_state.flags.contains(ConditionFlags::AC) {
            "a"
        } else {
            "."
        }
    );

    print!(
        " A ${:02x} B ${:02x} C ${:02x} D ${:02x} E ${:02x} H ${:02x} L ${:02x} SP {:04x}",
        state.processor_state.reg_a,
        state.processor_state.reg_b,
        state.processor_state.reg_c,
        state.processor_state.reg_d,
        state.processor_state.reg_e,
        state.processor_state.reg_h,
        state.processor_state.reg_l,
        state.processor_state.stack_pointer,
    );
}

pub fn debug_print_op_code(opcode: u8) -> (String, u8, String) {
    //! Print out the current opcode and return the number of bytes it uses including itself
    //! Parsed off of http://www.emulator101.com/reference/8080-by-opcode.html
    //! Todo: This needs to be parsing out the first white space surrounded integer as the num bytes
    let opcode_message = match opcode {
        0x00 => " 	NOP	1		".to_string(),
        0x01 => " 	LXI B,D16	3		B <- byte 3, C <- byte 2".to_string(),
        0x02 => " 	STAX B	1		(BC) <- A".to_string(),
        0x03 => " 	INX B	1		BC <- BC+1".to_string(),
        0x04 => " 	INR B	1	Z, S, P, AC	B <- B+1".to_string(),
        0x05 => " 	DCR B	1	Z, S, P, AC	B <- B-1".to_string(),
        0x06 => " 	MVI B, D8	2		B <- byte 2".to_string(),
        0x07 => " 	RLC	1	CY	A = A << 1; bit 0 = prev bit 7; CY = prev bit 7".to_string(),
        0x08 => " 	-			".to_string(),
        0x09 => " 	DAD B	1	CY	HL = HL + BC".to_string(),
        0x0a => " 	LDAX B	1		A <- (BC)".to_string(),
        0x0b => " 	DCX B	1		BC = BC-1".to_string(),
        0x0c => " 	INR C	1	Z, S, P, AC	C <- C+1".to_string(),
        0x0d => " 	DCR C	1	Z, S, P, AC	C <-C-1".to_string(),
        0x0e => " 	MVI C,D8	2		C <- byte 2".to_string(),
        0x0f => " 	RRC	1	CY	A = A >> 1; bit 7 = prev bit 0; CY = prev bit 0".to_string(),
        0x10 => " 	-			".to_string(),
        0x11 => " 	LXI D,D16	3		D <- byte 3, E <- byte 2".to_string(),
        0x12 => " 	STAX D	1		(DE) <- A".to_string(),
        0x13 => " 	INX D	1		DE <- DE + 1".to_string(),
        0x14 => " 	INR D	1	Z, S, P, AC	D <- D+1".to_string(),
        0x15 => " 	DCR D	1	Z, S, P, AC	D <- D-1".to_string(),
        0x16 => " 	MVI D, D8	2		D <- byte 2".to_string(),
        0x17 => " 	RAL	1	CY	A = A << 1; bit 0 = prev CY; CY = prev bit 7".to_string(),
        0x18 => " 	-			".to_string(),
        0x19 => " 	DAD D	1	CY	HL = HL + DE".to_string(),
        0x1a => " 	LDAX D	1		A <- (DE)".to_string(),
        0x1b => " 	DCX D	1		DE = DE-1".to_string(),
        0x1c => " 	INR E	1	Z, S, P, AC	E <-E+1".to_string(),
        0x1d => " 	DCR E	1	Z, S, P, AC	E <- E-1".to_string(),
        0x1e => " 	MVI E,D8	2		E <- byte 2".to_string(),
        0x1f => " 	RAR	1	CY	A = A >> 1; bit 7 = prev bit 7; CY = prev bit 0".to_string(),
        0x20 => " 	-			".to_string(),
        0x21 => " 	LXI H,D16	3		H <- byte 3, L <- byte 2".to_string(),
        0x22 => " 	SHLD adr	3		(adr) <-L; (adr+1)<-H".to_string(),
        0x23 => " 	INX H	1		HL <- HL + 1".to_string(),
        0x24 => " 	INR H	1	Z, S, P, AC	H <- H+1".to_string(),
        0x25 => " 	DCR H	1	Z, S, P, AC	H <- H-1".to_string(),
        0x26 => " 	MVI H,D8	2		H <- byte 2".to_string(),
        0x27 => " 	DAA	1		special".to_string(),
        0x28 => " 	-			".to_string(),
        0x29 => " 	DAD H	1	CY	HL = HL + HI".to_string(),
        0x2a => " 	LHLD adr	3		L <- (adr); H<-(adr+1)".to_string(),
        0x2b => " 	DCX H	1		HL = HL-1".to_string(),
        0x2c => " 	INR L	1	Z, S, P, AC	L <- L+1".to_string(),
        0x2d => " 	DCR L	1	Z, S, P, AC	L <- L-1".to_string(),
        0x2e => " 	MVI L, D8	2		L <- byte 2".to_string(),
        0x2f => " 	CMA	1		A <- !A".to_string(),
        0x30 => " 	-			".to_string(),
        0x31 => " 	LXI SP, D16	3		SP.hi <- byte 3, SP.lo <- byte 2".to_string(),
        0x32 => " 	STA adr	3		(adr) <- A".to_string(),
        0x33 => " 	INX SP	1		SP = SP + 1".to_string(),
        0x34 => " 	INR M	1	Z, S, P, AC	(HL) <- (HL)+1".to_string(),
        0x35 => " 	DCR M	1	Z, S, P, AC	(HL) <- (HL)-1".to_string(),
        0x36 => " 	MVI M,D8	2		(HL) <- byte 2".to_string(),
        0x37 => " 	STC	1	CY	CY = 1".to_string(),
        0x38 => " 	-			".to_string(),
        0x39 => " 	DAD SP	1	CY	HL = HL + SP".to_string(),
        0x3a => " 	LDA adr	3		A <- (adr)".to_string(),
        0x3b => " 	DCX SP	1		SP = SP-1".to_string(),
        0x3c => " 	INR A	1	Z, S, P, AC	A <- A+1".to_string(),
        0x3d => " 	DCR A	1	Z, S, P, AC	A <- A-1".to_string(),
        0x3e => " 	MVI A,D8	2		A <- byte 2".to_string(),
        0x3f => " 	CMC	1	CY	CY=!CY".to_string(),
        0x40 => " 	MOV B,B	1		B <- B".to_string(),
        0x41 => " 	MOV B,C	1		B <- C".to_string(),
        0x42 => " 	MOV B,D	1		B <- D".to_string(),
        0x43 => " 	MOV B,E	1		B <- E".to_string(),
        0x44 => " 	MOV B,H	1		B <- H".to_string(),
        0x45 => " 	MOV B,L	1		B <- L".to_string(),
        0x46 => " 	MOV B,M	1		B <- (HL)".to_string(),
        0x47 => " 	MOV B,A	1		B <- A".to_string(),
        0x48 => " 	MOV C,B	1		C <- B".to_string(),
        0x49 => " 	MOV C,C	1		C <- C".to_string(),
        0x4a => " 	MOV C,D	1		C <- D".to_string(),
        0x4b => " 	MOV C,E	1		C <- E".to_string(),
        0x4c => " 	MOV C,H	1		C <- H".to_string(),
        0x4d => " 	MOV C,L	1		C <- L".to_string(),
        0x4e => " 	MOV C,M	1		C <- (HL)".to_string(),
        0x4f => " 	MOV C,A	1		C <- A".to_string(),
        0x50 => " 	MOV D,B	1		D <- B".to_string(),
        0x51 => " 	MOV D,C	1		D <- C".to_string(),
        0x52 => " 	MOV D,D	1		D <- D".to_string(),
        0x53 => " 	MOV D,E	1		D <- E".to_string(),
        0x54 => " 	MOV D,H	1		D <- H".to_string(),
        0x55 => " 	MOV D,L	1		D <- L".to_string(),
        0x56 => " 	MOV D,M	1		D <- (HL)".to_string(),
        0x57 => " 	MOV D,A	1		D <- A".to_string(),
        0x58 => " 	MOV E,B	1		E <- B".to_string(),
        0x59 => " 	MOV E,C	1		E <- C".to_string(),
        0x5a => " 	MOV E,D	1		E <- D".to_string(),
        0x5b => " 	MOV E,E	1		E <- E".to_string(),
        0x5c => " 	MOV E,H	1		E <- H".to_string(),
        0x5d => " 	MOV E,L	1		E <- L".to_string(),
        0x5e => " 	MOV E,M	1		E <- (HL)".to_string(),
        0x5f => " 	MOV E,A	1		E <- A".to_string(),
        0x60 => " 	MOV H,B	1		H <- B".to_string(),
        0x61 => " 	MOV H,C	1		H <- C".to_string(),
        0x62 => " 	MOV H,D	1		H <- D".to_string(),
        0x63 => " 	MOV H,E	1		H <- E".to_string(),
        0x64 => " 	MOV H,H	1		H <- H".to_string(),
        0x65 => " 	MOV H,L	1		H <- L".to_string(),
        0x66 => " 	MOV H,M	1		H <- (HL)".to_string(),
        0x67 => " 	MOV H,A	1		H <- A".to_string(),
        0x68 => " 	MOV L,B	1		L <- B".to_string(),
        0x69 => " 	MOV L,C	1		L <- C".to_string(),
        0x6a => " 	MOV L,D	1		L <- D".to_string(),
        0x6b => " 	MOV L,E	1		L <- E".to_string(),
        0x6c => " 	MOV L,H	1		L <- H".to_string(),
        0x6d => " 	MOV L,L	1		L <- L".to_string(),
        0x6e => " 	MOV L,M	1		L <- (HL)".to_string(),
        0x6f => " 	MOV L,A	1		L <- A".to_string(),
        0x70 => " 	MOV M,B	1		(HL) <- B".to_string(),
        0x71 => " 	MOV M,C	1		(HL) <- C".to_string(),
        0x72 => " 	MOV M,D	1		(HL) <- D".to_string(),
        0x73 => " 	MOV M,E	1		(HL) <- E".to_string(),
        0x74 => " 	MOV M,H	1		(HL) <- H".to_string(),
        0x75 => " 	MOV M,L	1		(HL) <- L".to_string(),
        0x76 => " 	HLT	1		special".to_string(),
        0x77 => " 	MOV M,A	1		(HL) <- A".to_string(),
        0x78 => " 	MOV A,B	1		A <- B".to_string(),
        0x79 => " 	MOV A,C	1		A <- C".to_string(),
        0x7a => " 	MOV A,D	1		A <- D".to_string(),
        0x7b => " 	MOV A,E	1		A <- E".to_string(),
        0x7c => " 	MOV A,H	1		A <- H".to_string(),
        0x7d => " 	MOV A,L	1		A <- L".to_string(),
        0x7e => " 	MOV A,M	1		A <- (HL)".to_string(),
        0x7f => " 	MOV A,A	1		A <- A".to_string(),
        0x80 => " 	ADD B	1	Z, S, P, CY, AC	A <- A + B".to_string(),
        0x81 => " 	ADD C	1	Z, S, P, CY, AC	A <- A + C".to_string(),
        0x82 => " 	ADD D	1	Z, S, P, CY, AC	A <- A + D".to_string(),
        0x83 => " 	ADD E	1	Z, S, P, CY, AC	A <- A + E".to_string(),
        0x84 => " 	ADD H	1	Z, S, P, CY, AC	A <- A + H".to_string(),
        0x85 => " 	ADD L	1	Z, S, P, CY, AC	A <- A + L".to_string(),
        0x86 => " 	ADD M	1	Z, S, P, CY, AC	A <- A + (HL)".to_string(),
        0x87 => " 	ADD A	1	Z, S, P, CY, AC	A <- A + A".to_string(),
        0x88 => " 	ADC B	1	Z, S, P, CY, AC	A <- A + B + CY".to_string(),
        0x89 => " 	ADC C	1	Z, S, P, CY, AC	A <- A + C + CY".to_string(),
        0x8a => " 	ADC D	1	Z, S, P, CY, AC	A <- A + D + CY".to_string(),
        0x8b => " 	ADC E	1	Z, S, P, CY, AC	A <- A + E + CY".to_string(),
        0x8c => " 	ADC H	1	Z, S, P, CY, AC	A <- A + H + CY".to_string(),
        0x8d => " 	ADC L	1	Z, S, P, CY, AC	A <- A + L + CY".to_string(),
        0x8e => " 	ADC M	1	Z, S, P, CY, AC	A <- A + (HL) + CY".to_string(),
        0x8f => " 	ADC A	1	Z, S, P, CY, AC	A <- A + A + CY".to_string(),
        0x90 => " 	SUB B	1	Z, S, P, CY, AC	A <- A - B".to_string(),
        0x91 => " 	SUB C	1	Z, S, P, CY, AC	A <- A - C".to_string(),
        0x92 => " 	SUB D	1	Z, S, P, CY, AC	A <- A + D".to_string(),
        0x93 => " 	SUB E	1	Z, S, P, CY, AC	A <- A - E".to_string(),
        0x94 => " 	SUB H	1	Z, S, P, CY, AC	A <- A + H".to_string(),
        0x95 => " 	SUB L	1	Z, S, P, CY, AC	A <- A - L".to_string(),
        0x96 => " 	SUB M	1	Z, S, P, CY, AC	A <- A + (HL)".to_string(),
        0x97 => " 	SUB A	1	Z, S, P, CY, AC	A <- A - A".to_string(),
        0x98 => " 	SBB B	1	Z, S, P, CY, AC	A <- A - B - CY".to_string(),
        0x99 => " 	SBB C	1	Z, S, P, CY, AC	A <- A - C - CY".to_string(),
        0x9a => " 	SBB D	1	Z, S, P, CY, AC	A <- A - D - CY".to_string(),
        0x9b => " 	SBB E	1	Z, S, P, CY, AC	A <- A - E - CY".to_string(),
        0x9c => " 	SBB H	1	Z, S, P, CY, AC	A <- A - H - CY".to_string(),
        0x9d => " 	SBB L	1	Z, S, P, CY, AC	A <- A - L - CY".to_string(),
        0x9e => " 	SBB M	1	Z, S, P, CY, AC	A <- A - (HL) - CY".to_string(),
        0x9f => " 	SBB A	1	Z, S, P, CY, AC	A <- A - A - CY".to_string(),
        0xa0 => " 	ANA B	1	Z, S, P, CY, AC	A <- A & B".to_string(),
        0xa1 => " 	ANA C	1	Z, S, P, CY, AC	A <- A & C".to_string(),
        0xa2 => " 	ANA D	1	Z, S, P, CY, AC	A <- A & D".to_string(),
        0xa3 => " 	ANA E	1	Z, S, P, CY, AC	A <- A & E".to_string(),
        0xa4 => " 	ANA H	1	Z, S, P, CY, AC	A <- A & H".to_string(),
        0xa5 => " 	ANA L	1	Z, S, P, CY, AC	A <- A & L".to_string(),
        0xa6 => " 	ANA M	1	Z, S, P, CY, AC	A <- A & (HL)".to_string(),
        0xa7 => " 	ANA A	1	Z, S, P, CY, AC	A <- A & A".to_string(),
        0xa8 => " 	XRA B	1	Z, S, P, CY, AC	A <- A ^ B".to_string(),
        0xa9 => " 	XRA C	1	Z, S, P, CY, AC	A <- A ^ C".to_string(),
        0xaa => " 	XRA D	1	Z, S, P, CY, AC	A <- A ^ D".to_string(),
        0xab => " 	XRA E	1	Z, S, P, CY, AC	A <- A ^ E".to_string(),
        0xac => " 	XRA H	1	Z, S, P, CY, AC	A <- A ^ H".to_string(),
        0xad => " 	XRA L	1	Z, S, P, CY, AC	A <- A ^ L".to_string(),
        0xae => " 	XRA M	1	Z, S, P, CY, AC	A <- A ^ (HL)".to_string(),
        0xaf => " 	XRA A	1	Z, S, P, CY, AC	A <- A ^ A".to_string(),
        0xb0 => " 	ORA B	1	Z, S, P, CY, AC	A <- A | B".to_string(),
        0xb1 => " 	ORA C	1	Z, S, P, CY, AC	A <- A | C".to_string(),
        0xb2 => " 	ORA D	1	Z, S, P, CY, AC	A <- A | D".to_string(),
        0xb3 => " 	ORA E	1	Z, S, P, CY, AC	A <- A | E".to_string(),
        0xb4 => " 	ORA H	1	Z, S, P, CY, AC	A <- A | H".to_string(),
        0xb5 => " 	ORA L	1	Z, S, P, CY, AC	A <- A | L".to_string(),
        0xb6 => " 	ORA M	1	Z, S, P, CY, AC	A <- A | (HL)".to_string(),
        0xb7 => " 	ORA A	1	Z, S, P, CY, AC	A <- A | A".to_string(),
        0xb8 => " 	CMP B	1	Z, S, P, CY, AC	A - B".to_string(),
        0xb9 => " 	CMP C	1	Z, S, P, CY, AC	A - C".to_string(),
        0xba => " 	CMP D	1	Z, S, P, CY, AC	A - D".to_string(),
        0xbb => " 	CMP E	1	Z, S, P, CY, AC	A - E".to_string(),
        0xbc => " 	CMP H	1	Z, S, P, CY, AC	A - H".to_string(),
        0xbd => " 	CMP L	1	Z, S, P, CY, AC	A - L".to_string(),
        0xbe => " 	CMP M	1	Z, S, P, CY, AC	A - (HL)".to_string(),
        0xbf => " 	CMP A	1	Z, S, P, CY, AC	A - A".to_string(),
        0xc0 => " 	RNZ	1		if NZ, RET".to_string(),
        0xc1 => " 	POP B	1		C <- (sp); B <- (sp+1); sp <- sp+2".to_string(),
        0xc2 => " 	JNZ adr	3		if NZ, PC <- adr".to_string(),
        0xc3 => " 	JMP adr	3		PC <= adr".to_string(),
        0xc4 => " 	CNZ adr	3		if NZ, CALL adr".to_string(),
        0xc5 => " 	PUSH B	1		(sp-2)<-C; (sp-1)<-B; sp <- sp - 2".to_string(),
        0xc6 => " 	ADI D8	2	Z, S, P, CY, AC	A <- A + byte".to_string(),
        0xc7 => " 	RST 0	1		CALL $0".to_string(),
        0xc8 => " 	RZ	1		if Z, RET".to_string(),
        0xc9 => " 	RET	1		PC.lo <- (sp); PC.hi<-(sp+1); SP <- SP+2".to_string(),
        0xca => " 	JZ adr	3		if Z, PC <- adr".to_string(),
        0xcb => " 	-			".to_string(),
        0xcc => " 	CZ adr	3		if Z, CALL adr".to_string(),
        0xcd => " 	CALL adr	3		(SP-1)<-PC.hi;(SP-2)<-PC.lo;SP<-SP-2;PC=adr".to_string(),
        0xce => " 	ACI D8	2	Z, S, P, CY, AC	A <- A + data + CY".to_string(),
        0xcf => " 	RST 1	1		CALL $8".to_string(),
        0xd0 => " 	RNC	1		if NCY, RET".to_string(),
        0xd1 => " 	POP D	1		E <- (sp); D <- (sp+1); sp <- sp+2".to_string(),
        0xd2 => " 	JNC adr	3		if NCY, PC<-adr".to_string(),
        0xd3 => " 	OUT D8	2		special".to_string(),
        0xd4 => " 	CNC adr	3		if NCY, CALL adr".to_string(),
        0xd5 => " 	PUSH D	1		(sp-2)<-E; (sp-1)<-D; sp <- sp - 2".to_string(),
        0xd6 => " 	SUI D8	2	Z, S, P, CY, AC	A <- A - data".to_string(),
        0xd7 => " 	RST 2	1		CALL $10".to_string(),
        0xd8 => " 	RC	1		if CY, RET".to_string(),
        0xd9 => " 	-			".to_string(),
        0xda => " 	JC adr	3		if CY, PC<-adr".to_string(),
        0xdb => " 	IN D8	2		special".to_string(),
        0xdc => " 	CC adr	3		if CY, CALL adr".to_string(),
        0xdd => " 	-			".to_string(),
        0xde => " 	SBI D8	2	Z, S, P, CY, AC	A <- A - data - CY".to_string(),
        0xdf => " 	RST 3	1		CALL $18".to_string(),
        0xe0 => " 	RPO	1		if PO, RET".to_string(),
        0xe1 => " 	POP H	1		L <- (sp); H <- (sp+1); sp <- sp+2".to_string(),
        0xe2 => " 	JPO adr	3		if PO, PC <- adr".to_string(),
        0xe3 => " 	XTHL	1		L <-> (SP); H <-> (SP+1)".to_string(),
        0xe4 => " 	CPO adr	3		if PO, CALL adr".to_string(),
        0xe5 => " 	PUSH H	1		(sp-2)<-L; (sp-1)<-H; sp <- sp - 2".to_string(),
        0xe6 => " 	ANI D8	2	Z, S, P, CY, AC	A <- A & data".to_string(),
        0xe7 => " 	RST 4	1		CALL $20".to_string(),
        0xe8 => " 	RPE	1		if PE, RET".to_string(),
        0xe9 => " 	PCHL	1		PC.hi <- H; PC.lo <- L".to_string(),
        0xea => " 	JPE adr	3		if PE, PC <- adr".to_string(),
        0xeb => " 	XCHG	1		H <-> D; L <-> E".to_string(),
        0xec => " 	CPE adr	3		if PE, CALL adr".to_string(),
        0xed => " 	-			".to_string(),
        0xee => " 	XRI D8	2	Z, S, P, CY, AC	A <- A ^ data".to_string(),
        0xef => " 	RST 5	1		CALL $28".to_string(),
        0xf0 => " 	RP	1		if P, RET".to_string(),
        0xf1 => " 	POP PSW	1		flags <- (sp); A <- (sp+1); sp <- sp+2".to_string(),
        0xf2 => " 	JP adr	3		if P=1 PC <- adr".to_string(),
        0xf3 => " 	DI	1		special".to_string(),
        0xf4 => " 	CP adr	3		if P, PC <- adr".to_string(),
        0xf5 => " 	PUSH PSW	1		(sp-2)<-flags; (sp-1)<-A; sp <- sp - 2".to_string(),
        0xf6 => " 	ORI D8	2	Z, S, P, CY, AC	A <- A | data".to_string(),
        0xf7 => " 	RST 6	1		CALL $30".to_string(),
        0xf8 => " 	RM	1		if M, RET".to_string(),
        0xf9 => " 	SPHL	1		SP=HL".to_string(),
        0xfa => " 	JM adr	3		if M, PC <- adr".to_string(),
        0xfb => " 	EI	1		special".to_string(),
        0xfc => " 	CM adr	3		if M, CALL adr".to_string(),
        0xfd => " 	-			".to_string(),
        0xfe => " 	CPI D8	2	Z, S, P, CY, AC	A - data".to_string(),
        0xff => " 	RST 7	1		CALL $38".to_string(),
    };
    lazy_static! {
        static ref RE: Regex =
            Regex::new(r"^.*\t(?P<opcode_mnem>\S*).*\t(?P<num_bytes>[123])").unwrap();
    }
    if let Some(opcode_parsed) = RE.captures_iter(&opcode_message).next() {
        let num_bytes = opcode_parsed
            .name("num_bytes")
            .unwrap()
            .as_str()
            .to_string()
            .parse()
            .unwrap();
        let opcode_mnem = opcode_parsed
            .name("opcode_mnem")
            .unwrap()
            .as_str()
            .to_string();
        (opcode_mnem, num_bytes, opcode_message)
    } else {
        ("Not an instruction".to_string(), 1, opcode_message)
    }
}
