#[allow(unused)]
use web_sys::console;

pub fn debug_print_op_code(opcode: u8) -> String {
    //! Print out the current opcode and return the number of bytes it uses including itself
    //! Parsed off of http://www.emulator101.com/reference/8080-by-opcode.html
    match opcode {
        0x00 => {
            format!(" 	NOP	1		")
        }
        0x01 => {
            format!(" 	LXI B,D16	3		B <- byte 3, C <- byte 2")
        }
        0x02 => {
            format!(" 	STAX B	1		(BC) <- A")
        }
        0x03 => {
            format!(" 	INX B	1		BC <- BC+1")
        }
        0x04 => {
            format!(" 	INR B	1	Z, S, P, AC	B <- B+1")
        }
        0x05 => {
            format!(" 	DCR B	1	Z, S, P, AC	B <- B-1")
        }
        0x06 => {
            format!(" 	MVI B, D8	2		B <- byte 2")
        }
        0x07 => {
            format!(" 	RLC	1	CY	A = A << 1; bit 0 = prev bit 7; CY = prev bit 7")
        }
        0x08 => {
            format!(" 	-			")
        }
        0x09 => {
            format!(" 	DAD B	1	CY	HL = HL + BC")
        }
        0x0a => {
            format!(" 	LDAX B	1		A <- (BC)")
        }
        0x0b => {
            format!(" 	DCX B	1		BC = BC-1")
        }
        0x0c => {
            format!(" 	INR C	1	Z, S, P, AC	C <- C+1")
        }
        0x0d => {
            format!(" 	DCR C	1	Z, S, P, AC	C <-C-1")
        }
        0x0e => {
            format!(" 	MVI C,D8	2		C <- byte 2")
        }
        0x0f => {
            format!(" 	RRC	1	CY	A = A >> 1; bit 7 = prev bit 0; CY = prev bit 0")
        }
        0x10 => {
            format!(" 	-			")
        }
        0x11 => {
            format!(" 	LXI D,D16	3		D <- byte 3, E <- byte 2")
        }
        0x12 => {
            format!(" 	STAX D	1		(DE) <- A")
        }
        0x13 => {
            format!(" 	INX D	1		DE <- DE + 1")
        }
        0x14 => {
            format!(" 	INR D	1	Z, S, P, AC	D <- D+1")
        }
        0x15 => {
            format!(" 	DCR D	1	Z, S, P, AC	D <- D-1")
        }
        0x16 => {
            format!(" 	MVI D, D8	2		D <- byte 2")
        }
        0x17 => {
            format!(" 	RAL	1	CY	A = A << 1; bit 0 = prev CY; CY = prev bit 7")
        }
        0x18 => {
            format!(" 	-			")
        }
        0x19 => {
            format!(" 	DAD D	1	CY	HL = HL + DE")
        }
        0x1a => {
            format!(" 	LDAX D	1		A <- (DE)")
        }
        0x1b => {
            format!(" 	DCX D	1		DE = DE-1")
        }
        0x1c => {
            format!(" 	INR E	1	Z, S, P, AC	E <-E+1")
        }
        0x1d => {
            format!(" 	DCR E	1	Z, S, P, AC	E <- E-1")
        }
        0x1e => {
            format!(" 	MVI E,D8	2		E <- byte 2")
        }
        0x1f => {
            format!(" 	RAR	1	CY	A = A >> 1; bit 7 = prev bit 7; CY = prev bit 0")
        }
        0x20 => {
            format!(" 	-			")
        }
        0x21 => {
            format!(" 	LXI H,D16	3		H <- byte 3, L <- byte 2")
        }
        0x22 => {
            format!(" 	SHLD adr	3		(adr) <-L; (adr+1)<-H")
        }
        0x23 => {
            format!(" 	INX H	1		HL <- HL + 1")
        }
        0x24 => {
            format!(" 	INR H	1	Z, S, P, AC	H <- H+1")
        }
        0x25 => {
            format!(" 	DCR H	1	Z, S, P, AC	H <- H-1")
        }
        0x26 => {
            format!(" 	MVI H,D8	2		H <- byte 2")
        }
        0x27 => {
            format!(" 	DAA	1		special")
        }
        0x28 => {
            format!(" 	-			")
        }
        0x29 => {
            format!(" 	DAD H	1	CY	HL = HL + HI")
        }
        0x2a => {
            format!(" 	LHLD adr	3		L <- (adr); H<-(adr+1)")
        }
        0x2b => {
            format!(" 	DCX H	1		HL = HL-1")
        }
        0x2c => {
            format!(" 	INR L	1	Z, S, P, AC	L <- L+1")
        }
        0x2d => {
            format!(" 	DCR L	1	Z, S, P, AC	L <- L-1")
        }
        0x2e => {
            format!(" 	MVI L, D8	2		L <- byte 2")
        }
        0x2f => {
            format!(" 	CMA	1		A <- !A")
        }
        0x30 => {
            format!(" 	-			")
        }
        0x31 => {
            format!(" 	LXI SP, D16	3		SP.hi <- byte 3, SP.lo <- byte 2")
        }
        0x32 => {
            format!(" 	STA adr	3		(adr) <- A")
        }
        0x33 => {
            format!(" 	INX SP	1		SP = SP + 1")
        }
        0x34 => {
            format!(" 	INR M	1	Z, S, P, AC	(HL) <- (HL)+1")
        }
        0x35 => {
            format!(" 	DCR M	1	Z, S, P, AC	(HL) <- (HL)-1")
        }
        0x36 => {
            format!(" 	MVI M,D8	2		(HL) <- byte 2")
        }
        0x37 => {
            format!(" 	STC	1	CY	CY = 1")
        }
        0x38 => {
            format!(" 	-			")
        }
        0x39 => {
            format!(" 	DAD SP	1	CY	HL = HL + SP")
        }
        0x3a => {
            format!(" 	LDA adr	3		A <- (adr)")
        }
        0x3b => {
            format!(" 	DCX SP	1		SP = SP-1")
        }
        0x3c => {
            format!(" 	INR A	1	Z, S, P, AC	A <- A+1")
        }
        0x3d => {
            format!(" 	DCR A	1	Z, S, P, AC	A <- A-1")
        }
        0x3e => {
            format!(" 	MVI A,D8	2		A <- byte 2")
        }
        0x3f => {
            format!(" 	CMC	1	CY	CY=!CY")
        }
        0x40 => {
            format!(" 	MOV B,B	1		B <- B")
        }
        0x41 => {
            format!(" 	MOV B,C	1		B <- C")
        }
        0x42 => {
            format!(" 	MOV B,D	1		B <- D")
        }
        0x43 => {
            format!(" 	MOV B,E	1		B <- E")
        }
        0x44 => {
            format!(" 	MOV B,H	1		B <- H")
        }
        0x45 => {
            format!(" 	MOV B,L	1		B <- L")
        }
        0x46 => {
            format!(" 	MOV B,M	1		B <- (HL)")
        }
        0x47 => {
            format!(" 	MOV B,A	1		B <- A")
        }
        0x48 => {
            format!(" 	MOV C,B	1		C <- B")
        }
        0x49 => {
            format!(" 	MOV C,C	1		C <- C")
        }
        0x4a => {
            format!(" 	MOV C,D	1		C <- D")
        }
        0x4b => {
            format!(" 	MOV C,E	1		C <- E")
        }
        0x4c => {
            format!(" 	MOV C,H	1		C <- H")
        }
        0x4d => {
            format!(" 	MOV C,L	1		C <- L")
        }
        0x4e => {
            format!(" 	MOV C,M	1		C <- (HL)")
        }
        0x4f => {
            format!(" 	MOV C,A	1		C <- A")
        }
        0x50 => {
            format!(" 	MOV D,B	1		D <- B")
        }
        0x51 => {
            format!(" 	MOV D,C	1		D <- C")
        }
        0x52 => {
            format!(" 	MOV D,D	1		D <- D")
        }
        0x53 => {
            format!(" 	MOV D,E	1		D <- E")
        }
        0x54 => {
            format!(" 	MOV D,H	1		D <- H")
        }
        0x55 => {
            format!(" 	MOV D,L	1		D <- L")
        }
        0x56 => {
            format!(" 	MOV D,M	1		D <- (HL)")
        }
        0x57 => {
            format!(" 	MOV D,A	1		D <- A")
        }
        0x58 => {
            format!(" 	MOV E,B	1		E <- B")
        }
        0x59 => {
            format!(" 	MOV E,C	1		E <- C")
        }
        0x5a => {
            format!(" 	MOV E,D	1		E <- D")
        }
        0x5b => {
            format!(" 	MOV E,E	1		E <- E")
        }
        0x5c => {
            format!(" 	MOV E,H	1		E <- H")
        }
        0x5d => {
            format!(" 	MOV E,L	1		E <- L")
        }
        0x5e => {
            format!(" 	MOV E,M	1		E <- (HL)")
        }
        0x5f => {
            format!(" 	MOV E,A	1		E <- A")
        }
        0x60 => {
            format!(" 	MOV H,B	1		H <- B")
        }
        0x61 => {
            format!(" 	MOV H,C	1		H <- C")
        }
        0x62 => {
            format!(" 	MOV H,D	1		H <- D")
        }
        0x63 => {
            format!(" 	MOV H,E	1		H <- E")
        }
        0x64 => {
            format!(" 	MOV H,H	1		H <- H")
        }
        0x65 => {
            format!(" 	MOV H,L	1		H <- L")
        }
        0x66 => {
            format!(" 	MOV H,M	1		H <- (HL)")
        }
        0x67 => {
            format!(" 	MOV H,A	1		H <- A")
        }
        0x68 => {
            format!(" 	MOV L,B	1		L <- B")
        }
        0x69 => {
            format!(" 	MOV L,C	1		L <- C")
        }
        0x6a => {
            format!(" 	MOV L,D	1		L <- D")
        }
        0x6b => {
            format!(" 	MOV L,E	1		L <- E")
        }
        0x6c => {
            format!(" 	MOV L,H	1		L <- H")
        }
        0x6d => {
            format!(" 	MOV L,L	1		L <- L")
        }
        0x6e => {
            format!(" 	MOV L,M	1		L <- (HL)")
        }
        0x6f => {
            format!(" 	MOV L,A	1		L <- A")
        }
        0x70 => {
            format!(" 	MOV M,B	1		(HL) <- B")
        }
        0x71 => {
            format!(" 	MOV M,C	1		(HL) <- C")
        }
        0x72 => {
            format!(" 	MOV M,D	1		(HL) <- D")
        }
        0x73 => {
            format!(" 	MOV M,E	1		(HL) <- E")
        }
        0x74 => {
            format!(" 	MOV M,H	1		(HL) <- H")
        }
        0x75 => {
            format!(" 	MOV M,L	1		(HL) <- L")
        }
        0x76 => {
            format!(" 	HLT	1		special")
        }
        0x77 => {
            format!(" 	MOV M,A	1		(HL) <- A")
        }
        0x78 => {
            format!(" 	MOV A,B	1		A <- B")
        }
        0x79 => {
            format!(" 	MOV A,C	1		A <- C")
        }
        0x7a => {
            format!(" 	MOV A,D	1		A <- D")
        }
        0x7b => {
            format!(" 	MOV A,E	1		A <- E")
        }
        0x7c => {
            format!(" 	MOV A,H	1		A <- H")
        }
        0x7d => {
            format!(" 	MOV A,L	1		A <- L")
        }
        0x7e => {
            format!(" 	MOV A,M	1		A <- (HL)")
        }
        0x7f => {
            format!(" 	MOV A,A	1		A <- A")
        }
        0x80 => {
            format!(" 	ADD B	1	Z, S, P, CY, AC	A <- A + B")
        }
        0x81 => {
            format!(" 	ADD C	1	Z, S, P, CY, AC	A <- A + C")
        }
        0x82 => {
            format!(" 	ADD D	1	Z, S, P, CY, AC	A <- A + D")
        }
        0x83 => {
            format!(" 	ADD E	1	Z, S, P, CY, AC	A <- A + E")
        }
        0x84 => {
            format!(" 	ADD H	1	Z, S, P, CY, AC	A <- A + H")
        }
        0x85 => {
            format!(" 	ADD L	1	Z, S, P, CY, AC	A <- A + L")
        }
        0x86 => {
            format!(" 	ADD M	1	Z, S, P, CY, AC	A <- A + (HL)")
        }
        0x87 => {
            format!(" 	ADD A	1	Z, S, P, CY, AC	A <- A + A")
        }
        0x88 => {
            format!(" 	ADC B	1	Z, S, P, CY, AC	A <- A + B + CY")
        }
        0x89 => {
            format!(" 	ADC C	1	Z, S, P, CY, AC	A <- A + C + CY")
        }
        0x8a => {
            format!(" 	ADC D	1	Z, S, P, CY, AC	A <- A + D + CY")
        }
        0x8b => {
            format!(" 	ADC E	1	Z, S, P, CY, AC	A <- A + E + CY")
        }
        0x8c => {
            format!(" 	ADC H	1	Z, S, P, CY, AC	A <- A + H + CY")
        }
        0x8d => {
            format!(" 	ADC L	1	Z, S, P, CY, AC	A <- A + L + CY")
        }
        0x8e => {
            format!(" 	ADC M	1	Z, S, P, CY, AC	A <- A + (HL) + CY")
        }
        0x8f => {
            format!(" 	ADC A	1	Z, S, P, CY, AC	A <- A + A + CY")
        }
        0x90 => {
            format!(" 	SUB B	1	Z, S, P, CY, AC	A <- A - B")
        }
        0x91 => {
            format!(" 	SUB C	1	Z, S, P, CY, AC	A <- A - C")
        }
        0x92 => {
            format!(" 	SUB D	1	Z, S, P, CY, AC	A <- A + D")
        }
        0x93 => {
            format!(" 	SUB E	1	Z, S, P, CY, AC	A <- A - E")
        }
        0x94 => {
            format!(" 	SUB H	1	Z, S, P, CY, AC	A <- A + H")
        }
        0x95 => {
            format!(" 	SUB L	1	Z, S, P, CY, AC	A <- A - L")
        }
        0x96 => {
            format!(" 	SUB M	1	Z, S, P, CY, AC	A <- A + (HL)")
        }
        0x97 => {
            format!(" 	SUB A	1	Z, S, P, CY, AC	A <- A - A")
        }
        0x98 => {
            format!(" 	SBB B	1	Z, S, P, CY, AC	A <- A - B - CY")
        }
        0x99 => {
            format!(" 	SBB C	1	Z, S, P, CY, AC	A <- A - C - CY")
        }
        0x9a => {
            format!(" 	SBB D	1	Z, S, P, CY, AC	A <- A - D - CY")
        }
        0x9b => {
            format!(" 	SBB E	1	Z, S, P, CY, AC	A <- A - E - CY")
        }
        0x9c => {
            format!(" 	SBB H	1	Z, S, P, CY, AC	A <- A - H - CY")
        }
        0x9d => {
            format!(" 	SBB L	1	Z, S, P, CY, AC	A <- A - L - CY")
        }
        0x9e => {
            format!(" 	SBB M	1	Z, S, P, CY, AC	A <- A - (HL) - CY")
        }
        0x9f => {
            format!(" 	SBB A	1	Z, S, P, CY, AC	A <- A - A - CY")
        }
        0xa0 => {
            format!(" 	ANA B	1	Z, S, P, CY, AC	A <- A & B")
        }
        0xa1 => {
            format!(" 	ANA C	1	Z, S, P, CY, AC	A <- A & C")
        }
        0xa2 => {
            format!(" 	ANA D	1	Z, S, P, CY, AC	A <- A & D")
        }
        0xa3 => {
            format!(" 	ANA E	1	Z, S, P, CY, AC	A <- A & E")
        }
        0xa4 => {
            format!(" 	ANA H	1	Z, S, P, CY, AC	A <- A & H")
        }
        0xa5 => {
            format!(" 	ANA L	1	Z, S, P, CY, AC	A <- A & L")
        }
        0xa6 => {
            format!(" 	ANA M	1	Z, S, P, CY, AC	A <- A & (HL)")
        }
        0xa7 => {
            format!(" 	ANA A	1	Z, S, P, CY, AC	A <- A & A")
        }
        0xa8 => {
            format!(" 	XRA B	1	Z, S, P, CY, AC	A <- A ^ B")
        }
        0xa9 => {
            format!(" 	XRA C	1	Z, S, P, CY, AC	A <- A ^ C")
        }
        0xaa => {
            format!(" 	XRA D	1	Z, S, P, CY, AC	A <- A ^ D")
        }
        0xab => {
            format!(" 	XRA E	1	Z, S, P, CY, AC	A <- A ^ E")
        }
        0xac => {
            format!(" 	XRA H	1	Z, S, P, CY, AC	A <- A ^ H")
        }
        0xad => {
            format!(" 	XRA L	1	Z, S, P, CY, AC	A <- A ^ L")
        }
        0xae => {
            format!(" 	XRA M	1	Z, S, P, CY, AC	A <- A ^ (HL)")
        }
        0xaf => {
            format!(" 	XRA A	1	Z, S, P, CY, AC	A <- A ^ A")
        }
        0xb0 => {
            format!(" 	ORA B	1	Z, S, P, CY, AC	A <- A | B")
        }
        0xb1 => {
            format!(" 	ORA C	1	Z, S, P, CY, AC	A <- A | C")
        }
        0xb2 => {
            format!(" 	ORA D	1	Z, S, P, CY, AC	A <- A | D")
        }
        0xb3 => {
            format!(" 	ORA E	1	Z, S, P, CY, AC	A <- A | E")
        }
        0xb4 => {
            format!(" 	ORA H	1	Z, S, P, CY, AC	A <- A | H")
        }
        0xb5 => {
            format!(" 	ORA L	1	Z, S, P, CY, AC	A <- A | L")
        }
        0xb6 => {
            format!(" 	ORA M	1	Z, S, P, CY, AC	A <- A | (HL)")
        }
        0xb7 => {
            format!(" 	ORA A	1	Z, S, P, CY, AC	A <- A | A")
        }
        0xb8 => {
            format!(" 	CMP B	1	Z, S, P, CY, AC	A - B")
        }
        0xb9 => {
            format!(" 	CMP C	1	Z, S, P, CY, AC	A - C")
        }
        0xba => {
            format!(" 	CMP D	1	Z, S, P, CY, AC	A - D")
        }
        0xbb => {
            format!(" 	CMP E	1	Z, S, P, CY, AC	A - E")
        }
        0xbc => {
            format!(" 	CMP H	1	Z, S, P, CY, AC	A - H")
        }
        0xbd => {
            format!(" 	CMP L	1	Z, S, P, CY, AC	A - L")
        }
        0xbe => {
            format!(" 	CMP M	1	Z, S, P, CY, AC	A - (HL)")
        }
        0xbf => {
            format!(" 	CMP A	1	Z, S, P, CY, AC	A - A")
        }
        0xc0 => {
            format!(" 	RNZ	1		if NZ, RET")
        }
        0xc1 => {
            format!(" 	POP B	1		C <- (sp); B <- (sp+1); sp <- sp+2")
        }
        0xc2 => {
            format!(" 	JNZ adr	3		if NZ, PC <- adr")
        }
        0xc3 => {
            format!(" 	JMP adr	3		PC <= adr")
        }
        0xc4 => {
            format!(" 	CNZ adr	3		if NZ, CALL adr")
        }
        0xc5 => {
            format!(" 	PUSH B	1		(sp-2)<-C; (sp-1)<-B; sp <- sp - 2")
        }
        0xc6 => {
            format!(" 	ADI D8	2	Z, S, P, CY, AC	A <- A + byte")
        }
        0xc7 => {
            format!(" 	RST 0	1		CALL $0")
        }
        0xc8 => {
            format!(" 	RZ	1		if Z, RET")
        }
        0xc9 => {
            format!(" 	RET	1		PC.lo <- (sp); PC.hi<-(sp+1); SP <- SP+2")
        }
        0xca => {
            format!(" 	JZ adr	3		if Z, PC <- adr")
        }
        0xcb => {
            format!(" 	-			")
        }
        0xcc => {
            format!(" 	CZ adr	3		if Z, CALL adr")
        }
        0xcd => {
            format!(" 	CALL adr	3		(SP-1)<-PC.hi;(SP-2)<-PC.lo;SP<-SP-2;PC=adr")
        }
        0xce => {
            format!(" 	ACI D8	2	Z, S, P, CY, AC	A <- A + data + CY")
        }
        0xcf => {
            format!(" 	RST 1	1		CALL $8")
        }
        0xd0 => {
            format!(" 	RNC	1		if NCY, RET")
        }
        0xd1 => {
            format!(" 	POP D	1		E <- (sp); D <- (sp+1); sp <- sp+2")
        }
        0xd2 => {
            format!(" 	JNC adr	3		if NCY, PC<-adr")
        }
        0xd3 => {
            format!(" 	OUT D8	2		special")
        }
        0xd4 => {
            format!(" 	CNC adr	3		if NCY, CALL adr")
        }
        0xd5 => {
            format!(" 	PUSH D	1		(sp-2)<-E; (sp-1)<-D; sp <- sp - 2")
        }
        0xd6 => {
            format!(" 	SUI D8	2	Z, S, P, CY, AC	A <- A - data")
        }
        0xd7 => {
            format!(" 	RST 2	1		CALL $10")
        }
        0xd8 => {
            format!(" 	RC	1		if CY, RET")
        }
        0xd9 => {
            format!(" 	-			")
        }
        0xda => {
            format!(" 	JC adr	3		if CY, PC<-adr")
        }
        0xdb => {
            format!(" 	IN D8	2		special")
        }
        0xdc => {
            format!(" 	CC adr	3		if CY, CALL adr")
        }
        0xdd => {
            format!(" 	-			")
        }
        0xde => {
            format!(" 	SBI D8	2	Z, S, P, CY, AC	A <- A - data - CY")
        }
        0xdf => {
            format!(" 	RST 3	1		CALL $18")
        }
        0xe0 => {
            format!(" 	RPO	1		if PO, RET")
        }
        0xe1 => {
            format!(" 	POP H	1		L <- (sp); H <- (sp+1); sp <- sp+2")
        }
        0xe2 => {
            format!(" 	JPO adr	3		if PO, PC <- adr")
        }
        0xe3 => {
            format!(" 	XTHL	1		L <-> (SP); H <-> (SP+1)")
        }
        0xe4 => {
            format!(" 	CPO adr	3		if PO, CALL adr")
        }
        0xe5 => {
            format!(" 	PUSH H	1		(sp-2)<-L; (sp-1)<-H; sp <- sp - 2")
        }
        0xe6 => {
            format!(" 	ANI D8	2	Z, S, P, CY, AC	A <- A & data")
        }
        0xe7 => {
            format!(" 	RST 4	1		CALL $20")
        }
        0xe8 => {
            format!(" 	RPE	1		if PE, RET")
        }
        0xe9 => {
            format!(" 	PCHL	1		PC.hi <- H; PC.lo <- L")
        }
        0xea => {
            format!(" 	JPE adr	3		if PE, PC <- adr")
        }
        0xeb => {
            format!(" 	XCHG	1		H <-> D; L <-> E")
        }
        0xec => {
            format!(" 	CPE adr	3		if PE, CALL adr")
        }
        0xed => {
            format!(" 	-			")
        }
        0xee => {
            format!(" 	XRI D8	2	Z, S, P, CY, AC	A <- A ^ data")
        }
        0xef => {
            format!(" 	RST 5	1		CALL $28")
        }
        0xf0 => {
            format!(" 	RP	1		if P, RET")
        }
        0xf1 => {
            format!(" 	POP PSW	1		flags <- (sp); A <- (sp+1); sp <- sp+2")
        }
        0xf2 => {
            format!(" 	JP adr	3		if P=1 PC <- adr")
        }
        0xf3 => {
            format!(" 	DI	1		special")
        }
        0xf4 => {
            format!(" 	CP adr	3		if P, PC <- adr")
        }
        0xf5 => {
            format!(" 	PUSH PSW	1		(sp-2)<-flags; (sp-1)<-A; sp <- sp - 2")
        }
        0xf6 => {
            format!(" 	ORI D8	2	Z, S, P, CY, AC	A <- A | data")
        }
        0xf7 => {
            format!(" 	RST 6	1		CALL $30")
        }
        0xf8 => {
            format!(" 	RM	1		if M, RET")
        }
        0xf9 => {
            format!(" 	SPHL	1		SP=HL")
        }
        0xfa => {
            format!(" 	JM adr	3		if M, PC <- adr")
        }
        0xfb => {
            format!(" 	EI	1		special")
        }
        0xfc => {
            format!(" 	CM adr	3		if M, CALL adr")
        }
        0xfd => {
            format!(" 	-			")
        }
        0xfe => {
            format!(" 	CPI D8	2	Z, S, P, CY, AC	A - data")
        }
        0xff => {
            format!(" 	RST 7	1		CALL $38")
        }
    }
}
