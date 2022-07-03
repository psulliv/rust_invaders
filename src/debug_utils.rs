#[allow(unused)]
pub fn debug_print_op_code(opcode: u8) -> usize {
    //! Print out the current opcode and return the number of bytes it uses including itself
    //! Parsed off of http://www.emulator101.com/reference/8080-by-opcode.html
    match opcode {
        0x00 => {
            println!(" 	NOP	1		");
            1
        }
        0x01 => {
            println!(" 	LXI B,D16	3		B <- byte 3, C <- byte 2");
            3
        }
        0x02 => {
            println!(" 	STAX B	1		(BC) <- A");
            1
        }
        0x03 => {
            println!(" 	INX B	1		BC <- BC+1");
            1
        }
        0x04 => {
            println!(" 	INR B	1	Z, S, P, AC	B <- B+1");
            1
        }
        0x05 => {
            println!(" 	DCR B	1	Z, S, P, AC	B <- B-1");
            1
        }
        0x06 => {
            println!(" 	MVI B, D8	2		B <- byte 2");
            2
        }
        0x07 => {
            println!(" 	RLC	1	CY	A = A << 1; bit 0 = prev bit 7; CY = prev bit 7");
            1
        }
        0x08 => {
            println!(" 	-			");
            1
        }
        0x09 => {
            println!(" 	DAD B	1	CY	HL = HL + BC");
            1
        }
        0x0a => {
            println!(" 	LDAX B	1		A <- (BC)");
            1
        }
        0x0b => {
            println!(" 	DCX B	1		BC = BC-1");
            1
        }
        0x0c => {
            println!(" 	INR C	1	Z, S, P, AC	C <- C+1");
            1
        }
        0x0d => {
            println!(" 	DCR C	1	Z, S, P, AC	C <-C-1");
            1
        }
        0x0e => {
            println!(" 	MVI C,D8	2		C <- byte 2");
            2
        }
        0x0f => {
            println!(" 	RRC	1	CY	A = A >> 1; bit 7 = prev bit 0; CY = prev bit 0");
            1
        }
        0x10 => {
            println!(" 	-			");
            1
        }
        0x11 => {
            println!(" 	LXI D,D16	3		D <- byte 3, E <- byte 2");
            3
        }
        0x12 => {
            println!(" 	STAX D	1		(DE) <- A");
            1
        }
        0x13 => {
            println!(" 	INX D	1		DE <- DE + 1");
            1
        }
        0x14 => {
            println!(" 	INR D	1	Z, S, P, AC	D <- D+1");
            1
        }
        0x15 => {
            println!(" 	DCR D	1	Z, S, P, AC	D <- D-1");
            1
        }
        0x16 => {
            println!(" 	MVI D, D8	2		D <- byte 2");
            2
        }
        0x17 => {
            println!(" 	RAL	1	CY	A = A << 1; bit 0 = prev CY; CY = prev bit 7");
            1
        }
        0x18 => {
            println!(" 	-			");
            1
        }
        0x19 => {
            println!(" 	DAD D	1	CY	HL = HL + DE");
            1
        }
        0x1a => {
            println!(" 	LDAX D	1		A <- (DE)");
            1
        }
        0x1b => {
            println!(" 	DCX D	1		DE = DE-1");
            1
        }
        0x1c => {
            println!(" 	INR E	1	Z, S, P, AC	E <-E+1");
            1
        }
        0x1d => {
            println!(" 	DCR E	1	Z, S, P, AC	E <- E-1");
            1
        }
        0x1e => {
            println!(" 	MVI E,D8	2		E <- byte 2");
            2
        }
        0x1f => {
            println!(" 	RAR	1	CY	A = A >> 1; bit 7 = prev bit 7; CY = prev bit 0");
            1
        }
        0x20 => {
            println!(" 	-			");
            1
        }
        0x21 => {
            println!(" 	LXI H,D16	3		H <- byte 3, L <- byte 2");
            3
        }
        0x22 => {
            println!(" 	SHLD adr	3		(adr) <-L; (adr+1)<-H");
            3
        }
        0x23 => {
            println!(" 	INX H	1		HL <- HL + 1");
            1
        }
        0x24 => {
            println!(" 	INR H	1	Z, S, P, AC	H <- H+1");
            1
        }
        0x25 => {
            println!(" 	DCR H	1	Z, S, P, AC	H <- H-1");
            1
        }
        0x26 => {
            println!(" 	MVI H,D8	2		H <- byte 2");
            1
        }
        0x27 => {
            println!(" 	DAA	1		special");
            1
        }
        0x28 => {
            println!(" 	-			");
            1
        }
        0x29 => {
            println!(" 	DAD H	1	CY	HL = HL + HI");
            1
        }
        0x2a => {
            println!(" 	LHLD adr	3		L <- (adr); H<-(adr+1)");
            3
        }
        0x2b => {
            println!(" 	DCX H	1		HL = HL-1");
            1
        }
        0x2c => {
            println!(" 	INR L	1	Z, S, P, AC	L <- L+1");
            1
        }
        0x2d => {
            println!(" 	DCR L	1	Z, S, P, AC	L <- L-1");
            1
        }
        0x2e => {
            println!(" 	MVI L, D8	2		L <- byte 2");
            2
        }
        0x2f => {
            println!(" 	CMA	1		A <- !A");
            1
        }
        0x30 => {
            println!(" 	-			");
            1
        }
        0x31 => {
            println!(" 	LXI SP, D16	3		SP.hi <- byte 3, SP.lo <- byte 2");
            3
        }
        0x32 => {
            println!(" 	STA adr	3		(adr) <- A");
            3
        }
        0x33 => {
            println!(" 	INX SP	1		SP = SP + 1");
            1
        }
        0x34 => {
            println!(" 	INR M	1	Z, S, P, AC	(HL) <- (HL)+1");
            1
        }
        0x35 => {
            println!(" 	DCR M	1	Z, S, P, AC	(HL) <- (HL)-1");
            1
        }
        0x36 => {
            println!(" 	MVI M,D8	2		(HL) <- byte 2");
            2
        }
        0x37 => {
            println!(" 	STC	1	CY	CY = 1");
            1
        }
        0x38 => {
            println!(" 	-			");
            1
        }
        0x39 => {
            println!(" 	DAD SP	1	CY	HL = HL + SP");
            1
        }
        0x3a => {
            println!(" 	LDA adr	3		A <- (adr)");
            3
        }
        0x3b => {
            println!(" 	DCX SP	1		SP = SP-1");
            1
        }
        0x3c => {
            println!(" 	INR A	1	Z, S, P, AC	A <- A+1");
            1
        }
        0x3d => {
            println!(" 	DCR A	1	Z, S, P, AC	A <- A-1");
            1
        }
        0x3e => {
            println!(" 	MVI A,D8	2		A <- byte 2");
            2
        }
        0x3f => {
            println!(" 	CMC	1	CY	CY=!CY");
            1
        }
        0x40 => {
            println!(" 	MOV B,B	1		B <- B");
            1
        }
        0x41 => {
            println!(" 	MOV B,C	1		B <- C");
            1
        }
        0x42 => {
            println!(" 	MOV B,D	1		B <- D");
            1
        }
        0x43 => {
            println!(" 	MOV B,E	1		B <- E");
            1
        }
        0x44 => {
            println!(" 	MOV B,H	1		B <- H");
            1
        }
        0x45 => {
            println!(" 	MOV B,L	1		B <- L");
            1
        }
        0x46 => {
            println!(" 	MOV B,M	1		B <- (HL)");
            1
        }
        0x47 => {
            println!(" 	MOV B,A	1		B <- A");
            1
        }
        0x48 => {
            println!(" 	MOV C,B	1		C <- B");
            1
        }
        0x49 => {
            println!(" 	MOV C,C	1		C <- C");
            1
        }
        0x4a => {
            println!(" 	MOV C,D	1		C <- D");
            1
        }
        0x4b => {
            println!(" 	MOV C,E	1		C <- E");
            1
        }
        0x4c => {
            println!(" 	MOV C,H	1		C <- H");
            1
        }
        0x4d => {
            println!(" 	MOV C,L	1		C <- L");
            1
        }
        0x4e => {
            println!(" 	MOV C,M	1		C <- (HL)");
            1
        }
        0x4f => {
            println!(" 	MOV C,A	1		C <- A");
            1
        }
        0x50 => {
            println!(" 	MOV D,B	1		D <- B");
            1
        }
        0x51 => {
            println!(" 	MOV D,C	1		D <- C");
            1
        }
        0x52 => {
            println!(" 	MOV D,D	1		D <- D");
            1
        }
        0x53 => {
            println!(" 	MOV D,E	1		D <- E");
            1
        }
        0x54 => {
            println!(" 	MOV D,H	1		D <- H");
            1
        }
        0x55 => {
            println!(" 	MOV D,L	1		D <- L");
            1
        }
        0x56 => {
            println!(" 	MOV D,M	1		D <- (HL)");
            1
        }
        0x57 => {
            println!(" 	MOV D,A	1		D <- A");
            1
        }
        0x58 => {
            println!(" 	MOV E,B	1		E <- B");
            1
        }
        0x59 => {
            println!(" 	MOV E,C	1		E <- C");
            1
        }
        0x5a => {
            println!(" 	MOV E,D	1		E <- D");
            1
        }
        0x5b => {
            println!(" 	MOV E,E	1		E <- E");
            1
        }
        0x5c => {
            println!(" 	MOV E,H	1		E <- H");
            1
        }
        0x5d => {
            println!(" 	MOV E,L	1		E <- L");
            1
        }
        0x5e => {
            println!(" 	MOV E,M	1		E <- (HL)");
            1
        }
        0x5f => {
            println!(" 	MOV E,A	1		E <- A");
            1
        }
        0x60 => {
            println!(" 	MOV H,B	1		H <- B");
            1
        }
        0x61 => {
            println!(" 	MOV H,C	1		H <- C");
            1
        }
        0x62 => {
            println!(" 	MOV H,D	1		H <- D");
            1
        }
        0x63 => {
            println!(" 	MOV H,E	1		H <- E");
            1
        }
        0x64 => {
            println!(" 	MOV H,H	1		H <- H");
            1
        }
        0x65 => {
            println!(" 	MOV H,L	1		H <- L");
            1
        }
        0x66 => {
            println!(" 	MOV H,M	1		H <- (HL)");
            1
        }
        0x67 => {
            println!(" 	MOV H,A	1		H <- A");
            1
        }
        0x68 => {
            println!(" 	MOV L,B	1		L <- B");
            1
        }
        0x69 => {
            println!(" 	MOV L,C	1		L <- C");
            1
        }
        0x6a => {
            println!(" 	MOV L,D	1		L <- D");
            1
        }
        0x6b => {
            println!(" 	MOV L,E	1		L <- E");
            1
        }
        0x6c => {
            println!(" 	MOV L,H	1		L <- H");
            1
        }
        0x6d => {
            println!(" 	MOV L,L	1		L <- L");
            1
        }
        0x6e => {
            println!(" 	MOV L,M	1		L <- (HL)");
            1
        }
        0x6f => {
            println!(" 	MOV L,A	1		L <- A");
            1
        }
        0x70 => {
            println!(" 	MOV M,B	1		(HL) <- B");
            1
        }
        0x71 => {
            println!(" 	MOV M,C	1		(HL) <- C");
            1
        }
        0x72 => {
            println!(" 	MOV M,D	1		(HL) <- D");
            1
        }
        0x73 => {
            println!(" 	MOV M,E	1		(HL) <- E");
            1
        }
        0x74 => {
            println!(" 	MOV M,H	1		(HL) <- H");
            1
        }
        0x75 => {
            println!(" 	MOV M,L	1		(HL) <- L");
            1
        }
        0x76 => {
            println!(" 	HLT	1		special");
            1
        }
        0x77 => {
            println!(" 	MOV M,A	1		(HL) <- A");
            1
        }
        0x78 => {
            println!(" 	MOV A,B	1		A <- B");
            1
        }
        0x79 => {
            println!(" 	MOV A,C	1		A <- C");
            1
        }
        0x7a => {
            println!(" 	MOV A,D	1		A <- D");
            1
        }
        0x7b => {
            println!(" 	MOV A,E	1		A <- E");
            1
        }
        0x7c => {
            println!(" 	MOV A,H	1		A <- H");
            1
        }
        0x7d => {
            println!(" 	MOV A,L	1		A <- L");
            1
        }
        0x7e => {
            println!(" 	MOV A,M	1		A <- (HL)");
            1
        }
        0x7f => {
            println!(" 	MOV A,A	1		A <- A");
            1
        }
        0x80 => {
            println!(" 	ADD B	1	Z, S, P, CY, AC	A <- A + B");
            1
        }
        0x81 => {
            println!(" 	ADD C	1	Z, S, P, CY, AC	A <- A + C");
            1
        }
        0x82 => {
            println!(" 	ADD D	1	Z, S, P, CY, AC	A <- A + D");
            1
        }
        0x83 => {
            println!(" 	ADD E	1	Z, S, P, CY, AC	A <- A + E");
            1
        }
        0x84 => {
            println!(" 	ADD H	1	Z, S, P, CY, AC	A <- A + H");
            1
        }
        0x85 => {
            println!(" 	ADD L	1	Z, S, P, CY, AC	A <- A + L");
            1
        }
        0x86 => {
            println!(" 	ADD M	1	Z, S, P, CY, AC	A <- A + (HL)");
            1
        }
        0x87 => {
            println!(" 	ADD A	1	Z, S, P, CY, AC	A <- A + A");
            1
        }
        0x88 => {
            println!(" 	ADC B	1	Z, S, P, CY, AC	A <- A + B + CY");
            1
        }
        0x89 => {
            println!(" 	ADC C	1	Z, S, P, CY, AC	A <- A + C + CY");
            1
        }
        0x8a => {
            println!(" 	ADC D	1	Z, S, P, CY, AC	A <- A + D + CY");
            1
        }
        0x8b => {
            println!(" 	ADC E	1	Z, S, P, CY, AC	A <- A + E + CY");
            1
        }
        0x8c => {
            println!(" 	ADC H	1	Z, S, P, CY, AC	A <- A + H + CY");
            1
        }
        0x8d => {
            println!(" 	ADC L	1	Z, S, P, CY, AC	A <- A + L + CY");
            1
        }
        0x8e => {
            println!(" 	ADC M	1	Z, S, P, CY, AC	A <- A + (HL) + CY");
            1
        }
        0x8f => {
            println!(" 	ADC A	1	Z, S, P, CY, AC	A <- A + A + CY");
            1
        }
        0x90 => {
            println!(" 	SUB B	1	Z, S, P, CY, AC	A <- A - B");
            1
        }
        0x91 => {
            println!(" 	SUB C	1	Z, S, P, CY, AC	A <- A - C");
            1
        }
        0x92 => {
            println!(" 	SUB D	1	Z, S, P, CY, AC	A <- A + D");
            1
        }
        0x93 => {
            println!(" 	SUB E	1	Z, S, P, CY, AC	A <- A - E");
            1
        }
        0x94 => {
            println!(" 	SUB H	1	Z, S, P, CY, AC	A <- A + H");
            1
        }
        0x95 => {
            println!(" 	SUB L	1	Z, S, P, CY, AC	A <- A - L");
            1
        }
        0x96 => {
            println!(" 	SUB M	1	Z, S, P, CY, AC	A <- A + (HL)");
            1
        }
        0x97 => {
            println!(" 	SUB A	1	Z, S, P, CY, AC	A <- A - A");
            1
        }
        0x98 => {
            println!(" 	SBB B	1	Z, S, P, CY, AC	A <- A - B - CY");
            1
        }
        0x99 => {
            println!(" 	SBB C	1	Z, S, P, CY, AC	A <- A - C - CY");
            1
        }
        0x9a => {
            println!(" 	SBB D	1	Z, S, P, CY, AC	A <- A - D - CY");
            1
        }
        0x9b => {
            println!(" 	SBB E	1	Z, S, P, CY, AC	A <- A - E - CY");
            1
        }
        0x9c => {
            println!(" 	SBB H	1	Z, S, P, CY, AC	A <- A - H - CY");
            1
        }
        0x9d => {
            println!(" 	SBB L	1	Z, S, P, CY, AC	A <- A - L - CY");
            1
        }
        0x9e => {
            println!(" 	SBB M	1	Z, S, P, CY, AC	A <- A - (HL) - CY");
            1
        }
        0x9f => {
            println!(" 	SBB A	1	Z, S, P, CY, AC	A <- A - A - CY");
            1
        }
        0xa0 => {
            println!(" 	ANA B	1	Z, S, P, CY, AC	A <- A & B");
            1
        }
        0xa1 => {
            println!(" 	ANA C	1	Z, S, P, CY, AC	A <- A & C");
            1
        }
        0xa2 => {
            println!(" 	ANA D	1	Z, S, P, CY, AC	A <- A & D");
            1
        }
        0xa3 => {
            println!(" 	ANA E	1	Z, S, P, CY, AC	A <- A & E");
            1
        }
        0xa4 => {
            println!(" 	ANA H	1	Z, S, P, CY, AC	A <- A & H");
            1
        }
        0xa5 => {
            println!(" 	ANA L	1	Z, S, P, CY, AC	A <- A & L");
            1
        }
        0xa6 => {
            println!(" 	ANA M	1	Z, S, P, CY, AC	A <- A & (HL)");
            1
        }
        0xa7 => {
            println!(" 	ANA A	1	Z, S, P, CY, AC	A <- A & A");
            1
        }
        0xa8 => {
            println!(" 	XRA B	1	Z, S, P, CY, AC	A <- A ^ B");
            1
        }
        0xa9 => {
            println!(" 	XRA C	1	Z, S, P, CY, AC	A <- A ^ C");
            1
        }
        0xaa => {
            println!(" 	XRA D	1	Z, S, P, CY, AC	A <- A ^ D");
            1
        }
        0xab => {
            println!(" 	XRA E	1	Z, S, P, CY, AC	A <- A ^ E");
            1
        }
        0xac => {
            println!(" 	XRA H	1	Z, S, P, CY, AC	A <- A ^ H");
            1
        }
        0xad => {
            println!(" 	XRA L	1	Z, S, P, CY, AC	A <- A ^ L");
            1
        }
        0xae => {
            println!(" 	XRA M	1	Z, S, P, CY, AC	A <- A ^ (HL)");
            1
        }
        0xaf => {
            println!(" 	XRA A	1	Z, S, P, CY, AC	A <- A ^ A");
            1
        }
        0xb0 => {
            println!(" 	ORA B	1	Z, S, P, CY, AC	A <- A | B");
            1
        }
        0xb1 => {
            println!(" 	ORA C	1	Z, S, P, CY, AC	A <- A | C");
            1
        }
        0xb2 => {
            println!(" 	ORA D	1	Z, S, P, CY, AC	A <- A | D");
            1
        }
        0xb3 => {
            println!(" 	ORA E	1	Z, S, P, CY, AC	A <- A | E");
            1
        }
        0xb4 => {
            println!(" 	ORA H	1	Z, S, P, CY, AC	A <- A | H");
            1
        }
        0xb5 => {
            println!(" 	ORA L	1	Z, S, P, CY, AC	A <- A | L");
            1
        }
        0xb6 => {
            println!(" 	ORA M	1	Z, S, P, CY, AC	A <- A | (HL)");
            1
        }
        0xb7 => {
            println!(" 	ORA A	1	Z, S, P, CY, AC	A <- A | A");
            1
        }
        0xb8 => {
            println!(" 	CMP B	1	Z, S, P, CY, AC	A - B");
            1
        }
        0xb9 => {
            println!(" 	CMP C	1	Z, S, P, CY, AC	A - C");
            1
        }
        0xba => {
            println!(" 	CMP D	1	Z, S, P, CY, AC	A - D");
            1
        }
        0xbb => {
            println!(" 	CMP E	1	Z, S, P, CY, AC	A - E");
            1
        }
        0xbc => {
            println!(" 	CMP H	1	Z, S, P, CY, AC	A - H");
            1
        }
        0xbd => {
            println!(" 	CMP L	1	Z, S, P, CY, AC	A - L");
            1
        }
        0xbe => {
            println!(" 	CMP M	1	Z, S, P, CY, AC	A - (HL)");
            1
        }
        0xbf => {
            println!(" 	CMP A	1	Z, S, P, CY, AC	A - A");
            1
        }
        0xc0 => {
            println!(" 	RNZ	1		if NZ, RET");
            1
        }
        0xc1 => {
            println!(" 	POP B	1		C <- (sp); B <- (sp+1); sp <- sp+2");
            1
        }
        0xc2 => {
            println!(" 	JNZ adr	3		if NZ, PC <- adr");
            3
        }
        0xc3 => {
            println!(" 	JMP adr	3		PC <= adr");
            3
        }
        0xc4 => {
            println!(" 	CNZ adr	3		if NZ, CALL adr");
            3
        }
        0xc5 => {
            println!(" 	PUSH B	1		(sp-2)<-C; (sp-1)<-B; sp <- sp - 2");
            1
        }
        0xc6 => {
            println!(" 	ADI D8	2	Z, S, P, CY, AC	A <- A + byte");
            2
        }
        0xc7 => {
            println!(" 	RST 0	1		CALL $0");
            1
        }
        0xc8 => {
            println!(" 	RZ	1		if Z, RET");
            1
        }
        0xc9 => {
            println!(" 	RET	1		PC.lo <- (sp); PC.hi<-(sp+1); SP <- SP+2");
            1
        }
        0xca => {
            println!(" 	JZ adr	3		if Z, PC <- adr");
            3
        }
        0xcb => {
            println!(" 	-			");
            1
        }
        0xcc => {
            println!(" 	CZ adr	3		if Z, CALL adr");
            3
        }
        0xcd => {
            println!(" 	CALL adr	3		(SP-1)<-PC.hi;(SP-2)<-PC.lo;SP<-SP-2;PC=adr");
            3
        }
        0xce => {
            println!(" 	ACI D8	2	Z, S, P, CY, AC	A <- A + data + CY");
            2
        }
        0xcf => {
            println!(" 	RST 1	1		CALL $8");
            1
        }
        0xd0 => {
            println!(" 	RNC	1		if NCY, RET");
            1
        }
        0xd1 => {
            println!(" 	POP D	1		E <- (sp); D <- (sp+1); sp <- sp+2");
            1
        }
        0xd2 => {
            println!(" 	JNC adr	3		if NCY, PC<-adr");
            3
        }
        0xd3 => {
            println!(" 	OUT D8	2		special");
            2
        }
        0xd4 => {
            println!(" 	CNC adr	3		if NCY, CALL adr");
            3
        }
        0xd5 => {
            println!(" 	PUSH D	1		(sp-2)<-E; (sp-1)<-D; sp <- sp - 2");
            1
        }
        0xd6 => {
            println!(" 	SUI D8	2	Z, S, P, CY, AC	A <- A - data");
            2
        }
        0xd7 => {
            println!(" 	RST 2	1		CALL $10");
            1
        }
        0xd8 => {
            println!(" 	RC	1		if CY, RET");
            1
        }
        0xd9 => {
            println!(" 	-			");
            1
        }
        0xda => {
            println!(" 	JC adr	3		if CY, PC<-adr");
            3
        }
        0xdb => {
            println!(" 	IN D8	2		special");
            2
        }
        0xdc => {
            println!(" 	CC adr	3		if CY, CALL adr");
            3
        }
        0xdd => {
            println!(" 	-			");
            1
        }
        0xde => {
            println!(" 	SBI D8	2	Z, S, P, CY, AC	A <- A - data - CY");
            2
        }
        0xdf => {
            println!(" 	RST 3	1		CALL $18");
            1
        }
        0xe0 => {
            println!(" 	RPO	1		if PO, RET");
            1
        }
        0xe1 => {
            println!(" 	POP H	1		L <- (sp); H <- (sp+1); sp <- sp+2");
            1
        }
        0xe2 => {
            println!(" 	JPO adr	3		if PO, PC <- adr");
            3
        }
        0xe3 => {
            println!(" 	XTHL	1		L <-> (SP); H <-> (SP+1)");
            1
        }
        0xe4 => {
            println!(" 	CPO adr	3		if PO, CALL adr");
            3
        }
        0xe5 => {
            println!(" 	PUSH H	1		(sp-2)<-L; (sp-1)<-H; sp <- sp - 2");
            1
        }
        0xe6 => {
            println!(" 	ANI D8	2	Z, S, P, CY, AC	A <- A & data");
            2
        }
        0xe7 => {
            println!(" 	RST 4	1		CALL $20");
            1
        }
        0xe8 => {
            println!(" 	RPE	1		if PE, RET");
            1
        }
        0xe9 => {
            println!(" 	PCHL	1		PC.hi <- H; PC.lo <- L");
            1
        }
        0xea => {
            println!(" 	JPE adr	3		if PE, PC <- adr");
            3
        }
        0xeb => {
            println!(" 	XCHG	1		H <-> D; L <-> E");
            1
        }
        0xec => {
            println!(" 	CPE adr	3		if PE, CALL adr");
            3
        }
        0xed => {
            println!(" 	-			");
            1
        }
        0xee => {
            println!(" 	XRI D8	2	Z, S, P, CY, AC	A <- A ^ data");
            2
        }
        0xef => {
            println!(" 	RST 5	1		CALL $28");
            1
        }
        0xf0 => {
            println!(" 	RP	1		if P, RET");
            1
        }
        0xf1 => {
            println!(" 	POP PSW	1		flags <- (sp); A <- (sp+1); sp <- sp+2");
            1
        }
        0xf2 => {
            println!(" 	JP adr	3		if P=1 PC <- adr");
            3
        }
        0xf3 => {
            println!(" 	DI	1		special");
            1
        }
        0xf4 => {
            println!(" 	CP adr	3		if P, PC <- adr");
            3
        }
        0xf5 => {
            println!(" 	PUSH PSW	1		(sp-2)<-flags; (sp-1)<-A; sp <- sp - 2");
            1
        }
        0xf6 => {
            println!(" 	ORI D8	2	Z, S, P, CY, AC	A <- A | data");
            2
        }
        0xf7 => {
            println!(" 	RST 6	1		CALL $30");
            1
        }
        0xf8 => {
            println!(" 	RM	1		if M, RET");
            1
        }
        0xf9 => {
            println!(" 	SPHL	1		SP=HL");
            1
        }
        0xfa => {
            println!(" 	JM adr	3		if M, PC <- adr");
            3
        }
        0xfb => {
            println!(" 	EI	1		special");
            1
        }
        0xfc => {
            println!(" 	CM adr	3		if M, CALL adr");
            3
        }
        0xfd => {
            println!(" 	-			");
            1
        }
        0xfe => {
            println!(" 	CPI D8	2	Z, S, P, CY, AC	A - data");
            2
        }
        0xff => {
            println!(" 	RST 7	1		CALL $38");
            1
        }
    }
}
