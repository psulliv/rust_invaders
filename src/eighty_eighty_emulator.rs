#![allow(clippy::unusual_byte_groupings)]
use bitflags::bitflags;
use std::convert::From;
use std::mem;
use std::{
    fmt::Debug,
    ops::{Index, IndexMut},
};

use crate::{debug_utils::debug_print_op_code, space_invaders_rom};

bitflags! {
    struct ConditionFlags: u8 {
    const CY = 0b0000_0001;
    const P =  0b0000_0100;
    const AC = 0b0001_0000;
    const Z =  0b0100_0000;
    const S =  0b1000_0000;
    }
}

#[derive(PartialEq, Debug)]
pub struct ProcessorState {
    reg_a: u8,
    reg_b: u8,
    reg_c: u8,
    reg_d: u8,
    reg_e: u8,
    reg_h: u8,
    reg_l: u8,
    stack_pointer: u16,
    prog_counter: u16,
    flags: ConditionFlags,
}

impl ProcessorState {
    pub fn new() -> Self {
        ProcessorState {
            reg_a: 0,
            reg_b: 0,
            reg_c: 0,
            reg_d: 0,
            reg_e: 0,
            reg_h: 0,
            reg_l: 0,
            stack_pointer: 0x2400,
            prog_counter: 0,
            flags: ConditionFlags {
                bits: (0b0000_0000),
            },
        }
    }

    /// This uses the register pair bits from the opcode to
    /// load an address into a register pair
    /// 00 B-C
    /// 01 D-E
    /// 10 H-L
    /// 11 SP (not really a pair)
    pub fn set_rp(&mut self, data: u16, pair: RPairBitPattern) {
        match pair {
            RPairBitPattern::BC => {
                self.reg_b = (data >> 8) as u8;
                self.reg_c = data as u8;
            }
            RPairBitPattern::DE => {
                self.reg_d = (data >> 8) as u8;
                self.reg_e = data as u8;
            }
            RPairBitPattern::HL => {
                self.reg_h = (data >> 8) as u8;
                self.reg_l = data as u8;
            }
            RPairBitPattern::SP => {
                self.stack_pointer = data;
            }
        }
    }

    /// gets the contents of the rp as a u16, probably for use as an address
    pub fn get_rp(&mut self, pair: RPairBitPattern) -> u16 {
        match pair {
            RPairBitPattern::BC => two_le_eights_to_one_sixteen(self.reg_c, self.reg_b),
            RPairBitPattern::DE => two_le_eights_to_one_sixteen(self.reg_e, self.reg_d),
            RPairBitPattern::HL => two_le_eights_to_one_sixteen(self.reg_l, self.reg_h),
            RPairBitPattern::SP => self.stack_pointer,
        }
    }

    /// Pushes an address to the stack
    pub fn push_address(&mut self, mem_map: &mut MemMap, address: u16) {
        mem_map[self.stack_pointer - 1] = ((address) >> 8) as u8;
        mem_map[self.stack_pointer - 2] = address as u8;
        self.stack_pointer -= 2;
    }

    /// Push one byte to the stack
    pub fn push_byte(&mut self, mem_map: &mut MemMap, this_data: u8) {
        mem_map[self.stack_pointer - 1] = this_data;
        self.stack_pointer -= 1;
    }

    /// Pops an address from the stack
    pub fn pop_address(&mut self, mem_map: &mut MemMap) -> u16 {
        let mut address = (mem_map[self.stack_pointer + 1] as u16) << 8;
        address |= mem_map[self.stack_pointer] as u16;
        self.stack_pointer += 2;
        address
    }

    /// Pop one byte from the stack
    pub fn pop_byte(&mut self, mem_map: &mut MemMap) -> u8 {
        let this_data = mem_map[self.stack_pointer];
        self.stack_pointer += 1;
        this_data
    }

    /// Checks an instruction's condition mode against the condition flags
    pub fn check_condition(&self, condition: ConditionBitPattern) -> bool {
        match condition {
            ConditionBitPattern::NZ => !(self.flags.contains(ConditionFlags::Z)),
            ConditionBitPattern::Z => self.flags.contains(ConditionFlags::Z),
            ConditionBitPattern::NC => !(self.flags.contains(ConditionFlags::CY)),
            ConditionBitPattern::C => self.flags.contains(ConditionFlags::CY),
            ConditionBitPattern::PO => !(self.flags.contains(ConditionFlags::P)),
            ConditionBitPattern::PE => self.flags.contains(ConditionFlags::P),
            ConditionBitPattern::P => !(self.flags.contains(ConditionFlags::S)),
            ConditionBitPattern::M => self.flags.contains(ConditionFlags::S),
        }
    }
}

pub struct MemMap {
    rom: [u8; space_invaders_rom::SPACE_INVADERS_ROM.len()],
    rw_mem: Vec<u8>,
}

impl MemMap {
    // Todo: Replace space invaders specific memory layout with
    // configurable map.
    pub fn new() -> Self {
        MemMap {
            rom: space_invaders_rom::SPACE_INVADERS_ROM,
            rw_mem: vec![0; 4096],
        }
    }
}

impl Index<u16> for MemMap {
    // Todo: Replace space invaders specific memory layout with
    // configurable map.
    type Output = u8;
    fn index(&self, idx: u16) -> &Self::Output {
        if idx < (space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) {
            &self.rom[idx as usize]
        } else {
            &self.rw_mem[(idx - space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) as usize]
        }
    }
}

impl IndexMut<u16> for MemMap {
    // Todo: Replace space invaders specific memory layout with
    // configurable map.
    fn index_mut(&mut self, idx: u16) -> &mut Self::Output {
        if idx < (space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) {
            if cfg!(test) {
                &mut self.rom[idx as usize]
            } else {
                panic!("Attempted to mutate ROM");
            }
        } else {
            &mut self.rw_mem[(idx - space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) as usize]
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum RPairBitPattern {
    BC = 0b00,
    DE = 0b01,
    HL = 0b10,
    SP = 0b11,
}

impl From<u8> for RPairBitPattern {
    fn from(this_byte: u8) -> Self {
        match this_byte {
            0b00 => RPairBitPattern::BC,
            0b01 => RPairBitPattern::DE,
            0b10 => RPairBitPattern::HL,
            0b11 => RPairBitPattern::SP,
            _ => panic!("Failed to parse u8 to RPairBitPattern"),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum RegisterBitPattern {
    A = 0b111,
    B = 0b000,
    C = 0b001,
    D = 0b010,
    E = 0b011,
    H = 0b100,
    L = 0b101,
    // Not really a register but pattern is used that way, sometimes means memory
    // sometimes means its another direction of operation.
    Other = 0b110,
}

impl From<u8> for RegisterBitPattern {
    fn from(this_byte: u8) -> Self {
        match this_byte {
            0b111 => RegisterBitPattern::A,
            0b000 => RegisterBitPattern::B,
            0b001 => RegisterBitPattern::C,
            0b010 => RegisterBitPattern::D,
            0b011 => RegisterBitPattern::E,
            0b100 => RegisterBitPattern::H,
            0b101 => RegisterBitPattern::L,
            0b110 => RegisterBitPattern::Other,
            _ => panic!("Failed to parse u8 to RegisterBitPattern"),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum ConditionBitPattern {
    NZ = 0b000,
    Z = 0b001,
    NC = 0b010,
    C = 0b011,
    PO = 0b100,
    PE = 0b101,
    P = 0b110,
    M = 0b111,
}

impl From<u8> for ConditionBitPattern {
    fn from(this_byte: u8) -> Self {
        match this_byte {
            0b000 => ConditionBitPattern::NZ,
            0b001 => ConditionBitPattern::Z,
            0b010 => ConditionBitPattern::NC,
            0b011 => ConditionBitPattern::C,
            0b100 => ConditionBitPattern::PO,
            0b101 => ConditionBitPattern::PE,
            0b110 => ConditionBitPattern::P,
            0b111 => ConditionBitPattern::M,
            _ => panic!("failed to parse u8 to ConditionBitPattern"),
        }
    }
}

/// Take two u8 in little end order and put them in a single u16
fn two_le_eights_to_one_sixteen(first_byte: u8, second_byte: u8) -> u16 {
    (second_byte as u16) << 8 | first_byte as u16
}

/// returns the register pair specified by bits 5 and 4.
fn get_register_pair_bit_pattern(cur_instruction: u8) -> RPairBitPattern {
    ((cur_instruction & 0b00_11_0000) >> 4).into()
}

/// returns the condition mode specified by bits 5, 4, and 3
fn get_condition_bit_pattern(cur_instruction: u8) -> ConditionBitPattern {
    ((cur_instruction & 0b00_111_000) >> 3).into()
}

/// returns register bits defined by bits 2,1, and 0
fn get_source_register_bit_pattern(cur_instruction: u8) -> RegisterBitPattern {
    (cur_instruction & 0b00_000_111).into()
}

/// returns register bits defined by bits 5, 4, and 3
fn get_destination_register_bit_pattern(cur_instruction: u8) -> RegisterBitPattern {
    ((cur_instruction & 0b00_111_000) >> 3).into()
}

/// match statement for operation decoding
pub fn iterate_processor_state(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // get the next opcode at the current program counter
    let cur_instruction = mem_map[state.prog_counter];
    debug_print_op_code(cur_instruction);
    match cur_instruction {
        0x00 => {
            opcode_nop(state);
        }
        0x01 => {
            opcode_lxi(state, mem_map);
        }
        0x02 => {
            opcode_stax(state, mem_map);
        }
        0x03 => {
            opcode_inx(state, mem_map);
            // 1
        }
        0x04 => {
            panic!("    INR B   1       Z, S, P, AC     B <- B+1");
            // 1
        }
        0x05 => {
            panic!("    DCR B   1       Z, S, P, AC     B <- B-1");
            // 1
        }
        0x06 => {
            opcode_mvi(state, mem_map);
        }
        0x07 => {
            panic!("    RLC     1       CY      A = A << 1; bit 0 = prev bit 7; CY = prev bit 7");
            // 1
        }
        0x08 => {
            panic!("    -                       ");
            // 1
        }
        0x09 => {
            panic!("    DAD B   1       CY      HL = HL + BC");
            // 1
        }
        0x0a => {
            opcode_ldax(state, mem_map);
            // 1
        }
        0x0b => {
            panic!("    DCX B   1               BC = BC-1");
            // 1
        }
        0x0c => {
            panic!("    INR C   1       Z, S, P, AC     C <- C+1");
            // 1
        }
        0x0d => {
            panic!("    DCR C   1       Z, S, P, AC     C <-C-1");
            // 1
        }
        0x0e => {
            opcode_mvi(state, mem_map);
        }
        0x0f => {
            panic!("    RRC     1       CY      A = A >> 1; bit 7 = prev bit 0; CY = prev bit 0");
            // 1
        }
        0x10 => {
            panic!("    -                       ");
            // 1
        }
        0x11 => {
            opcode_lxi(state, mem_map);
        }
        0x12 => {
            opcode_stax(state, mem_map);
        }
        0x13 => {
            opcode_inx(state, mem_map);
            // 1
        }
        0x14 => {
            panic!("    INR D   1       Z, S, P, AC     D <- D+1");
            // 1
        }
        0x15 => {
            panic!("    DCR D   1       Z, S, P, AC     D <- D-1");
            // 1
        }
        0x16 => {
            opcode_mvi(state, mem_map);
        }
        0x17 => {
            panic!("    RAL     1       CY      A = A << 1; bit 0 = prev CY; CY = prev bit 7");
            // 1
        }
        0x18 => {
            panic!("    -                       ");
            // 1
        }
        0x19 => {
            panic!("    DAD D   1       CY      HL = HL + DE");
            // 1
        }
        0x1a => {
            opcode_ldax(state, mem_map);
        }
        0x1b => {
            panic!("    DCX D   1               DE = DE-1");
            // 1
        }
        0x1c => {
            panic!("    INR E   1       Z, S, P, AC     E <-E+1");
            // 1
        }
        0x1d => {
            panic!("    DCR E   1       Z, S, P, AC     E <- E-1");
            // 1
        }
        0x1e => {
            opcode_mvi(state, mem_map);
        }
        0x1f => {
            panic!("    RAR     1       CY      A = A >> 1; bit 7 = prev bit 7; CY = prev bit 0");
            // 1
        }
        0x20 => {
            panic!("    -                       ");
            // 1
        }
        0x21 => {
            opcode_lxi(state, mem_map);
        }
        0x22 => {
            opcode_shld(state, mem_map);
        }
        0x23 => {
            opcode_inx(state, mem_map);
            // 1
        }
        0x24 => {
            panic!("    INR H   1       Z, S, P, AC     H <- H+1");
            // 1
        }
        0x25 => {
            panic!("    DCR H   1       Z, S, P, AC     H <- H-1");
            // 1
        }
        0x26 => {
            opcode_mvi(state, mem_map);
        }
        0x27 => {
            panic!("    DAA     1               special");
            // 1
        }
        0x28 => {
            panic!("    -                       ");
            // 1
        }
        0x29 => {
            panic!("    DAD H   1       CY      HL = HL + HI");
            // 1
        }
        0x2a => {
            opcode_lhld(state, mem_map);
        }
        0x2b => {
            panic!("    DCX H   1               HL = HL-1");
            // 1
        }
        0x2c => {
            panic!("    INR L   1       Z, S, P, AC     L <- L+1");
            // 1
        }
        0x2d => {
            panic!("    DCR L   1       Z, S, P, AC     L <- L-1");
            // 1
        }
        0x2e => {
            opcode_mvi(state, mem_map);
        }
        0x2f => {
            panic!("    CMA     1               A <- !A");
            // 1
        }
        0x30 => {
            panic!("    -                       ");
            // 1
        }
        0x31 => {
            opcode_lxi(state, mem_map);
        }
        0x32 => {
            opcode_sta(state, mem_map);
        }
        0x33 => {
            opcode_inx(state, mem_map);
            // 1
        }
        0x34 => {
            panic!("    INR M   1       Z, S, P, AC     (HL) <- (HL)+1");
            // 1
        }
        0x35 => {
            panic!("    DCR M   1       Z, S, P, AC     (HL) <- (HL)-1");
            // 1
        }
        0x36 => {
            opcode_mvi(state, mem_map);
        }
        0x37 => {
            panic!("    STC     1       CY      CY = 1");
            // 1
        }
        0x38 => {
            panic!("    -                       ");
            // 1
        }
        0x39 => {
            panic!("    DAD SP  1       CY      HL = HL + SP");
            // 1
        }
        0x3a => {
            opcode_lda(state, mem_map);
        }
        0x3b => {
            panic!("    DCX SP  1               SP = SP-1");
            // 1
        }
        0x3c => {
            panic!("    INR A   1       Z, S, P, AC     A <- A+1");
            // 1
        }
        0x3d => {
            panic!("    DCR A   1       Z, S, P, AC     A <- A-1");
            // 1
        }
        0x3e => {
            opcode_mvi(state, mem_map);
        }
        0x3f => {
            panic!("    CMC     1       CY      CY=!CY");
            // 1
        }
        0x40 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x41 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x42 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x43 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x44 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x45 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x46 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x47 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x48 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x49 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x4a => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x4b => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x4c => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x4d => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x4e => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x4f => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x50 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x51 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x52 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x53 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x54 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x55 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x56 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x57 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x58 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x59 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x5a => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x5b => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x5c => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x5d => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x5e => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x5f => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x60 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x61 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x62 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x63 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x64 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x65 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x66 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x67 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x68 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x69 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x6a => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x6b => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x6c => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x6d => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x6e => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x6f => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x70 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x71 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x72 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x73 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x74 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x75 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x76 => {
            panic!("    HLT     1               special");
            // 1
        }
        0x77 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x78 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x79 => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x7a => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x7b => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x7c => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x7d => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x7e => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x7f => {
            opcode_mov(state, mem_map);
            // 1
        }
        0x80 => {
            panic!("    ADD B   1       Z, S, P, CY, AC A <- A + B");
            // 1
        }
        0x81 => {
            panic!("    ADD C   1       Z, S, P, CY, AC A <- A + C");
            // 1
        }
        0x82 => {
            panic!("    ADD D   1       Z, S, P, CY, AC A <- A + D");
            // 1
        }
        0x83 => {
            panic!("    ADD E   1       Z, S, P, CY, AC A <- A + E");
            // 1
        }
        0x84 => {
            panic!("    ADD H   1       Z, S, P, CY, AC A <- A + H");
            // 1
        }
        0x85 => {
            panic!("    ADD L   1       Z, S, P, CY, AC A <- A + L");
            // 1
        }
        0x86 => {
            panic!("    ADD M   1       Z, S, P, CY, AC A <- A + (HL)");
            // 1
        }
        0x87 => {
            panic!("    ADD A   1       Z, S, P, CY, AC A <- A + A");
            // 1
        }
        0x88 => {
            panic!("    ADC B   1       Z, S, P, CY, AC A <- A + B + CY");
            // 1
        }
        0x89 => {
            panic!("    ADC C   1       Z, S, P, CY, AC A <- A + C + CY");
            // 1
        }
        0x8a => {
            panic!("    ADC D   1       Z, S, P, CY, AC A <- A + D + CY");
            // 1
        }
        0x8b => {
            panic!("    ADC E   1       Z, S, P, CY, AC A <- A + E + CY");
            // 1
        }
        0x8c => {
            panic!("    ADC H   1       Z, S, P, CY, AC A <- A + H + CY");
            // 1
        }
        0x8d => {
            panic!("    ADC L   1       Z, S, P, CY, AC A <- A + L + CY");
            // 1
        }
        0x8e => {
            panic!("    ADC M   1       Z, S, P, CY, AC A <- A + (HL) + CY");
            // 1
        }
        0x8f => {
            panic!("    ADC A   1       Z, S, P, CY, AC A <- A + A + CY");
            // 1
        }
        0x90 => {
            panic!("    SUB B   1       Z, S, P, CY, AC A <- A - B");
            // 1
        }
        0x91 => {
            panic!("    SUB C   1       Z, S, P, CY, AC A <- A - C");
            // 1
        }
        0x92 => {
            panic!("    SUB D   1       Z, S, P, CY, AC A <- A + D");
            // 1
        }
        0x93 => {
            panic!("    SUB E   1       Z, S, P, CY, AC A <- A - E");
            // 1
        }
        0x94 => {
            panic!("    SUB H   1       Z, S, P, CY, AC A <- A + H");
            // 1
        }
        0x95 => {
            panic!("    SUB L   1       Z, S, P, CY, AC A <- A - L");
            // 1
        }
        0x96 => {
            panic!("    SUB M   1       Z, S, P, CY, AC A <- A + (HL)");
            // 1
        }
        0x97 => {
            panic!("    SUB A   1       Z, S, P, CY, AC A <- A - A");
            // 1
        }
        0x98 => {
            panic!("    SBB B   1       Z, S, P, CY, AC A <- A - B - CY");
            // 1
        }
        0x99 => {
            panic!("    SBB C   1       Z, S, P, CY, AC A <- A - C - CY");
            // 1
        }
        0x9a => {
            panic!("    SBB D   1       Z, S, P, CY, AC A <- A - D - CY");
            // 1
        }
        0x9b => {
            panic!("    SBB E   1       Z, S, P, CY, AC A <- A - E - CY");
            // 1
        }
        0x9c => {
            panic!("    SBB H   1       Z, S, P, CY, AC A <- A - H - CY");
            // 1
        }
        0x9d => {
            panic!("    SBB L   1       Z, S, P, CY, AC A <- A - L - CY");
            // 1
        }
        0x9e => {
            panic!("    SBB M   1       Z, S, P, CY, AC A <- A - (HL) - CY");
            // 1
        }
        0x9f => {
            panic!("    SBB A   1       Z, S, P, CY, AC A <- A - A - CY");
            // 1
        }
        0xa0 => {
            panic!("    ANA B   1       Z, S, P, CY, AC A <- A & B");
            // 1
        }
        0xa1 => {
            panic!("    ANA C   1       Z, S, P, CY, AC A <- A & C");
            // 1
        }
        0xa2 => {
            panic!("    ANA D   1       Z, S, P, CY, AC A <- A & D");
            // 1
        }
        0xa3 => {
            panic!("    ANA E   1       Z, S, P, CY, AC A <- A & E");
            // 1
        }
        0xa4 => {
            panic!("    ANA H   1       Z, S, P, CY, AC A <- A & H");
            // 1
        }
        0xa5 => {
            panic!("    ANA L   1       Z, S, P, CY, AC A <- A & L");
            // 1
        }
        0xa6 => {
            panic!("    ANA M   1       Z, S, P, CY, AC A <- A & (HL)");
            // 1
        }
        0xa7 => {
            panic!("    ANA A   1       Z, S, P, CY, AC A <- A & A");
            // 1
        }
        0xa8 => {
            panic!("    XRA B   1       Z, S, P, CY, AC A <- A ^ B");
            // 1
        }
        0xa9 => {
            panic!("    XRA C   1       Z, S, P, CY, AC A <- A ^ C");
            // 1
        }
        0xaa => {
            panic!("    XRA D   1       Z, S, P, CY, AC A <- A ^ D");
            // 1
        }
        0xab => {
            panic!("    XRA E   1       Z, S, P, CY, AC A <- A ^ E");
            // 1
        }
        0xac => {
            panic!("    XRA H   1       Z, S, P, CY, AC A <- A ^ H");
            // 1
        }
        0xad => {
            panic!("    XRA L   1       Z, S, P, CY, AC A <- A ^ L");
            // 1
        }
        0xae => {
            panic!("    XRA M   1       Z, S, P, CY, AC A <- A ^ (HL)");
            // 1
        }
        0xaf => {
            panic!("    XRA A   1       Z, S, P, CY, AC A <- A ^ A");
            // 1
        }
        0xb0 => {
            panic!("    ORA B   1       Z, S, P, CY, AC A <- A | B");
            // 1
        }
        0xb1 => {
            panic!("    ORA C   1       Z, S, P, CY, AC A <- A | C");
            // 1
        }
        0xb2 => {
            panic!("    ORA D   1       Z, S, P, CY, AC A <- A | D");
            // 1
        }
        0xb3 => {
            panic!("    ORA E   1       Z, S, P, CY, AC A <- A | E");
            // 1
        }
        0xb4 => {
            panic!("    ORA H   1       Z, S, P, CY, AC A <- A | H");
            // 1
        }
        0xb5 => {
            panic!("    ORA L   1       Z, S, P, CY, AC A <- A | L");
            // 1
        }
        0xb6 => {
            panic!("    ORA M   1       Z, S, P, CY, AC A <- A | (HL)");
            // 1
        }
        0xb7 => {
            panic!("    ORA A   1       Z, S, P, CY, AC A <- A | A");
            // 1
        }
        0xb8 => {
            panic!("    CMP B   1       Z, S, P, CY, AC A - B");
            // 1
        }
        0xb9 => {
            panic!("    CMP C   1       Z, S, P, CY, AC A - C");
            // 1
        }
        0xba => {
            panic!("    CMP D   1       Z, S, P, CY, AC A - D");
            // 1
        }
        0xbb => {
            panic!("    CMP E   1       Z, S, P, CY, AC A - E");
            // 1
        }
        0xbc => {
            panic!("    CMP H   1       Z, S, P, CY, AC A - H");
            // 1
        }
        0xbd => {
            panic!("    CMP L   1       Z, S, P, CY, AC A - L");
            // 1
        }
        0xbe => {
            panic!("    CMP M   1       Z, S, P, CY, AC A - (HL)");
            // 1
        }
        0xbf => {
            panic!("    CMP A   1       Z, S, P, CY, AC A - A");
            // 1
        }
        0xc0 => {
            opcode_ret(state, mem_map);
        }
        0xc1 => {
            opcode_pop(state, mem_map);
        }
        0xc2 => {
            opcode_jmp(state, mem_map);
            // 3
        }
        0xc3 => {
            opcode_jmp(state, mem_map);
        }
        0xc4 => {
            opcode_call(state, mem_map);
        }
        0xc5 => {
            opcode_push(state, mem_map);
        }
        0xc6 => {
            panic!("    ADI D8  2       Z, S, P, CY, AC A <- A + byte");
            // 2
        }
        0xc7 => {
            opcode_rst(state, mem_map);
        }
        0xc8 => {
            opcode_ret(state, mem_map);
        }
        0xc9 => {
            opcode_ret(state, mem_map);
            // 1
        }
        0xca => {
            opcode_jmp(state, mem_map);
            // 3
        }
        0xcb => {
            panic!("    -                       ");
            // 1
        }
        0xcc => {
            opcode_call(state, mem_map);
        }
        0xcd => {
            opcode_call(state, mem_map);
        }
        0xce => {
            panic!("    ACI D8  2       Z, S, P, CY, AC A <- A + data + CY");
            // 2
        }
        0xcf => {
            opcode_rst(state, mem_map);
        }
        0xd0 => {
            opcode_ret(state, mem_map);
        }
        0xd1 => {
            opcode_pop(state, mem_map);
        }
        0xd2 => {
            opcode_jmp(state, mem_map);
            // 3
        }
        0xd3 => {
            panic!("    OUT D8  2               special");
            // 2
        }
        0xd4 => {
            opcode_call(state, mem_map);
        }
        0xd5 => {
            opcode_push(state, mem_map);
        }
        0xd6 => {
            panic!("    SUI D8  2       Z, S, P, CY, AC A <- A - data");
            // 2
        }
        0xd7 => {
            opcode_rst(state, mem_map);
        }
        0xd8 => {
            opcode_ret(state, mem_map);
        }
        0xd9 => {
            panic!("    -                       ");
            // 1
        }
        0xda => {
            opcode_jmp(state, mem_map);
            // 3
        }
        0xdb => {
            panic!("    IN D8   2               special");
            // 2
        }
        0xdc => {
            opcode_call(state, mem_map);
        }
        0xdd => {
            panic!("    -                       ");
            // 1
        }
        0xde => {
            panic!("    SBI D8  2       Z, S, P, CY, AC A <- A - data - CY");
            // 2
        }
        0xdf => {
            opcode_rst(state, mem_map);
        }
        0xe0 => {
            opcode_ret(state, mem_map);
        }
        0xe1 => {
            opcode_pop(state, mem_map);
        }
        0xe2 => {
            opcode_jmp(state, mem_map);
            // 3
        }
        0xe3 => {
            opcode_xthl(state, mem_map);
        }
        0xe4 => {
            opcode_call(state, mem_map);
        }
        0xe5 => {
            opcode_push(state, mem_map);
        }
        0xe6 => {
            panic!("    ANI D8  2       Z, S, P, CY, AC A <- A & data");
            // 2
        }
        0xe7 => {
            opcode_rst(state, mem_map);
        }
        0xe8 => {
            opcode_ret(state, mem_map);
        }
        0xe9 => {
            opcode_pchl(state, mem_map);
        }
        0xea => {
            opcode_jmp(state, mem_map);
            // 3
        }
        0xeb => {
            opcode_xchg(state, mem_map);
        }
        0xec => {
            opcode_call(state, mem_map);
        }
        0xed => {
            panic!("    -                       ");
            // 1
        }
        0xee => {
            panic!("    XRI D8  2       Z, S, P, CY, AC A <- A ^ data");
            // 2
        }
        0xef => {
            opcode_rst(state, mem_map);
        }
        0xf0 => {
            opcode_ret(state, mem_map);
        }
        0xf1 => {
            opcode_pop(state, mem_map);
        }
        0xf2 => {
            opcode_jmp(state, mem_map);
            // 3
        }
        0xf3 => {
            panic!("    DI      1               special");
            // 1
        }
        0xf4 => {
            opcode_call(state, mem_map);
        }
        0xf5 => {
            opcode_push(state, mem_map);
        }
        0xf6 => {
            panic!("    ORI D8  2       Z, S, P, CY, AC A <- A | data");
            // 2
        }
        0xf7 => {
            opcode_rst(state, mem_map);
        }
        0xf8 => {
            opcode_ret(state, mem_map);
        }
        0xf9 => {
            opcode_sphl(state, mem_map);
        }
        0xfa => {
            opcode_jmp(state, mem_map);
            // 3
        }
        0xfb => {
            panic!("    EI      1               special");
            // 1
        }
        0xfc => {
            opcode_call(state, mem_map);
        }
        0xfd => {
            panic!("    -                       ");
            // 1
        }
        0xfe => {
            panic!("    CPI D8  2       Z, S, P, CY, AC A - data");
            // 2
        }
        0xff => {
            opcode_rst(state, mem_map);
        }
    };
    println!("{:?}", state);
}

/// CALL,CNZ, CZ, CNC, CC, CPO, CPE, CP, CM addr
/// ((SP) - 1) ~ (PCH) [program counter high bits]
/// ((SP) - 2) ~ (PCl) [program counter low bits]
/// (SP) ~ (SP) - 2 [reduce current stack pointer by two]
/// (PC) ~ (byte 3) (byte 2) [move byte 3 and 2 to the program counter]
/// The high-order eight bits of the next instruction ad-
/// dress are moved to the memory location whose
/// address is one less than the content of register SP.
/// The low-order eight bits of the next instruction ad-
/// dress are moved to the memory location whose
/// address is two less than the content of register SP.
/// The content of register SP is decremented by 2. Con-
/// trol is transferred to the instruction whose address is
/// specified in byte 3 and byte 2 of the current
/// instruction.
fn opcode_call(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let second_byte = mem_map[state.prog_counter + 1];
    let third_byte = mem_map[state.prog_counter + 2];
    let condition = get_condition_bit_pattern(cur_instruction);
    let c_type = cur_instruction & 0b00_000_111;
    if c_type == 0b101 || state.check_condition(condition) {
        state.push_address(mem_map, state.prog_counter);
        state.prog_counter = two_le_eights_to_one_sixteen(second_byte, third_byte);
    } else {
        state.prog_counter += 3;
    }
}

/// NOP (No op)
/// No operation is performed. The registers and flags
/// are unaffected.
fn opcode_nop(state: &mut ProcessorState) {
    state.prog_counter += 1;
}

/// INX (increment register pair)
/// (rh) (rl) ..- (rh) (rl) + 1
/// The content of the register pair rp is incremented by
/// one. Note: No condition flags are affected
fn opcode_inx(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let rp_bits = get_register_pair_bit_pattern(cur_instruction);
    let rp_16 = state.get_rp(rp_bits);
    state.set_rp(rp_16 + 1, rp_bits);
    state.prog_counter += 1;
}

/// JMP addr (Jump)
/// (PC) ~ (byte 3) (byte 2)
/// Control is transferred to the instruction whose ad-
/// dress is specified in byte 3 and byte 2 of the current
/// instruction.
fn opcode_jmp(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let second_byte = mem_map[state.prog_counter + 1];
    let third_byte = mem_map[state.prog_counter + 2];
    let j_type = cur_instruction & 0b00_000_111;
    let condition = get_condition_bit_pattern(cur_instruction);
    if j_type == 0b011 || state.check_condition(condition) {
        let address = two_le_eights_to_one_sixteen(second_byte, third_byte);
        state.prog_counter = address;
    } else {
        state.prog_counter += 3;
    }
}

/// LDAX rp (load accumulator direct)
/// (A)~ ((rp))
/// The content of the memory location, whose address
/// is in the register pair rp, is moved to register A. Note:
/// only register pairs rp=B (registers B and C·) or rp=D
/// (registers D and E) may be specified.
fn opcode_ldax(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let rp_bits = get_register_pair_bit_pattern(cur_instruction);
    match rp_bits {
        RPairBitPattern::BC => {
            let address = (state.reg_b as u16) << 8 | state.reg_c as u16;
            state.reg_a = mem_map[address];
        }
        RPairBitPattern::DE => {
            let address = (state.reg_d as u16) << 8 | state.reg_e as u16;
            state.reg_a = mem_map[address];
        }
        _ => {
            panic!("unhandled register pair bits in opcode_ldax");
        }
    }
    state.prog_counter += 1;
}

/// STAX rp (Store accumulator indirect)
/// ((rp)) ~ (A)
/// The content of register A is moved to the memory lo-
/// cation whose address is in the register pair rp. Note:
/// only register pairs rp=B (registers B and C) or rp=D
/// (registers D and E) may be specified.
fn opcode_stax(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let rp_bits = get_register_pair_bit_pattern(cur_instruction);
    match rp_bits {
        RPairBitPattern::BC => {
            // register pair B-C
            let address = (state.reg_b as u16) << 8 | state.reg_c as u16;
            mem_map[address] = state.reg_a;
        }
        RPairBitPattern::DE => {
            // register pair D-E
            let address = (state.reg_d as u16) << 8 | state.reg_e as u16;
            mem_map[address] = state.reg_a;
        }
        _ => {
            panic!("unhandled register pair bits in opcode_ldax");
        }
    }
    state.prog_counter += 1;
}

/// XCHG (Exchange H and L with D and E)
/// (H)~(D)
/// (L)~(E)
/// The contents of registers Hand L are exchanged with
/// the contents of registers D and E.
fn opcode_xchg(state: &mut ProcessorState, _mem_map: &mut MemMap) {
    mem::swap(&mut state.reg_d, &mut state.reg_h);
    mem::swap(&mut state.reg_e, &mut state.reg_l);
    state.prog_counter += 1;
}

/// LXI rp, data 16 (Load register pair immediate)
/// (rh) ~ (byte 3),
/// (rl) ~ (byte 2)
/// Byte 3 of the instruction is moved into the high-order
/// register (rh) of the register pair rp. Byte 2 of the in-
/// struction is moved into the low-order register (rl) of
/// the register pair rp.
fn opcode_lxi(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // panic!("         LXI SP, D16     3               SP.hi <- byte 3, SP.lo <- byte 2");

    // This handles several register pairs so we will match against it's opcode
    // 0 | 0 | R | P | 0 | 0 | 0 | 1
    let cur_instruction = mem_map[state.prog_counter];
    let second_byte = mem_map[state.prog_counter + 1];
    let third_byte = mem_map[state.prog_counter + 2];
    // Masking and shifting so that we can use this as a match that looks similar
    // to the RP legend in the book.
    let rp_bits = get_register_pair_bit_pattern(cur_instruction);
    state.prog_counter += 3;
    match rp_bits {
        RPairBitPattern::BC => {
            // register pair B-C
            state.reg_b = third_byte;
            state.reg_c = second_byte;
        }
        RPairBitPattern::DE => {
            // register pair D-E
            state.reg_d = third_byte;
            state.reg_e = second_byte;
        }
        RPairBitPattern::HL => {
            // register pair H-L
            state.reg_h = third_byte;
            state.reg_l = second_byte;
        }
        RPairBitPattern::SP => {
            // register SP
            state.stack_pointer = (third_byte as u16) << 8 | (second_byte as u16);
        }
    }
}

/// LOA addr (Load Accumulator direct)
/// (A) ~ ((byte 3) (byte 2))
/// The content of the memory location, whose address
/// is specified in byte 2 and byte 3 of the instruction, is
/// moved to register A.
fn opcode_lda(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let second_byte = mem_map[state.prog_counter + 1];
    let third_byte = mem_map[state.prog_counter + 2];
    state.reg_a = mem_map[two_le_eights_to_one_sixteen(second_byte, third_byte)];
    state.prog_counter += 3;
}

/// STA addr (Store Accumulator direct)
/// ((byte 3)(byte 2)) ~ (A)
/// The content of the accumulator is moved to the
/// memory location whose address is specified in byte
/// 2 and byte 3 of the instruction.
fn opcode_sta(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let second_byte = mem_map[state.prog_counter + 1];
    let third_byte = mem_map[state.prog_counter + 2];
    mem_map[two_le_eights_to_one_sixteen(second_byte, third_byte)] = state.reg_a;
    state.prog_counter += 3;
}

/// LHLD addr (Load Hand L direct)
/// (L) ~ ((byte 3)(byte 2))
/// (H) ~ ((byte 3) (byte 2) + 1)
/// The content of the memory location, whose address
/// is specified in byte 2 and byte 3 of the instruction, is
/// moved to register L. The content of the memory loca-
/// tion at the succeeding address is moved to register H.
fn opcode_lhld(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let second_byte = mem_map[state.prog_counter + 1];
    let third_byte = mem_map[state.prog_counter + 2];
    let address = two_le_eights_to_one_sixteen(second_byte, third_byte);
    state.reg_l = mem_map[address];
    state.reg_h = mem_map[address + 1];
    state.prog_counter += 3;
}

/// SHLD addr (Store Hand L direct)
/// ((byte 3) (byte 2)) ~ (L)
/// ((byte 3)(byte 2) + 1) ~ (H)
/// The content of register L is moved to the memory lo-
/// cation whose address is specified in byte 2 and byte
/// 3. The content of register H is moved to the succeed-
/// ing memory location.
fn opcode_shld(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let second_byte = mem_map[state.prog_counter + 1];
    let third_byte = mem_map[state.prog_counter + 2];
    let address = two_le_eights_to_one_sixteen(second_byte, third_byte);
    mem_map[address] = state.reg_l;
    mem_map[address + 1] = state.reg_h;
    state.prog_counter += 3;
}

/// MOV r, M (Move from memory)
/// (r) ~ ((H) (L))
/// The content of the memory location, whose address
/// is in registers H and L, is moved to register r.
/// MOV M, r (Move to memory)
/// ((H)(L))~ (r)
/// The content of register r is moved to the memory lo-
/// cation whose address is in registers Hand L.
fn opcode_mov(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let dest = get_destination_register_bit_pattern(cur_instruction);
    let src = get_source_register_bit_pattern(cur_instruction);
    if dest == RegisterBitPattern::Other {
        match src {
            // A
            RegisterBitPattern::A => {
                state.reg_a = mem_map[state.get_rp(RPairBitPattern::HL)];
            }
            // B
            RegisterBitPattern::B => {
                state.reg_b = mem_map[state.get_rp(RPairBitPattern::HL)];
            }
            // C
            RegisterBitPattern::C => {
                state.reg_c = mem_map[state.get_rp(RPairBitPattern::HL)];
            }
            // D
            RegisterBitPattern::D => {
                state.reg_d = mem_map[state.get_rp(RPairBitPattern::HL)];
            }
            // E
            RegisterBitPattern::E => {
                state.reg_e = mem_map[state.get_rp(RPairBitPattern::HL)];
            }
            // H
            RegisterBitPattern::H => {
                state.reg_h = mem_map[state.get_rp(RPairBitPattern::HL)];
            }
            // L
            RegisterBitPattern::L => {
                state.reg_l = mem_map[state.get_rp(RPairBitPattern::HL)];
            }
            RegisterBitPattern::Other => {
                panic!("Requested impossible mov operation, mem to mem");
            }
        }
    } else if src == RegisterBitPattern::Other {
        // then this is a 0 | 1 | D | D | D | 1 | 1 | 0
        // format opcode, use dest
        match dest {
            // A
            RegisterBitPattern::A => {
                mem_map[state.get_rp(RPairBitPattern::HL)] = state.reg_a;
            }
            // B
            RegisterBitPattern::B => {
                mem_map[state.get_rp(RPairBitPattern::HL)] = state.reg_b;
            }
            // C
            RegisterBitPattern::C => {
                mem_map[state.get_rp(RPairBitPattern::HL)] = state.reg_c;
            }
            // D
            RegisterBitPattern::D => {
                mem_map[state.get_rp(RPairBitPattern::HL)] = state.reg_d;
            }
            // E
            RegisterBitPattern::E => {
                mem_map[state.get_rp(RPairBitPattern::HL)] = state.reg_e;
            }
            // H
            RegisterBitPattern::H => {
                mem_map[state.get_rp(RPairBitPattern::HL)] = state.reg_h;
            }
            // L
            RegisterBitPattern::L => {
                mem_map[state.get_rp(RPairBitPattern::HL)] = state.reg_l;
            }
            RegisterBitPattern::Other => {
                panic!("Requested impossible mov operation, mem to mem");
            }
        }
    } else {
        let byte_to_move = match src {
            RegisterBitPattern::A => state.reg_a,
            // B
            RegisterBitPattern::B => state.reg_b,
            // C
            RegisterBitPattern::C => state.reg_c,
            // D
            RegisterBitPattern::D => state.reg_d,
            // E
            RegisterBitPattern::E => state.reg_e,
            // H
            RegisterBitPattern::H => state.reg_h,
            // L
            RegisterBitPattern::L => state.reg_l,
            RegisterBitPattern::Other => {
                panic!("Programming error, should have been caught by earlier condition");
            }
        };
        match dest {
            // A
            RegisterBitPattern::A => {
                state.reg_a = byte_to_move;
            }
            // B
            RegisterBitPattern::B => {
                state.reg_b = byte_to_move;
            }
            // C
            RegisterBitPattern::C => {
                state.reg_c = byte_to_move;
            }
            // D
            RegisterBitPattern::D => {
                state.reg_d = byte_to_move;
            }
            // E
            RegisterBitPattern::E => {
                state.reg_e = byte_to_move;
            }
            // H
            RegisterBitPattern::H => {
                state.reg_h = byte_to_move;
            }
            // L
            RegisterBitPattern::L => {
                state.reg_l = byte_to_move;
            }
            RegisterBitPattern::Other => {
                panic!("Programming error should have been caught earlier");
            }
        }
    }
    state.prog_counter += 1;
}

/// MVI r, data (Move Immediate)
/// (r) ~ (byte 2)
/// The content of byte 2 of the instruction is moved to
/// register r.
/// MVI M, data (Move to memory immediate)
/// ((H) (L)) ~ (byte 2)
/// The content of byte 2 of the instruction is moved to
/// the memory location whose address is in registers H
/// and L.
fn opcode_mvi(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let dest = get_destination_register_bit_pattern(cur_instruction);
    let second_byte = mem_map[state.prog_counter + 1];
    state.prog_counter += 2;
    match dest {
        RegisterBitPattern::A => {
            state.reg_a = second_byte;
        }
        RegisterBitPattern::B => {
            state.reg_b = second_byte;
        }
        RegisterBitPattern::C => {
            state.reg_c = second_byte;
        }
        RegisterBitPattern::D => {
            state.reg_d = second_byte;
        }
        RegisterBitPattern::E => {
            state.reg_e = second_byte;
        }
        RegisterBitPattern::H => {
            state.reg_h = second_byte;
        }
        RegisterBitPattern::L => {
            state.reg_l = second_byte;
        }
        RegisterBitPattern::Other => {
            let address = state.get_rp(RPairBitPattern::HL);
            mem_map[address] = second_byte;
        }
    }
}

/// RET (Return)
/// (PCl) ~ ((SP));
/// (PCH) ~ ((SP) + 1);
/// (SP) ~ (SP) + 2;
/// The content of the memory location whose address
/// is specified in register SP is moved to the low-order
/// eight bits of register PC. The content of the memory
/// location whose address is one more than the content
/// of register SP is moved to the high-order eight bits of
/// register PC. The content of register SP is incremented
/// by 2.
fn opcode_ret(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let r_type = cur_instruction & 0b00_000_111;
    let condition = get_condition_bit_pattern(cur_instruction);
    if r_type == 0b001 || state.check_condition(condition) {
        state.prog_counter = state.pop_address(mem_map);
    } else {
        state.prog_counter += 1;
    }
}

/// RST n (Restart)
/// ((SP) - 1) ~ (PCH)
/// ((SP) - 2) ~ (PCl)
/// (SP) ~ (SP) - 2
/// (PC) ~ 8* (NNN)
/// The high-order eight bits of the next instruction ad-
/// dress are moved to the memory location whose
/// address is one less than the content of register SP.
/// The low-order eight bits of the next instruction ad-
/// dress are moved to the memory location whose
/// address is two less than the content of register SP.
/// The content of register SP is decremented by two.
/// Control is transferred to the instruction whose ad-
/// dress is eight times the content of NNN.
fn opcode_rst(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];

    // kind of lazy but the call address is essentially this mask on the
    // instruction, it's really NNN * 8, buts that really a 3 bit left
    // shift and it's already three bits to the left if you mask it.
    let rst_address = (cur_instruction & 0b00_111_000) as u16;
    state.push_address(mem_map, state.prog_counter);
    state.prog_counter = rst_address;
}

/// PCHL (Jump Hand l indirect - move Hand L to PC)
/// (PCH) ~ (H)
/// (PCl) ~ (l)
/// The content of register H is moved to the high-order
/// eight bits of register PC. The content of register l is
/// moved to the low-order eight bits of register PC.
fn opcode_pchl(state: &mut ProcessorState, _mem_map: &mut MemMap) {
    state.prog_counter = state.get_rp(RPairBitPattern::HL);
}

/// PUSH rp (Push)
/// ((SP) - 1) ~ (rh)
/// ((SP) - 2) ~ (rl)
/// (SP) ~ (SP) - 2
/// The content of the high-order register of register pair
/// rp is moved to the memory location whose address is
/// one less than the content of register SP. The content
/// of the low-order register of register pair rp is moved
/// to the memory location whose address is two less
/// than the content of register SP. The cont~nt of reg-
/// ister SP is decremented by 2. Note: Register pair
/// rp = SP may not be specified ..
fn opcode_push(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let rpair = get_register_pair_bit_pattern(cur_instruction);
    match rpair {
        RPairBitPattern::BC => {
            let this_data = state.get_rp(rpair);
            state.push_address(mem_map, this_data);
        }
        RPairBitPattern::DE => {
            let this_data = state.get_rp(rpair);
            state.push_address(mem_map, this_data);
        }
        RPairBitPattern::HL => {
            let this_data = state.get_rp(rpair);
            state.push_address(mem_map, this_data);
        }
        RPairBitPattern::SP => {
            // Not really SP this is used for pushing the condition flags
            // and the accumulator
            let the_flags = state.flags.bits;
            let the_accumulator = state.reg_a;
            state.push_byte(mem_map, the_accumulator);
            state.push_byte(mem_map, the_flags);
        }
    }
    state.prog_counter += 1;
}

/// POP rp (Pop)
/// (rl) ~((SP))
/// (rh) ~ ((SP) + 1)
/// (SP) ~ (SP) + 2
/// The content of the memory location, whose address
/// is specified by the content of register SP, is moved to
/// the low-order register of register pair rp. The content
/// of the memory location, whose address is one more
/// than the content of register SP, is moved to the high-
/// order register of register pair rp. The content of reg-
/// ister SP is incremented by 2. Note: Register pair
/// rp =SP may not be specified
fn opcode_pop(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let rpair = get_register_pair_bit_pattern(cur_instruction);
    match rpair {
        RPairBitPattern::BC => {
            let this_data = state.pop_address(mem_map);
            state.set_rp(this_data, RPairBitPattern::BC)
        }
        RPairBitPattern::DE => {
            let this_data = state.pop_address(mem_map);
            state.set_rp(this_data, RPairBitPattern::DE)
        }
        RPairBitPattern::HL => {
            let this_data = state.pop_address(mem_map);
            state.set_rp(this_data, RPairBitPattern::HL)
        }
        RPairBitPattern::SP => {
            // Not really SP this is used for popping the condition flags and accumulator
            let the_flags = state.pop_byte(mem_map);
            state.flags = ConditionFlags { bits: (the_flags) };
            let the_accumulator = state.pop_byte(mem_map);
            state.reg_a = the_accumulator;
        }
    }

    state.prog_counter += 1;
}

/// XTHL (Exchange stack top with H and L)
/// (L) ~((SP))
/// (H) ~ ((SP) + 1)
/// The content of the L register is exchanged with the
/// content of the memory location whose address is
/// specified by the content of register SP. The content
/// of the H register is exchanged with the content of the
/// memory location whose address is one more than the
/// content of register SP
fn opcode_xthl(state: &mut ProcessorState, mem_map: &mut MemMap) {
    mem::swap(&mut state.reg_l, &mut mem_map[state.stack_pointer]);
    mem::swap(&mut state.reg_h, &mut mem_map[state.stack_pointer + 1]);
    state.prog_counter += 1;
}

/// SPHL (Move HL to SP)
/// (SP) ~ (H) (L)
/// The contents of registers Hand L (16 bits) are moved
/// to register SP.
fn opcode_sphl(state: &mut ProcessorState, _mem_map: &mut MemMap) {
    state.stack_pointer = state.get_rp(RPairBitPattern::HL);
    state.prog_counter += 1;
}

// Todo: swap out register pair register condition flag and other bit twiddling
// and masking with functions.

// Todo: remove extra match branches in the iterate state function
// since most of these opcodes use bits in the instruction to determine
// register source and destinations this can be pared down to many fewer
// operations and match arms. Instead of checking for equality in these arms
// we would check for certain bits to be set, e.g. LXI instructions.

// Todo: After implementing the operations necessary for space invaders to play
// (or all of them) remove the braces in the match arms so that the return from
// the opcode_<instruction> function goes straight to the match arm. Also, if we
// don't implement certain instructions (aren't needed to play the game) we
// should remove their instructions from the match arms and include a catch all
// instruction that panics

// Todo: figure out a way to make certain pieces of the test setup common like python's
// <TestCase>.setUp(). Avoid making the tests reliant on portions of the code base
// that they are testing however. IE don't replace the bits added to test ROM code
// with bits that we are using as enums, if both of those are wrong in the same
// way it masks a failure.

// Todo: Make the variable names, the tests, and the memory portion generic over
// potential other 8080 emulation uses. When the IO ports come around as something
// to implement come up with some sort of API to access those. This may be impossible
// since we couldn't really take a ROM input given our platform of web assembly but
// that can certainly be a stretch goal.

// Todo: replace the panics with Err valuse that aggregate into a good error message at a single
// panic! near the top and handle the errors that can be handled. Most of this shouldn't be
// able to be handled though since there aren't external deps, disk usage, network
// usage, additional threads to contend, or even memory usage outside the initial allocations.

// somehow integrate memory layout and configuration into the processor state, maybe add
// them under an overarching machine state? Maybe just integrate the `emulate_loop`
// `iterate_state` and whatnot with a struct and method but leave the underlying impl
// imperativeish?

#[cfg(test)]
mod tests {
    use super::*;
    use crate::space_invaders_rom;

    #[test]
    fn basic_nop_step() {
        let mut test_state = ProcessorState::new();
        let cur_instruction_address = test_state.prog_counter;
        let mut si_mem = MemMap::new();
        let test_rom = [0; 8192];
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, cur_instruction_address + 1);
    }

    #[test]
    fn basic_jmp_step() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b1100_0011;
        let test_address: u16 = 0x0020;
        test_rom[1] = test_address as u8;
        test_rom[2] = (test_address >> 8) as u8;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_address, test_state.prog_counter);
    }

    #[test]
    fn jnz_step() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // jnz 0b11_000_010
        test_rom[0] = 0b11_000_010;
        // last address in the space
        let test_address: u16 = (test_rom.len() - 1) as u16;
        // putting something easy to recognize at the end
        test_rom[test_address as usize] = 0xff;
        // insert address in bytes 1 and 2 in little endian byte order
        // we are explicitly truncating the bits by masking off as u8 here
        // the high order bits and casting as u8
        test_rom[1] = test_address as u8;
        test_rom[2] = (test_address >> 8) as u8;
        si_mem.rom = test_rom;

        // assert not equal
        test_state.flags.set(ConditionFlags::Z, true);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_ne!(test_state.prog_counter, test_address);

        // assert equal
        test_state.prog_counter = 0;
        test_state.flags.set(ConditionFlags::Z, false);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, test_address);
    }

    #[test]
    fn jz_step() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // jz 0b11_001_010
        test_rom[0] = 0b11_001_010;
        // last address in the space
        let test_address: u16 = (test_rom.len() - 1) as u16;
        // putting something easy to recognize at the end
        test_rom[test_address as usize] = 0xff;
        // insert address in bytes 1 and 2 in little endian byte order
        // we are explicitly truncating the bits by masking off as u8 here
        // the high order bits and casting as u8
        test_rom[1] = test_address as u8;
        test_rom[2] = (test_address >> 8) as u8;
        si_mem.rom = test_rom;

        // assert not equal
        test_state.flags.set(ConditionFlags::Z, false);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_ne!(test_state.prog_counter, test_address);

        // assert equal
        test_state.prog_counter = 0;
        test_state.flags.set(ConditionFlags::Z, true);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, test_address);
    }

    #[test]
    fn lxi_step_bc() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair b-c is 00
        test_rom[0] = (0b00 << 4) | 0b0000_0001;
        test_rom[1] = 0xff;
        test_rom[2] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_b, test_rom[2]);
        assert_eq!(test_state.reg_c, test_rom[1]);
    }

    #[test]
    fn lxi_step_de() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair d-e is 01
        test_rom[0] = (0b01 << 4) | 0b0000_0001;
        test_rom[1] = 0xff;
        test_rom[2] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_d, test_rom[2]);
        assert_eq!(test_state.reg_e, test_rom[1]);
    }
    #[test]
    fn lxi_step_hl() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair h-l is 10
        test_rom[0] = (0b10 << 4) | 0b0000_0001;
        test_rom[1] = 0xff;
        test_rom[2] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_h, test_rom[2]);
        assert_eq!(test_state.reg_l, test_rom[1]);
    }

    #[test]
    fn lxi_step_sp() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register sp is 11
        test_rom[0] = (0b11 << 4) | 0b0000_0001;
        test_rom[1] = 0xff;
        test_rom[2] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(
            test_state.stack_pointer,
            ((test_rom[2] as u16) << 8) | (test_rom[1] as u16)
        );
    }

    #[test]
    fn lxi_step_good_pc() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register sp is 11
        test_rom[0] = (0b11 << 4) | 0b0000_0001;
        test_rom[1] = 0xff;
        test_rom[2] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, 3);
    }

    #[test]
    fn mvi_step_reg_a() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 111 is a
        test_rom[0] = 0b00_000_110 | (0b111 << 3);
        test_rom[1] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_a, 0xff);
    }

    #[test]
    fn mvi_step_reg_b() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 000 is b
        test_rom[0] = 0b00_000_110 | (0b000 << 3);
        test_rom[1] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_b, 0xff);
    }

    #[test]
    fn mvi_step_reg_c() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 001 is c
        test_rom[0] = 0b00_000_110 | (0b001 << 3);
        test_rom[1] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_c, 0xff);
    }

    #[test]
    fn mvi_step_reg_d() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 010 is d
        test_rom[0] = 0b00_000_110 | (0b010 << 3);
        test_rom[1] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_d, 0xff);
    }

    #[test]
    fn mvi_step_reg_e() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 011 is e
        test_rom[0] = 0b00_000_110 | (0b011 << 3);
        test_rom[1] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_e, 0xff);
    }

    #[test]
    fn mvi_step_reg_h() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 100 is h
        test_rom[0] = 0b00_000_110 | (0b100 << 3);
        test_rom[1] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_h, 0xff);
    }

    #[test]
    fn mvi_step_reg_l() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 010 is l
        test_rom[0] = 0b00_000_110 | (0b101 << 3);
        test_rom[1] = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_l, 0xff);
    }

    #[test]
    fn mvi_step_mem() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 110 is memory
        test_rom[0] = 0b00_000_110 | (0b110 << 3);
        test_rom[1] = 0xff;
        test_state.reg_h = ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        test_state.reg_l = ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(
            si_mem[space_invaders_rom::SPACE_INVADERS_ROM.len() as u16],
            0xff
        );
    }

    #[test]
    fn mvi_step_good_pc() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 110 is memory
        test_rom[0] = 0b00_000_110 | (0b110 << 3);
        test_rom[1] = 0xff;
        test_state.reg_h = ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        test_state.reg_l = ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, 2);
    }

    #[test]
    fn call_uncon() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        let orig_stack_add = test_state.stack_pointer;
        // 0b1100_1101 is unconditional call
        test_rom[0] = 0b1100_1101;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, 0x0020);
        assert_eq!(test_state.stack_pointer, orig_stack_add - 2);
    }
    #[test]
    fn call_nz() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        let orig_stack_add = test_state.stack_pointer;
        // 0b11_000_100 is cnz
        test_rom[0] = 0b11_000_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        si_mem.rom = test_rom;
        test_state.flags.set(ConditionFlags::Z, true);
        iterate_processor_state(&mut test_state, &mut si_mem);

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(test_state.prog_counter, 0x0020);
        assert_ne!(test_state.stack_pointer, orig_stack_add - 2);
        test_state.prog_counter = 0;
        test_state.flags.set(ConditionFlags::Z, false);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, 0x0020);
        assert_eq!(test_state.stack_pointer, orig_stack_add - 2);
    }

    #[test]
    fn call_z() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        let orig_stack_add = test_state.stack_pointer;
        // 0b11_001_100 is cz
        test_rom[0] = 0b11_001_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        si_mem.rom = test_rom;
        test_state.flags.set(ConditionFlags::Z, false);
        iterate_processor_state(&mut test_state, &mut si_mem);

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(test_state.prog_counter, 0x0020);
        assert_ne!(test_state.stack_pointer, orig_stack_add - 2);
        test_state.prog_counter = 0;
        test_state.flags.set(ConditionFlags::Z, true);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, 0x0020);
        assert_eq!(test_state.stack_pointer, orig_stack_add - 2);
    }

    #[test]
    fn call_nc() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        let orig_stack_add = test_state.stack_pointer;
        // 0b11_010_100 is cnc
        test_rom[0] = 0b11_010_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        si_mem.rom = test_rom;
        test_state.flags.set(ConditionFlags::CY, true);
        iterate_processor_state(&mut test_state, &mut si_mem);

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(test_state.prog_counter, 0x0020);
        assert_ne!(test_state.stack_pointer, orig_stack_add - 2);
        test_state.prog_counter = 0;
        test_state.flags.set(ConditionFlags::CY, false);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, 0x0020);
        assert_eq!(test_state.stack_pointer, orig_stack_add - 2);
    }

    #[test]
    fn call_c() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        let orig_stack_add = test_state.stack_pointer;
        // 0b11_011_100 is cc
        test_rom[0] = 0b11_011_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        si_mem.rom = test_rom;
        test_state.flags.set(ConditionFlags::CY, false);
        iterate_processor_state(&mut test_state, &mut si_mem);

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(test_state.prog_counter, 0x0020);
        assert_ne!(test_state.stack_pointer, orig_stack_add - 2);
        test_state.prog_counter = 0;
        test_state.flags.set(ConditionFlags::CY, true);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, 0x0020);
        assert_eq!(test_state.stack_pointer, orig_stack_add - 2);
    }
    #[test]
    fn call_po() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        let orig_stack_add = test_state.stack_pointer;
        // 0b11_100_100 is cpo
        test_rom[0] = 0b11_100_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        si_mem.rom = test_rom;
        test_state.flags.set(ConditionFlags::P, true);
        iterate_processor_state(&mut test_state, &mut si_mem);

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(test_state.prog_counter, 0x0020);
        assert_ne!(test_state.stack_pointer, orig_stack_add - 2);
        test_state.prog_counter = 0;
        test_state.flags.set(ConditionFlags::P, false);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, 0x0020);
        assert_eq!(test_state.stack_pointer, orig_stack_add - 2);
    }
    #[test]
    fn call_pe() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        let orig_stack_add = test_state.stack_pointer;
        // 0b11_101_100 is cpo
        test_rom[0] = 0b11_101_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        si_mem.rom = test_rom;
        test_state.flags.set(ConditionFlags::P, false);
        iterate_processor_state(&mut test_state, &mut si_mem);

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(test_state.prog_counter, 0x0020);
        assert_ne!(test_state.stack_pointer, orig_stack_add - 2);
        test_state.prog_counter = 0;
        test_state.flags.set(ConditionFlags::P, true);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, 0x0020);
        assert_eq!(test_state.stack_pointer, orig_stack_add - 2);
    }
    #[test]
    fn call_p() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        let orig_stack_add = test_state.stack_pointer;
        // 0b11_110_100 is cp
        test_rom[0] = 0b11_110_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        si_mem.rom = test_rom;
        test_state.flags.set(ConditionFlags::S, true);
        iterate_processor_state(&mut test_state, &mut si_mem);

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(test_state.prog_counter, 0x0020);
        assert_ne!(test_state.stack_pointer, orig_stack_add - 2);
        test_state.prog_counter = 0;
        test_state.flags.set(ConditionFlags::S, false);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, 0x0020);
        assert_eq!(test_state.stack_pointer, orig_stack_add - 2);
    }
    #[test]
    fn call_m() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        let orig_stack_add = test_state.stack_pointer;
        // 0b11_111_100 is cp
        test_rom[0] = 0b11_111_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        si_mem.rom = test_rom;
        test_state.flags.set(ConditionFlags::S, false);
        iterate_processor_state(&mut test_state, &mut si_mem);

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(test_state.prog_counter, 0x0020);
        assert_ne!(test_state.stack_pointer, orig_stack_add - 2);
        test_state.prog_counter = 0;
        test_state.flags.set(ConditionFlags::S, true);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.prog_counter, 0x0020);
        assert_eq!(test_state.stack_pointer, orig_stack_add - 2);
    }

    #[test]
    fn ldax_bc() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair b-c is 00
        test_rom[0] = 0b00_00_1010;
        let some_rando_address = 0x0020;
        test_rom[some_rando_address] = 0xff;
        test_state.reg_b = (some_rando_address >> 8) as u8;
        test_state.reg_c = some_rando_address as u8;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_a, 0xff);
    }

    #[test]
    fn ldax_de() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair d-e is 01
        test_rom[0] = 0b00_01_1010;
        let some_rando_address = 0x0020;
        test_rom[some_rando_address] = 0xff;
        test_state.reg_d = (some_rando_address >> 8) as u8;
        test_state.reg_e = some_rando_address as u8;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_a, 0xff);
    }

    #[test]
    fn verify_getset_rp_bc() {
        let mut test_state = ProcessorState::new();
        let some_address: u16 = 0xfffe;
        test_state.set_rp(some_address, 0b00.into());
        assert_eq!(test_state.get_rp(0b00.into()), some_address);
    }

    #[test]
    fn verify_getset_rp_de() {
        let mut test_state = ProcessorState::new();
        let some_address: u16 = 0xfffe;
        test_state.set_rp(some_address, 0b01.into());
        assert_eq!(test_state.get_rp(0b01.into()), some_address);
    }

    #[test]
    fn verify_getset_rp_hl() {
        let mut test_state = ProcessorState::new();
        let some_address: u16 = 0xfffe;
        test_state.set_rp(some_address, 0b10.into());
        assert_eq!(test_state.get_rp(0b10.into()), some_address);
    }

    #[test]
    fn verify_getset_rp_sp() {
        let mut test_state = ProcessorState::new();
        let some_address: u16 = 0xfffe;
        test_state.set_rp(some_address, 0b11.into());
        assert_eq!(test_state.get_rp(0b11.into()), some_address);
    }

    #[test]
    fn verify_push() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        test_state.push_address(&mut si_mem, 0xfffe);
        let address_in_stack = ((si_mem[test_state.stack_pointer + 1] as u16) << 8)
            | si_mem[test_state.stack_pointer] as u16;
        assert_eq!(0xfffe, address_in_stack);
    }

    #[test]
    fn verify_pop() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        test_state.push_address(&mut si_mem, 0xfffe);
        assert_eq!(0xfffe, test_state.pop_address(&mut si_mem));
    }

    #[test]
    fn mov_a_mem() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair d-e is 01
        test_rom[0] = 0b01_110_111;
        let some_rando_address = 0x0020;
        test_state.reg_a = 0xff;
        test_state.reg_h = (some_rando_address >> 8) as u8;
        test_state.reg_l = some_rando_address as u8;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_a, si_mem[some_rando_address]);
    }

    #[test]
    fn mov_mem_a() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair d-e is 01
        test_rom[0] = 0b01_110_111;
        let some_rando_address = 0x0020;
        test_rom[some_rando_address] = 0xff;
        test_state.reg_h = (some_rando_address >> 8) as u8;
        test_state.reg_l = some_rando_address as u8;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_a, 0xff);
    }

    #[test]
    fn mov_a_b() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair d-e is 01
        test_rom[0] = 0b01_111_000;
        test_state.reg_b = 0xff;
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_a, 0xff);
    }

    #[test]
    fn ret_uncon() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        let original_stack_pointer = test_state.stack_pointer;
        test_rom[0] = 0b11_001_001;
        si_mem.rom = test_rom;
        test_state.push_address(&mut si_mem, 0x0020);
        assert_ne!(0x0020, test_state.prog_counter);
        assert_ne!(original_stack_pointer, test_state.stack_pointer);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(0x0020, test_state.prog_counter);
        assert_eq!(original_stack_pointer, test_state.stack_pointer);
    }

    #[test]
    fn rz() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b11_001_000;
        si_mem.rom = test_rom;
        test_state.push_address(&mut si_mem, 0x0020);
        test_state.flags.set(ConditionFlags::Z, false);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_ne!(0x0020, test_state.prog_counter);
        test_state.prog_counter = 0;
        test_state.flags.set(ConditionFlags::Z, true);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(0x0020, test_state.prog_counter);
    }

    #[test]
    fn rst() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        // RST to location 001, address 1000;
        test_rom[0] = 0b11_001_111;
        si_mem.rom = test_rom;
        assert_ne!(0b1000, test_state.prog_counter);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(0b1000, test_state.prog_counter);
    }

    #[test]
    fn pchl() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        // RST to location 001, address 1000;
        test_rom[0] = 0b11_101_001;
        si_mem.rom = test_rom;
        test_state.set_rp(0x0020, RPairBitPattern::HL);
        assert_ne!(0x0020, test_state.prog_counter);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(0x0020, test_state.prog_counter);
        assert_ne!(test_state.prog_counter, 0);
    }
    #[test]
    fn push_byte() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        test_state.flags.set(ConditionFlags::Z, true);
        test_state.flags.set(ConditionFlags::CY, true);
        let orig_stack_pointer = test_state.stack_pointer;
        test_state.push_byte(&mut si_mem, test_state.flags.bits);
        let mut test_flags = ConditionFlags {
            bits: (0b0000_0000),
        };
        test_flags.set(ConditionFlags::Z, true);
        test_flags.set(ConditionFlags::CY, true);
        assert_eq!(si_mem[test_state.stack_pointer], test_flags.bits);
        assert_ne!(test_state.stack_pointer, orig_stack_pointer);
    }

    #[test]
    fn pop_byte() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        test_state.flags.set(ConditionFlags::Z, true);
        test_state.flags.set(ConditionFlags::CY, true);
        test_state.push_byte(&mut si_mem, test_state.flags.bits);
        let orig_stack_pointer = test_state.stack_pointer;
        let mut test_flags = ConditionFlags {
            bits: (0b0000_0000),
        };
        test_flags.set(ConditionFlags::Z, true);
        test_flags.set(ConditionFlags::CY, true);
        assert_eq!(test_state.pop_byte(&mut si_mem), test_flags.bits);
        assert_ne!(test_state.stack_pointer, orig_stack_pointer);
    }

    #[test]
    fn push_pop_op_bc() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        // Push opcode for bc
        test_rom[0] = 0b11_00_0101;
        // Pop opcode for bc
        test_rom[1] = 0b11_00_0001;
        let original_stack_pointer = test_state.stack_pointer;

        test_state.set_rp(0xfffe, RPairBitPattern::BC);

        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_ne!(test_state.prog_counter, 0);
        assert_ne!(original_stack_pointer, test_state.stack_pointer);
        test_state.reg_b = 0;
        test_state.reg_c = 0;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_ne!(test_state.prog_counter, 1);
        assert_eq!(test_state.get_rp(RPairBitPattern::BC), 0xfffe);
        assert_eq!(original_stack_pointer, test_state.stack_pointer);
    }

    #[test]
    fn push_pop_op_a_flags() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        // Push opcode for flags and acc
        test_rom[0] = 0b11_11_0101;
        // Pop opcode for flags and acc
        test_rom[1] = 0b11_11_0001;
        let original_stack_pointer = test_state.stack_pointer;

        test_state.reg_a = 0xfe;
        test_state.flags.set(ConditionFlags::Z, true);
        test_state.flags.set(ConditionFlags::CY, true);
        test_state.flags.set(ConditionFlags::S, true);

        let mut test_flags = ConditionFlags {
            bits: (0b0000_0000),
        };
        test_flags.set(ConditionFlags::Z, true);
        test_flags.set(ConditionFlags::CY, true);
        test_flags.set(ConditionFlags::S, true);
        si_mem.rom = test_rom;
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_ne!(test_state.prog_counter, 0);
        assert_ne!(original_stack_pointer, test_state.stack_pointer);
        test_state.reg_a = 0;
        test_state.flags = ConditionFlags {
            bits: (0b0000_0000),
        };
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_a, 0xfe);
        assert_eq!(test_state.flags, test_flags);
        assert_eq!(original_stack_pointer, test_state.stack_pointer);
        assert_ne!(test_state.prog_counter, 1);
    }

    #[test]
    fn xthl() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b11_100_011;
        si_mem.rom = test_rom;
        test_state.reg_h = 0xde;
        test_state.reg_l = 0xad;
        test_state.push_address(&mut si_mem, 0xbeef);

        assert_ne!(test_state.get_rp(RPairBitPattern::HL), 0xbeef);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.get_rp(RPairBitPattern::HL), 0xbeef);
        assert_ne!(test_state.prog_counter, 0);
    }

    #[test]
    fn sphl() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b11_111_001;
        si_mem.rom = test_rom;
        test_state.reg_h = 0xde;
        test_state.reg_l = 0xad;

        assert_ne!(
            test_state.get_rp(RPairBitPattern::HL),
            test_state.stack_pointer
        );
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(
            test_state.get_rp(RPairBitPattern::HL),
            test_state.stack_pointer
        );
        assert_ne!(test_state.prog_counter, 0);
    }

    #[test]
    fn lda() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b00_111_010;
        let some_address: u16 = 0x1337;

        test_rom[1] = some_address as u8;
        test_rom[2] = (some_address >> 8) as u8;
        si_mem.rom = test_rom;
        si_mem[some_address] = 0xfe;

        assert_ne!(test_state.reg_a, 0xfe);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_a, 0xfe);
        assert_ne!(test_state.prog_counter, 0);
    }

    #[test]
    fn sta() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b00_110_010;
        let some_address: u16 = 0x1337;

        test_rom[1] = some_address as u8;
        test_rom[2] = (some_address >> 8) as u8;
        si_mem.rom = test_rom;
        test_state.reg_a = 0xfe;

        assert_ne!(si_mem[some_address], 0xfe);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(si_mem[some_address], 0xfe);
        assert_ne!(test_state.prog_counter, 0);
    }

    #[test]
    fn lhld() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b00_101_010;
        let some_address: u16 = 0x1337;

        test_rom[1] = some_address as u8;
        test_rom[2] = (some_address >> 8) as u8;
        si_mem.rom = test_rom;
        si_mem[some_address] = 0xbe;
        si_mem[some_address + 1] = 0xef;

        assert_ne!(test_state.reg_l, 0xbe);
        assert_ne!(test_state.reg_h, 0xef);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_l, 0xbe);
        assert_eq!(test_state.reg_h, 0xef);
        assert_ne!(test_state.prog_counter, 0);
    }

    #[test]
    fn shld() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b00_100_010;
        let some_address: u16 = 0x1337;
        test_rom[1] = some_address as u8;
        test_rom[2] = (some_address >> 8) as u8;
        test_state.reg_l = 0xbe;
        test_state.reg_h = 0xef;
        si_mem.rom = test_rom;

        assert_ne!(si_mem[some_address], 0xbe);
        assert_ne!(si_mem[some_address + 1], 0xef);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(si_mem[some_address], 0xbe);
        assert_eq!(si_mem[some_address + 1], 0xef);
        assert_ne!(test_state.prog_counter, 0);
    }

    #[test]
    fn stax() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        // Testing rp BC
        test_rom[0] = 0b00_00_0010;
        let some_address: u16 = 0x1337;
        test_state.reg_c = some_address as u8;
        test_state.reg_b = (some_address >> 8) as u8;
        test_state.reg_a = 0xfe;

        si_mem.rom = test_rom;

        assert_ne!(si_mem[some_address], 0xfe);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(si_mem[some_address], 0xfe);
        assert_ne!(test_state.prog_counter, 0);
    }

    #[test]
    fn xchg() {
        let mut test_state = ProcessorState::new();
        let mut si_mem = MemMap::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        si_mem.rom = test_rom;

        test_rom[0] = 0b1110_1011;
        si_mem.rom = test_rom;
        test_state.reg_d = 0xbe;
        test_state.reg_e = 0xef;

        assert_eq!(test_state.reg_h, 0x00);
        assert_eq!(test_state.reg_l, 0x00);
        assert_eq!(test_state.reg_d, 0xbe);
        assert_eq!(test_state.reg_e, 0xef);
        iterate_processor_state(&mut test_state, &mut si_mem);
        assert_eq!(test_state.reg_h, 0xbe);
        assert_eq!(test_state.reg_l, 0xef);
        assert_eq!(test_state.reg_d, 0x00);
        assert_eq!(test_state.reg_e, 0x00);
        assert_ne!(test_state.prog_counter, 0);
    }
}
