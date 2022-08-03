//!CPU: Intel 8080 @ 2MHz (CPU similar to the (newer) Zilog Z80)

#![allow(clippy::unusual_byte_groupings)]

use crate::{
    debug_utils,
    machine::{MachineState, PortState},
    space_invaders_rom,
};
use bitflags::bitflags;
use std::{
    convert::From,
    fmt::Debug,
    mem,
    ops::{Index, IndexMut},
    sync::{Arc, Mutex},
};

const CYCLES_BEFORE_ISR: usize = 300_000;

const CYCLES8080: [u8; 256] = [
    4, 10, 7, 5, 5, 5, 7, 4, 4, 10, 7, 5, 5, 5, 7, 4, //0x00..0x0f
    4, 10, 7, 5, 5, 5, 7, 4, 4, 10, 7, 5, 5, 5, 7, 4, //0x10..0x1f
    4, 10, 16, 5, 5, 5, 7, 4, 4, 10, 16, 5, 5, 5, 7, 4, // etc
    4, 10, 13, 5, 10, 10, 10, 4, 4, 10, 13, 5, 5, 5, 7, 4, // 0x30-0x3f
    5, 5, 5, 5, 5, 5, 7, 5, 5, 5, 5, 5, 5, 5, 7, 5, // 0x40..0x4f
    5, 5, 5, 5, 5, 5, 7, 5, 5, 5, 5, 5, 5, 5, 7, 5, //
    5, 5, 5, 5, 5, 5, 7, 5, 5, 5, 5, 5, 5, 5, 7, 5, //
    7, 7, 7, 7, 7, 7, 7, 7, 5, 5, 5, 5, 5, 5, 7, 5, //
    4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, // 0x80..8x4f
    4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, //
    4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, //
    4, 4, 4, 4, 4, 4, 7, 4, 4, 4, 4, 4, 4, 4, 7, 4, //
    11, 10, 10, 10, 17, 11, 7, 11, 11, 10, 10, 10, 10, 17, 7, 11, //0xc0..0xcf
    11, 10, 10, 10, 17, 11, 7, 11, 11, 10, 10, 10, 10, 17, 7, 11, //
    11, 10, 10, 18, 17, 11, 7, 11, 11, 5, 10, 5, 17, 17, 7, 11, //
    11, 10, 10, 4, 17, 11, 7, 11, 11, 5, 10, 4, 17, 17, 7, 11, //
];

bitflags! {
    pub struct ConditionFlags: u8 {
    const CY = 0b0000_0001;
    const P =  0b0000_0100;
    const AC = 0b0001_0000;
    const Z =  0b0100_0000;
    const S =  0b1000_0000;
    }
}

impl ConditionFlags {
    // Clear and reset Z, S, P flags per "standard" rules
    pub fn set_zsp(&mut self, result: u8) {
        self.remove(ConditionFlags::Z | ConditionFlags::S | ConditionFlags::P);
        if result == 0 {
            self.insert(ConditionFlags::Z);
        }
        if (result & 0b1000_0000) >> 7 == 1 {
            self.insert(ConditionFlags::S);
        }
        if result.count_ones() % 2 == 0 {
            self.insert(ConditionFlags::P);
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct ProcessorState {
    pub reg_a: u8,
    pub reg_b: u8,
    pub reg_c: u8,
    pub reg_d: u8,
    pub reg_e: u8,
    pub reg_h: u8,
    pub reg_l: u8,
    pub stack_pointer: u16,
    pub prog_counter: u16,
    pub flags: ConditionFlags,
    pub interrupts_enabled: bool,
    pub last_int: ProcessorInterrupt,
    pub trace_me: bool,
    pub instr_count: usize,
    pub cycle_count_since_last_int: u128,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum ProcessorInterrupt {
    ScanLine96 = 1,
    ScanLine224 = 2,
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
            stack_pointer: 0,
            prog_counter: 0,
            flags: ConditionFlags {
                bits: (0b0000_0000),
            },
            interrupts_enabled: false,
            last_int: ProcessorInterrupt::ScanLine96,
            trace_me: false,
            instr_count: 0,
            cycle_count_since_last_int: 0,
        }
    }

    // Gets value from a register, given its bit pattern.
    pub fn get_reg_value(&self, pattern: RegisterBitPattern) -> Option<u8> {
        match pattern {
            RegisterBitPattern::A => Some(self.reg_a),
            RegisterBitPattern::B => Some(self.reg_b),
            RegisterBitPattern::C => Some(self.reg_c),
            RegisterBitPattern::D => Some(self.reg_d),
            RegisterBitPattern::E => Some(self.reg_e),
            RegisterBitPattern::H => Some(self.reg_h),
            RegisterBitPattern::L => Some(self.reg_l),
            RegisterBitPattern::Other => None,
        }
    }

    // Sets register value
    pub fn set_reg_value(&mut self, pattern: RegisterBitPattern, value: u8) -> Option<()> {
        match pattern {
            RegisterBitPattern::A => {
                self.reg_a = value;
                Some(())
            }
            RegisterBitPattern::B => {
                self.reg_b = value;
                Some(())
            }
            RegisterBitPattern::C => {
                self.reg_c = value;
                Some(())
            }
            RegisterBitPattern::D => {
                self.reg_d = value;
                Some(())
            }
            RegisterBitPattern::E => {
                self.reg_e = value;
                Some(())
            }
            RegisterBitPattern::H => {
                self.reg_h = value;
                Some(())
            }
            RegisterBitPattern::L => {
                self.reg_l = value;
                Some(())
            }
            RegisterBitPattern::Other => None,
        }
    }

    // Gets value at the address indicated by the register pair bit pattern
    pub fn get_mem_value(&mut self, reg_pair: RPairBitPattern, mem_map: &MemMap) -> u8 {
        let addr = self.get_rp(reg_pair);
        mem_map[addr]
    }

    // Sets value at the address indicated by the register pair bit pattern
    pub fn set_mem_value(&mut self, reg_pair: RPairBitPattern, mem_map: &mut MemMap, value: u8) {
        let addr = self.get_rp(reg_pair);
        mem_map[addr] = value;
    }

    /// This uses the register pair bits from the opcode to
    /// load a u16 into a register pair
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

    /// gets the contents of the rp as a u16
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
        if self.stack_pointer == 0 {
            panic!("Trying to push with uninitialized stack pointer")
        }
        mem_map[self.stack_pointer - 1] = ((address) >> 8) as u8;
        mem_map[self.stack_pointer - 2] = address as u8;
        self.stack_pointer -= 2;
    }

    /// Push one byte to the stack
    pub fn push_byte(&mut self, mem_map: &mut MemMap, this_data: u8) {
        if self.stack_pointer == 0 {
            panic!("Trying to push with uninitialized stack pointer")
        }
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

impl Default for ProcessorState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct MemMap {
    pub rom: [u8; space_invaders_rom::SPACE_INVADERS_ROM.len()],
    pub rw_mem: Vec<u8>,
}

impl Default for MemMap {
    fn default() -> Self {
        Self::new()
    }
}

impl MemMap {
    // Todo: Replace space invaders specific memory layout with
    // configurable map.
    pub fn new() -> Self {
        MemMap {
            rom: space_invaders_rom::SPACE_INVADERS_ROM,
            rw_mem: vec![0; 0x4000],
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
        } else if ((idx as usize) - space_invaders_rom::SPACE_INVADERS_ROM.len()) < 0x4000 {
            &mut self.rw_mem[(idx - space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) as usize]
        } else {
            panic!(
                "Index out of bounds for mem map of {} at ({} - rom length) is {}",
                8192,
                idx,
                (idx as usize) - space_invaders_rom::SPACE_INVADERS_ROM.len()
            );
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

impl MachineState {
    pub fn do_next_interrupt(&mut self) {
        if self.processor_state.interrupts_enabled {
            if self.last_interrupt == ProcessorInterrupt::ScanLine224 {
                self.generate_interrupt(ProcessorInterrupt::ScanLine96)
            } else if self.last_interrupt == ProcessorInterrupt::ScanLine96 {
                self.generate_interrupt(ProcessorInterrupt::ScanLine224)
            }
        }
    }

    pub fn interrupt_due(&self) -> bool {
        self.processor_state.cycle_count_since_last_int > CYCLES_BEFORE_ISR.try_into().unwrap()
    }

    pub fn generate_interrupt(&mut self, interrupt: ProcessorInterrupt) {
        self.processor_state.interrupts_enabled = false;
        self.last_interrupt = interrupt;
        self.processor_state.cycle_count_since_last_int = 0;
        opcode_rst(
            &mut self.processor_state,
            &mut self.mem_map,
            self.last_interrupt,
        );
    }
    /// Match statement for operation decoding
    pub fn iterate_processor_state(&mut self) {
        self.processor_state.instr_count += 1;
        let cur_instruction = self.mem_map[self.processor_state.prog_counter];
        if self.processor_state.trace_me {
            debug_utils::opcode_printer(self);
        }

        match cur_instruction {
            0x00 => {
                opcode_nop(&mut self.processor_state);
            }
            0x01 => {
                opcode_lxi(&mut self.processor_state, &mut self.mem_map);
            }
            0x02 => {
                opcode_stax(&mut self.processor_state, &mut self.mem_map);
            }
            0x03 => {
                opcode_inx(&mut self.processor_state, &mut self.mem_map);
            }
            0x04 => {
                opcode_inr(&mut self.processor_state, &mut self.mem_map);
            }
            0x05 => {
                opcode_dcr(&mut self.processor_state, &mut self.mem_map);
            }
            0x06 => {
                opcode_mvi(&mut self.processor_state, &mut self.mem_map);
            }
            0x07 => {
                opcode_rlc(&mut self.processor_state);
            }
            0x08 => {
                panic!("Bogus opcode, bailing");
            }
            0x09 => {
                opcode_dad(&mut self.processor_state, &mut self.mem_map);
            }
            0x0a => {
                opcode_ldax(&mut self.processor_state, &mut self.mem_map);
            }
            0x0b => {
                opcode_dcx(&mut self.processor_state, &mut self.mem_map);
            }
            0x0c => {
                opcode_inr(&mut self.processor_state, &mut self.mem_map);
            }
            0x0d => {
                opcode_dcr(&mut self.processor_state, &mut self.mem_map);
            }
            0x0e => {
                opcode_mvi(&mut self.processor_state, &mut self.mem_map);
            }
            0x0f => {
                opcode_rrc(&mut self.processor_state);
            }
            0x10 => {
                panic!("Bogus opcode, bailing");
            }
            0x11 => {
                opcode_lxi(&mut self.processor_state, &mut self.mem_map);
            }
            0x12 => {
                opcode_stax(&mut self.processor_state, &mut self.mem_map);
            }
            0x13 => {
                opcode_inx(&mut self.processor_state, &mut self.mem_map);
            }
            0x14 => {
                opcode_inr(&mut self.processor_state, &mut self.mem_map);
            }
            0x15 => {
                opcode_dcr(&mut self.processor_state, &mut self.mem_map);
            }
            0x16 => {
                opcode_mvi(&mut self.processor_state, &mut self.mem_map);
            }
            0x17 => {
                opcode_ral(&mut self.processor_state);
            }
            0x18 => {
                panic!("Bogus opcode, bailing");
            }
            0x19 => {
                opcode_dad(&mut self.processor_state, &mut self.mem_map);
            }
            0x1a => {
                opcode_ldax(&mut self.processor_state, &mut self.mem_map);
            }
            0x1b => {
                opcode_dcx(&mut self.processor_state, &mut self.mem_map);
            }
            0x1c => {
                opcode_inr(&mut self.processor_state, &mut self.mem_map);
            }
            0x1d => {
                opcode_dcr(&mut self.processor_state, &mut self.mem_map);
            }
            0x1e => {
                opcode_mvi(&mut self.processor_state, &mut self.mem_map);
            }
            0x1f => {
                opcode_rar(&mut self.processor_state);
            }
            0x20 => {
                panic!("Bogus opcode, bailing: {}", cur_instruction);
            }
            0x21 => {
                opcode_lxi(&mut self.processor_state, &mut self.mem_map);
            }
            0x22 => {
                opcode_shld(&mut self.processor_state, &mut self.mem_map);
            }
            0x23 => {
                opcode_inx(&mut self.processor_state, &mut self.mem_map);
            }
            0x24 => {
                opcode_inr(&mut self.processor_state, &mut self.mem_map);
            }
            0x25 => {
                opcode_dcr(&mut self.processor_state, &mut self.mem_map);
            }
            0x26 => {
                opcode_mvi(&mut self.processor_state, &mut self.mem_map);
            }
            0x27 => {
                opcode_daa(&mut self.processor_state);
            }
            0x28 => {
                panic!("Bogus opcode, bailing");
            }
            0x29 => {
                opcode_dad(&mut self.processor_state, &mut self.mem_map);
            }
            0x2a => {
                opcode_lhld(&mut self.processor_state, &mut self.mem_map);
            }
            0x2b => {
                opcode_dcx(&mut self.processor_state, &mut self.mem_map);
            }
            0x2c => {
                opcode_inr(&mut self.processor_state, &mut self.mem_map);
            }
            0x2d => {
                opcode_dcr(&mut self.processor_state, &mut self.mem_map);
            }
            0x2e => {
                opcode_mvi(&mut self.processor_state, &mut self.mem_map);
            }
            0x2f => {
                opcode_cma(&mut self.processor_state);
            }
            0x30 => {
                panic!("Bogus opcode, bailing");
            }
            0x31 => {
                opcode_lxi(&mut self.processor_state, &mut self.mem_map);
            }
            0x32 => {
                opcode_sta(&mut self.processor_state, &mut self.mem_map);
            }
            0x33 => {
                opcode_inx(&mut self.processor_state, &mut self.mem_map);
            }
            0x34 => {
                opcode_inr(&mut self.processor_state, &mut self.mem_map);
            }
            0x35 => {
                opcode_dcr(&mut self.processor_state, &mut self.mem_map);
            }
            0x36 => {
                opcode_mvi(&mut self.processor_state, &mut self.mem_map);
            }
            0x37 => {
                opcode_stc(&mut self.processor_state);
            }
            0x38 => {
                panic!("Bogus opcode, bailing");
            }
            0x39 => {
                opcode_dad(&mut self.processor_state, &mut self.mem_map);
            }
            0x3a => {
                opcode_lda(&mut self.processor_state, &mut self.mem_map);
            }
            0x3b => {
                opcode_dcx(&mut self.processor_state, &mut self.mem_map);
            }
            0x3c => {
                opcode_inr(&mut self.processor_state, &mut self.mem_map);
            }
            0x3d => {
                opcode_dcr(&mut self.processor_state, &mut self.mem_map);
            }
            0x3e => {
                opcode_mvi(&mut self.processor_state, &mut self.mem_map);
            }
            0x3f => {
                opcode_cmc(&mut self.processor_state);
            }
            0x40 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x41 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x42 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x43 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x44 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x45 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x46 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x47 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x48 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x49 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x4a => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x4b => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x4c => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x4d => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x4e => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x4f => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x50 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x51 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x52 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x53 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x54 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x55 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x56 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x57 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x58 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x59 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x5a => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x5b => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x5c => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x5d => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x5e => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x5f => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x60 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x61 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x62 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x63 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x64 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x65 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x66 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x67 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x68 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x69 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x6a => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x6b => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x6c => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x6d => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x6e => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x6f => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x70 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x71 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x72 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x73 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x74 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x75 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x76 => {
                opcode_halt(&mut self.processor_state, &mut self.mem_map);
            }
            0x77 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x78 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x79 => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x7a => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x7b => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x7c => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x7d => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x7e => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x7f => {
                opcode_mov(&mut self.processor_state, &mut self.mem_map);
            }
            0x80 => {
                opcode_add(&mut self.processor_state, &mut self.mem_map);
            }
            0x81 => {
                opcode_add(&mut self.processor_state, &mut self.mem_map);
            }
            0x82 => {
                opcode_add(&mut self.processor_state, &mut self.mem_map);
            }
            0x83 => {
                opcode_add(&mut self.processor_state, &mut self.mem_map);
            }
            0x84 => {
                opcode_add(&mut self.processor_state, &mut self.mem_map);
            }
            0x85 => {
                opcode_add(&mut self.processor_state, &mut self.mem_map);
            }
            0x86 => {
                opcode_add(&mut self.processor_state, &mut self.mem_map);
            }
            0x87 => {
                opcode_add(&mut self.processor_state, &mut self.mem_map);
            }
            0x88 => {
                opcode_adc(&mut self.processor_state, &mut self.mem_map);
            }
            0x89 => {
                opcode_adc(&mut self.processor_state, &mut self.mem_map);
            }
            0x8a => {
                opcode_adc(&mut self.processor_state, &mut self.mem_map);
            }
            0x8b => {
                opcode_adc(&mut self.processor_state, &mut self.mem_map);
            }
            0x8c => {
                opcode_adc(&mut self.processor_state, &mut self.mem_map);
            }
            0x8d => {
                opcode_adc(&mut self.processor_state, &mut self.mem_map);
            }
            0x8e => {
                opcode_adc(&mut self.processor_state, &mut self.mem_map);
            }
            0x8f => {
                opcode_adc(&mut self.processor_state, &mut self.mem_map);
            }
            0x90 => {
                opcode_sub(&mut self.processor_state, &mut self.mem_map);
            }
            0x91 => {
                opcode_sub(&mut self.processor_state, &mut self.mem_map);
            }
            0x92 => {
                opcode_sub(&mut self.processor_state, &mut self.mem_map);
            }
            0x93 => {
                opcode_sub(&mut self.processor_state, &mut self.mem_map);
            }
            0x94 => {
                opcode_sub(&mut self.processor_state, &mut self.mem_map);
            }
            0x95 => {
                opcode_sub(&mut self.processor_state, &mut self.mem_map);
            }
            0x96 => {
                opcode_sub(&mut self.processor_state, &mut self.mem_map);
            }
            0x97 => {
                opcode_sub(&mut self.processor_state, &mut self.mem_map);
            }
            0x98 => {
                opcode_sbb(&mut self.processor_state, &mut self.mem_map);
            }
            0x99 => {
                opcode_sbb(&mut self.processor_state, &mut self.mem_map);
            }
            0x9a => {
                opcode_sbb(&mut self.processor_state, &mut self.mem_map);
            }
            0x9b => {
                opcode_sbb(&mut self.processor_state, &mut self.mem_map);
            }
            0x9c => {
                opcode_sbb(&mut self.processor_state, &mut self.mem_map);
            }
            0x9d => {
                opcode_sbb(&mut self.processor_state, &mut self.mem_map);
            }
            0x9e => {
                opcode_sbb(&mut self.processor_state, &mut self.mem_map);
            }
            0x9f => {
                opcode_sbb(&mut self.processor_state, &mut self.mem_map);
            }
            0xa0 => {
                opcode_ana(&mut self.processor_state, &mut self.mem_map);
            }
            0xa1 => {
                opcode_ana(&mut self.processor_state, &mut self.mem_map);
            }
            0xa2 => {
                opcode_ana(&mut self.processor_state, &mut self.mem_map);
            }
            0xa3 => {
                opcode_ana(&mut self.processor_state, &mut self.mem_map);
            }
            0xa4 => {
                opcode_ana(&mut self.processor_state, &mut self.mem_map);
            }
            0xa5 => {
                opcode_ana(&mut self.processor_state, &mut self.mem_map);
            }
            0xa6 => {
                opcode_ana(&mut self.processor_state, &mut self.mem_map);
            }
            0xa7 => {
                opcode_ana(&mut self.processor_state, &mut self.mem_map);
            }
            0xa8 => {
                opcode_xra(&mut self.processor_state, &mut self.mem_map);
            }
            0xa9 => {
                opcode_xra(&mut self.processor_state, &mut self.mem_map);
            }
            0xaa => {
                opcode_xra(&mut self.processor_state, &mut self.mem_map);
            }
            0xab => {
                opcode_xra(&mut self.processor_state, &mut self.mem_map);
            }
            0xac => {
                opcode_xra(&mut self.processor_state, &mut self.mem_map);
            }
            0xad => {
                opcode_xra(&mut self.processor_state, &mut self.mem_map);
            }
            0xae => {
                opcode_xra(&mut self.processor_state, &mut self.mem_map);
            }
            0xaf => {
                opcode_xra(&mut self.processor_state, &mut self.mem_map);
            }
            0xb0 => {
                opcode_ora(&mut self.processor_state, &mut self.mem_map);
            }
            0xb1 => {
                opcode_ora(&mut self.processor_state, &mut self.mem_map);
            }
            0xb2 => {
                opcode_ora(&mut self.processor_state, &mut self.mem_map);
            }
            0xb3 => {
                opcode_ora(&mut self.processor_state, &mut self.mem_map);
            }
            0xb4 => {
                opcode_ora(&mut self.processor_state, &mut self.mem_map);
            }
            0xb5 => {
                opcode_ora(&mut self.processor_state, &mut self.mem_map);
            }
            0xb6 => {
                opcode_ora(&mut self.processor_state, &mut self.mem_map);
            }
            0xb7 => {
                opcode_ora(&mut self.processor_state, &mut self.mem_map);
            }
            0xb8 => {
                opcode_cmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xb9 => {
                opcode_cmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xba => {
                opcode_cmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xbb => {
                opcode_cmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xbc => {
                opcode_cmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xbd => {
                opcode_cmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xbe => {
                opcode_cmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xbf => {
                opcode_cmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xc0 => {
                opcode_ret(&mut self.processor_state, &mut self.mem_map);
            }
            0xc1 => {
                opcode_pop(&mut self.processor_state, &mut self.mem_map);
            }
            0xc2 => {
                opcode_jmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xc3 => {
                opcode_jmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xc4 => {
                opcode_call(&mut self.processor_state, &mut self.mem_map);
            }
            0xc5 => {
                opcode_push(&mut self.processor_state, &mut self.mem_map);
            }
            0xc6 => {
                opcode_adi(&mut self.processor_state, &mut self.mem_map);
            }
            0xc7 => {
                panic!("interrupt vectors emulated at start of iterate_state");
            }
            0xc8 => {
                opcode_ret(&mut self.processor_state, &mut self.mem_map);
            }
            0xc9 => {
                opcode_ret(&mut self.processor_state, &mut self.mem_map);
            }
            0xca => {
                opcode_jmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xcb => {
                panic!("Bogus opcode, bailing");
            }
            0xcc => {
                opcode_call(&mut self.processor_state, &mut self.mem_map);
            }
            0xcd => {
                opcode_call(&mut self.processor_state, &mut self.mem_map);
            }
            0xce => {
                opcode_aci(&mut self.processor_state, &mut self.mem_map);
            }
            0xcf => {
                panic!("interrupt vectors emulated at start of iterate_state");
            }
            0xd0 => {
                opcode_ret(&mut self.processor_state, &mut self.mem_map);
            }
            0xd1 => {
                opcode_pop(&mut self.processor_state, &mut self.mem_map);
            }
            0xd2 => {
                opcode_jmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xd3 => {
                opcode_out(
                    &mut self.processor_state,
                    &mut self.mem_map,
                    self.port_state.clone(),
                );
            }
            0xd4 => {
                opcode_call(&mut self.processor_state, &mut self.mem_map);
            }
            0xd5 => {
                opcode_push(&mut self.processor_state, &mut self.mem_map);
            }
            0xd6 => {
                opcode_sui(&mut self.processor_state, &mut self.mem_map);
            }
            0xd7 => {
                panic!("interrupt vectors emulated at start of iterate_state");
            }
            0xd8 => {
                opcode_ret(&mut self.processor_state, &mut self.mem_map);
            }
            0xd9 => {
                panic!("Bogus opcode, bailing");
            }
            0xda => {
                opcode_jmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xdb => {
                opcode_in(
                    &mut self.processor_state,
                    &mut self.mem_map,
                    self.port_state.clone(),
                );
            }
            0xdc => {
                opcode_call(&mut self.processor_state, &mut self.mem_map);
            }
            0xdd => {
                panic!("Bogus opcode, bailing");
            }
            0xde => {
                opcode_sbi(&mut self.processor_state, &mut self.mem_map);
            }
            0xdf => {
                panic!("interrupt vectors emulated at start of iterate_state");
            }
            0xe0 => {
                opcode_ret(&mut self.processor_state, &mut self.mem_map);
            }
            0xe1 => {
                opcode_pop(&mut self.processor_state, &mut self.mem_map);
            }
            0xe2 => {
                opcode_jmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xe3 => {
                opcode_xthl(&mut self.processor_state, &mut self.mem_map);
            }
            0xe4 => {
                opcode_call(&mut self.processor_state, &mut self.mem_map);
            }
            0xe5 => {
                opcode_push(&mut self.processor_state, &mut self.mem_map);
            }
            0xe6 => {
                opcode_ani(&mut self.processor_state, &mut self.mem_map);
            }
            0xe7 => {
                panic!("interrupt vectors emulated at start of iterate_state");
            }
            0xe8 => {
                opcode_ret(&mut self.processor_state, &mut self.mem_map);
            }
            0xe9 => {
                opcode_pchl(&mut self.processor_state, &mut self.mem_map);
            }
            0xea => {
                opcode_jmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xeb => {
                opcode_xchg(&mut self.processor_state, &mut self.mem_map);
            }
            0xec => {
                opcode_call(&mut self.processor_state, &mut self.mem_map);
            }
            0xed => {
                panic!("Bogus opcode, bailing");
            }
            0xee => {
                opcode_xri(&mut self.processor_state, &mut self.mem_map);
            }
            0xef => {
                panic!("interrupt vectors emulated at start of iterate_state");
            }
            0xf0 => {
                opcode_ret(&mut self.processor_state, &mut self.mem_map);
            }
            0xf1 => {
                opcode_pop(&mut self.processor_state, &mut self.mem_map);
            }
            0xf2 => {
                opcode_jmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xf3 => {
                opcode_di(&mut self.processor_state, &mut self.mem_map);
            }
            0xf4 => {
                opcode_call(&mut self.processor_state, &mut self.mem_map);
            }
            0xf5 => {
                opcode_push(&mut self.processor_state, &mut self.mem_map);
            }
            0xf6 => {
                opcode_ori(&mut self.processor_state, &mut self.mem_map);
            }
            0xf7 => {
                panic!("interrupt vectors emulated at start of iterate_state");
            }
            0xf8 => {
                opcode_ret(&mut self.processor_state, &mut self.mem_map);
            }
            0xf9 => {
                opcode_sphl(&mut self.processor_state, &mut self.mem_map);
            }
            0xfa => {
                opcode_jmp(&mut self.processor_state, &mut self.mem_map);
            }
            0xfb => {
                opcode_ei(&mut self.processor_state, &mut self.mem_map);
            }
            0xfc => {
                opcode_call(&mut self.processor_state, &mut self.mem_map);
            }
            0xfd => {
                panic!("Bogus opcode, bailing");
            }
            0xfe => {
                opcode_cpi(&mut self.processor_state, &mut self.mem_map);
            }
            0xff => {
                panic!("interrupt vectors emulated at start of iterate_state");
            }
        };
        if self.processor_state.trace_me {
            debug_utils::processor_state_printer(self);
        }
        self.processor_state.cycle_count_since_last_int +=
            CYCLES8080[cur_instruction as usize] as u128;
    }
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
        state.push_address(mem_map, state.prog_counter + 3);
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
    if src == RegisterBitPattern::Other {
        match dest {
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
    } else if dest == RegisterBitPattern::Other {
        // then this is a 0 | 1 | D | D | D | 1 | 1 | 0
        // format opcode, use dest
        match src {
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

/// ADD r (Add Register)
/// (A) ~ (A) + (r)
/// The content of register r is added to the content of
/// the accumulator. The result is placed in the accumulator.
/// ADD M (Add memory)
/// (A) ~ (A) + ((H) (L))
/// The content of the memory location whose address is
/// contained in the H and L registers is added to the
/// content of the accumulator. The result is placed in the
/// accumulator.
fn opcode_add(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 0 | 0 | 0 | 0 | S | S | S
    // or
    // 1 | 0 | 0 | 0 | 0 | 1 | 1 | 0

    let cur_instruction = mem_map[state.prog_counter];
    let src = get_source_register_bit_pattern(cur_instruction);
    state.prog_counter += 1;

    let addend = match state.get_reg_value(src) {
        Some(value) => value,
        None => {
            // Then this is a 1 | 0 | 0 | 0 | 0 | 1 | 1 | 0
            // format opcode subtracting value located at the
            // memory address in pair H-L from accumulator.
            state.get_mem_value(RPairBitPattern::HL, mem_map)
        }
    };
    // Add first four bits to detect carry to fifth.
    let low_add = (state.reg_a & 0x0f) + (addend & 0x0f);
    let aux_carry = low_add & 0b0001_0000 != 0;

    // Perform addition, move sum into accumulator.
    let (sum, overflow) = state.reg_a.overflowing_add(addend);
    state.reg_a = sum;

    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
    state.flags.set(ConditionFlags::CY, overflow);
    state.flags.set(ConditionFlags::AC, aux_carry);
}

/// ADI data (Add Immediate)
/// (A) ~ (A) + (byte 2)
/// The content of the second byte of the instruction is
/// added to the content of the accumulator. The result
/// is placed in the accumulator.
fn opcode_adi(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 1 | 0 | 0 | 0 | 1 | 1 | 0
    let second_byte = mem_map[state.prog_counter + 1];
    state.prog_counter += 2;

    // Add first four bits to detect carry to fifth.
    let low_add = (state.reg_a & 0x0f) + (second_byte & 0x0f);
    let aux_carry = low_add & 0x10 != 0;

    // Perform addition, move sum into accumulator.
    let (sum, overflow) = state.reg_a.overflowing_add(second_byte);
    state.reg_a = sum;

    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
    state.flags.set(ConditionFlags::CY, overflow);
    state.flags.set(ConditionFlags::AC, aux_carry);
}

/// ADC r (Add Register with carry)
/// (A) ~ (A) + (r) + (CY)
/// The content of register r and the content of the carry
/// bit are added to the content of the accumulator. The
/// result is placed in the accumulator.
/// ADC M (Add memory with carry)
/// (A) ~ (A) + ((H) (L)) + (CY)
/// The content of the memory location whose address is
/// contained in the Hand L registers and the content of
/// the CY flag are added to the accumulator. The result
/// is placed in the accumulator.
fn opcode_adc(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let src = get_source_register_bit_pattern(cur_instruction);
    state.prog_counter += 1;

    let addend = match state.get_reg_value(src) {
        Some(value) => value,
        None => {
            // Then this is a 1 | 0 | 0 | 0 | 0 | 1 | 1 | 0
            // format opcode subtracting value located at the
            // memory address in pair H-L from accumulator.
            state.get_mem_value(RPairBitPattern::HL, mem_map)
        }
    };
    let cf_state: u8 = state.check_condition(ConditionBitPattern::C).into();

    // Add first four bits (+1 for carry flag, if set) to detect carry to fifth.
    let low_add = (state.reg_a & 0x0f) + (addend & 0x0f) + cf_state;
    let aux_carry = low_add & 0x10 != 0;

    // Perform addition, move sum into accumulator.
    let (sum, overflow_first_add) = state.reg_a.overflowing_add(addend);
    let (sum, overflow_second_add) = sum.overflowing_add(cf_state);
    let overflow = overflow_first_add || overflow_second_add;
    state.reg_a = sum;

    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
    state.flags.set(ConditionFlags::CY, overflow);
    state.flags.set(ConditionFlags::AC, aux_carry);
}

/// ACI data (Add immediate with carry)
/// (A) ~ (A) + (byte 2) + (CY)
/// The content of the second byte of the instruction and
/// the content of the CY flag are added to the contents
/// of the accumulator. The result is placed in the
/// accumulator.
fn opcode_aci(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 1 | 0 | 0 | 1 | 1 | 1 | 0
    let second_byte = mem_map[state.prog_counter + 1];
    state.prog_counter += 2;

    let cf_state: u8 = state.check_condition(ConditionBitPattern::C).into();

    // Add first four bits (+1 for carry flag, if set) to detect carry to fifth.
    let low_add = (state.reg_a & 0x0f) + (second_byte & 0x0f) + cf_state;
    let aux_carry = low_add & 0x10 != 0;

    // Perform addition, move sum into accumulator.
    let (sum, overflow_first_add) = state.reg_a.overflowing_add(second_byte);
    let (sum, overflow_second_add) = sum.overflowing_add(cf_state);
    let overflow = overflow_first_add || overflow_second_add;
    state.reg_a = sum;

    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
    state.flags.set(ConditionFlags::CY, overflow);
    state.flags.set(ConditionFlags::AC, aux_carry);
}

/// SUB r (Subtract register)
/// (A) ~ (A) - (r)
/// The content of register r is subtracted from the
/// content of the accumulator. The result is placed in
/// the accumulator.
/// SUB M (Subtract memory)
/// (A) ~ (A) - ((H) (L))
/// The content of the memory location whose address is
/// contained in the Hand L registers is subtracted from
/// the content of the accumulator. The result is placed
/// in the accumulator
fn opcode_sub(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 0 | 0 | 1 | 0 | S | S | S
    // or
    // 1 | 0 | 0 | 1 | 0 | 1 | 1 | 0

    let cur_instruction = mem_map[state.prog_counter];
    let src = get_source_register_bit_pattern(cur_instruction);
    state.prog_counter += 1;

    let subtrahend = match state.get_reg_value(src) {
        Some(value) => value,
        None => {
            // Then this is a 1 | 0 | 0 | 0 | 0 | 1 | 1 | 0
            // format opcode subtracting value located at the
            // memory address in pair H-L from accumulator.
            state.get_mem_value(RPairBitPattern::HL, mem_map)
        }
    };
    let twos_complement = subtrahend.wrapping_neg();

    // Add first four bits to detect carry to fifth.
    let low_add = (state.reg_a & 0x0f) + (twos_complement & 0x0f);
    let aux_carry = low_add & 0x10 != 0;

    // Perform addition, move sum into accumulator.
    let (sum, overflow) = state.reg_a.overflowing_add(twos_complement);
    state.reg_a = sum;

    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
    state.flags.set(ConditionFlags::CY, !overflow);
    state.flags.set(ConditionFlags::AC, aux_carry);
}

/// SUI data (Subtract immediate)
/// (A) ~ (A) - (byte 2)
/// The content of the second byte of the instruction is
/// subtracted from the content of the accumulator. The
/// result is placed in the accumulator.
fn opcode_sui(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 1 | 0 | 1 | 0 | 1 | 1 | 0
    let second_byte = mem_map[state.prog_counter + 1];
    state.prog_counter += 2;
    let twos_complement = second_byte.wrapping_neg();
    let low_add = (state.reg_a & 0x0f) + (twos_complement & 0x0f);
    let aux_carry = low_add & 0x10 != 0;
    let (sum, overflow) = state.reg_a.overflowing_add(twos_complement);
    state.reg_a = sum;

    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
    state.flags.set(ConditionFlags::CY, !overflow);
    state.flags.set(ConditionFlags::AC, aux_carry);
}

/// SBB r (Subtract Register with borrow)
/// (A) ~ (A) - (r) - (CY)
/// The content of register r and the content of the CY
/// flag are both subtracted from the accumulator. The
/// result is placed in the accumulator.
/// SBB M (Subtract memory with borrow)
/// (A) ~ (A) - ((H) (L)) - (CY)
/// The content of the memory location whose address is
/// contained in the Hand L registers and the content of
/// the CY flag are both subtracted from the accumulator.
/// The result is placed in the accumulator.
fn opcode_sbb(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 0 | 0 | 1 | 1 | S | S | S
    // or
    // 1 | 0 | 0 | 1 | 1 | 1 | 1 | 0
    let cur_instruction = mem_map[state.prog_counter];
    let src = get_source_register_bit_pattern(cur_instruction);
    state.prog_counter += 1;

    let subtrahend = match state.get_reg_value(src) {
        Some(value) => value,
        None => {
            // Then this is a 1 | 0 | 0 | 0 | 0 | 1 | 1 | 0
            // format opcode subtracting value located at the
            // memory address in pair H-L from accumulator.
            state.get_mem_value(RPairBitPattern::HL, mem_map)
        }
    };
    // Perform subtraction using two's complement
    let cf_state = state.check_condition(ConditionBitPattern::C);
    let twos_complement = (subtrahend + cf_state as u8).wrapping_neg();
    let low_add = (state.reg_a & 0x0f) + (twos_complement & 0x0f);
    let (sum, overflow) = state.reg_a.overflowing_add(twos_complement);
    let aux_carry = low_add & 0x10 != 0;
    state.reg_a = sum;

    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
    state.flags.set(ConditionFlags::CY, !overflow);
    state.flags.set(ConditionFlags::AC, aux_carry);
}

/// SBI data (Subtract immediate with borrow)
/// (A) ~ (A) - (byte 2) - (CY)
/// The contents of the second byte of the instruction
/// and the contents of the CY flag are both subtracted
/// from the accumulator. The result is placed in the
/// accumulator.
fn opcode_sbi(state: &mut ProcessorState, mem_map: &mut MemMap) {
    //1 | 1 | 0 | 1 | 1 | 1 | 1 | 0
    let second_byte = mem_map[state.prog_counter + 1];
    state.prog_counter += 2;

    // Perform subtraction using two's complement
    let cf_state = state.check_condition(ConditionBitPattern::C);
    let twos_complement = (second_byte + cf_state as u8).wrapping_neg();
    let low_add = (state.reg_a & 0x0f) + (twos_complement & 0x0f);
    let (sum, overflow) = state.reg_a.overflowing_add(twos_complement);
    let aux_carry = low_add & 0x10 != 0;
    state.reg_a = sum;

    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
    state.flags.set(ConditionFlags::CY, !overflow);
    state.flags.set(ConditionFlags::AC, aux_carry);
}

/// INR r (Increment Register)
/// (r) ~ (r) + 1
/// The content of register r is incremented by one.
/// Note: All condition flags except CY are affected.
/// INR M (Increment Memory)
/// ((H) (L)) ~ ((H) (L)) + 1
/// The content of the memory location whose address
/// is contained in the Hand L registers is incremented
/// by one. Note: All condition flags except CY are
/// affected.
fn opcode_inr(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 0 | 0 | D | D | D | 1 | 0 | 0
    // or
    // 0 | 0 | 1 | 1 | 0 | 1 | 0 | 0
    let cur_instruction = mem_map[state.prog_counter];
    let src = ((cur_instruction & 0b00_111_000) >> 3).into();
    state.prog_counter += 1;

    let result: u8;
    let low_add: u8;
    let current = state.get_reg_value(src);
    match current {
        Some(current) => {
            low_add = (current & 0x0f) + 0x01;
            result = current.wrapping_add(0x01);
            state.set_reg_value(src, result);
        }
        None => {
            low_add = (state.get_mem_value(RPairBitPattern::HL, mem_map) & 0x0f) + 0x01;
            result = state
                .get_mem_value(RPairBitPattern::HL, mem_map)
                .wrapping_add(0x01);
            state.set_mem_value(RPairBitPattern::HL, mem_map, result);
        }
    }
    let aux_carry = low_add & 0x10 != 0;

    // Update all flags except CY
    state.flags.set_zsp(result);
    state.flags &= !ConditionFlags::AC;
    if aux_carry {
        state.flags |= ConditionFlags::AC;
    }
}

/// DCR r (Decrement Register)
/// (r) ~ (r) - 1
/// The content of register r is decremented by one.
/// Note: All condition flags except CY are affected.
/// INR M (Decrement Memory)
/// ((H) (L)) ~ ((H) (L)) - 1
/// The content of the memory location whose address
/// is contained in the Hand L registers is decremented
/// by one. Note: All condition flags except CY are
/// affected.
fn opcode_dcr(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 0 | 0 | D | D | D | 1 | 0 | 1
    // or
    // 0 | 0 | 1 | 1 | 0 | 1 | 0 | 1
    let cur_instruction = mem_map[state.prog_counter];
    let src = ((cur_instruction & 0b00_111_000) >> 3).into();
    state.prog_counter += 1;

    let result: u8;
    let low_add: u8;
    let current = state.get_reg_value(src);
    match current {
        Some(current) => {
            low_add = (current & 0x0f) + 0x0f;
            result = current.wrapping_sub(0x01);
            state.set_reg_value(src, result);
        }
        None => {
            low_add = (state.get_mem_value(RPairBitPattern::HL, mem_map) & 0x0f) + 0x0f;
            result = state
                .get_mem_value(RPairBitPattern::HL, mem_map)
                .wrapping_sub(0x01);
            state.set_mem_value(RPairBitPattern::HL, mem_map, result);
        }
    }
    let aux_carry = low_add & 0x10 != 0;
    state.flags.set(ConditionFlags::AC, aux_carry);
    state.flags.set_zsp(result);
}

/// DCX rp (Decrement register pair)
/// (rh) (rl) ~ (rh) (rl) - 1
/// The content of the register pair rp is decremented
/// by one. Note: No condition flags are affected.
fn opcode_dcx(state: &mut ProcessorState, mem_map: &mut MemMap) {
    let cur_instruction = mem_map[state.prog_counter];
    let rp_bits = get_register_pair_bit_pattern(cur_instruction);
    let rp_16 = state.get_rp(rp_bits);
    state.set_rp(rp_16.wrapping_sub(0x01), rp_bits);
    state.prog_counter += 1;
}

/// DAD rp (Add register pair to H and L)
/// (H) (L) ~ (H) (L) + (rh) (rl)
/// The content of the register pair rp is added to the
/// content of the register pair H and L. The result is
/// placed in the register pair H and L. Note: Only the
/// CY flag is affected. It is set if there is a carry out of
/// the double precision add; otherwise it is reset.
fn opcode_dad(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 0 | 0 | R | P | 1 | 0 | 0 | 1
    let cur_instruction = mem_map[state.prog_counter];
    let src = get_register_pair_bit_pattern(cur_instruction);
    state.prog_counter += 1;
    let rp_value = state.get_rp(src);
    let hl_value = state.get_rp(RPairBitPattern::HL);
    let (sum, overflow) = hl_value.overflowing_add(rp_value);
    state.reg_h = ((0xff00 & sum) >> 8) as u8;
    state.reg_l = (0x00ff & sum) as u8;
    state.flags.set(ConditionFlags::CY, overflow);
}

/// DAA (Decimal Adjust Accumulator)
/// The eight-bit number in the accumulator is adjusted
/// to form two four-bit Binary-Coded-Decimal digits by
/// the following process:
/// 1. If the value of the least significant 4 bits of the
/// accumulator is greater than 9 or if the AC flag
/// is set, 6 is added to the accumulator.
/// 2. If the value of the most significant 4 bits of the
/// accumulator is now greater than 9, or if the CY
/// flag is set, 6 is added to the most significant 4
/// bits of the accumulator.
/// NOTE: All flags are affected.
fn opcode_daa(state: &mut ProcessorState) {
    // 0 | 0 | 1 | 0 | 0 | 1 | 1 | 1
    let low = state.reg_a & 0x0f;
    let high = (state.reg_a & 0xf0) >> 4;
    let aux_set = state.flags & ConditionFlags::AC == ConditionFlags::AC;
    let carry_set = state.flags & ConditionFlags::CY == ConditionFlags::CY;
    let mut correction: u8 = 0;
    state.prog_counter += 1;

    if low > 0b1001 || aux_set {
        correction += 0b0110;
        if low + correction > 0b1111 {
            state.flags |= ConditionFlags::AC;
        }
    }
    if carry_set || high > 0b1001 || (high == 0b1001 && low > 0b1001) {
        correction += 0b0110_0000;
        state.flags |= ConditionFlags::CY;
    }
    state.reg_a = state.reg_a.wrapping_add(correction);
    state.flags.set_zsp(state.reg_a);
}

/// ANA r (AND Register)
/// (A) ~ (A) /\ (r)
/// The content of register r is logically anded with the
/// content of the accumulator. The result is placed in
/// the accumulator. The CY flag is cleared.
/// ANA M (AND memory)
/// (A) ~ (A) /\ ((H) (L))
/// The contents of the memory location whose address
/// is contained in the H and L registers is logically anded
/// with the content of the accumulator. The result is
/// placed in the accumulator. The CY flag is cleared.
fn opcode_ana(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 0 | 1 | 0 | 0 | S | S | S
    // or
    // 1 | 0 | 1 | 0 | 0 | 1 | 1 | 0
    let cur_instruction = mem_map[state.prog_counter];
    let src = get_source_register_bit_pattern(cur_instruction);
    state.prog_counter += 1;

    let reg = match state.get_reg_value(src) {
        Some(reg) => reg,
        None => state.get_mem_value(RPairBitPattern::HL, mem_map),
    };
    state.reg_a &= reg;
    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
}

/// ANI data (AND immediate)
/// (A) ~ (A) /\ (byte 2)
/// The content of the second byte of the instruction is
/// logically anded with the contents of the accumulator.
/// The result is placed in the accumulator. The CY and
/// AC flags are cleared
fn opcode_ani(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 1 | 1 | 0 | 0 | 1 | 1 | 0
    let second_byte = mem_map[state.prog_counter + 1];
    state.prog_counter += 2;
    state.reg_a &= second_byte;
    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
}

/// XRA r (Exclusive OR Register)
/// (A) ~ (A) ⊻ (r)
/// The content of register r is exclusive-or'd with the
/// content of the accumulator. The result is placed in
/// the accumulator. The CY and AC flags are cleared
/// XRA M (Exclusive OR memory)
/// (A) ~ (A) ⊻ ((H) (L))
/// The content of the memory location whose address
/// is contained in the Hand L registers is exclusive-OR'd
/// with the content of the accumulator. The result is
/// placed in the accumulator. The CY and AC flags are
/// cleared.
fn opcode_xra(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 0 | 1 | 0 | 1 | S | S | S
    // or
    // 1 | 0 | 1 | 0 | 1 | 1 | 1 | 0
    let cur_instruction = mem_map[state.prog_counter];
    let src = get_source_register_bit_pattern(cur_instruction);
    state.prog_counter += 1;

    let reg = match state.get_reg_value(src) {
        Some(reg) => reg,
        None => state.get_mem_value(RPairBitPattern::HL, mem_map),
    };
    state.reg_a ^= reg;
    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
}

/// XRI data (Exclusive OR immediate)
/// (A) ~ (A) ⊻ (byte 2)
/// The content of the second byte of the instruction is
/// logically anded with the contents of the accumulator.
/// The result is placed in the accumulator. The CY and
/// AC flags are cleared
fn opcode_xri(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 1 | 1 | 0 | 1 | 1 | 1 | 0
    let second_byte = mem_map[state.prog_counter + 1];
    state.prog_counter += 2;
    state.reg_a ^= second_byte;
    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
}

/// ORA r (OR Register)
/// (A) ~ (A) V (r)
/// The content of register r is inclusive-OR'd with the
/// content of the accumulator. The result is placed in
/// the accumulator. The CY and AC flags are cleared.
/// ORA M (OR memory)
/// (A) ~ (A) V ((H) (L))
/// The content of the memory location whose address is
/// contained in the Hand L registers is inclusive-OR'd
/// with the content of the accumu lator. The result is
/// placed in the accumulator. The CY and AC flags are
/// cleared.
fn opcode_ora(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 0 | 1 | 1 | 0 | S | S | S
    // or
    // 1 | 0 | 1 | 1 | 0 | 1 | 1 | 0
    let cur_instruction = mem_map[state.prog_counter];
    let src = get_source_register_bit_pattern(cur_instruction);
    state.prog_counter += 1;

    let reg = match state.get_reg_value(src) {
        Some(reg) => reg,
        None => state.get_mem_value(RPairBitPattern::HL, mem_map),
    };
    state.reg_a |= reg;
    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
}

/// ORI data (OR immediate)
/// (A) ~ (A) V (byte 2)
/// The content of the second byte of the instruction is
/// inclusive-OR'd with the content of the accumulator.
/// The result is placed in the accumulator. The CY and
/// AC flags are cleared.
fn opcode_ori(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 1 | 1 | 1 | 0 | 1 | 1 | 0
    let second_byte = mem_map[state.prog_counter + 1];
    state.prog_counter += 2;
    state.reg_a |= second_byte;
    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(state.reg_a);
}

/// CMP r (Compare Register)
/// (A) - (r)
/// The content of register r is subtracted from the
/// accumulator. The accumulator remains unchanged. The
/// condition flags are set as a result of the subtraction.
/// The Z flag is set to 1 if (A) = (r). The CY flag is
/// set to 1 if (A) < (r) .
/// CMP M (Compare memory)
/// (A) - ((H) (L))
/// The content of the memory location whose address
/// is contained in the Hand L registers is subtracted
/// from the accumulator. The accumulator remains unchanged.
/// The condition flags are set as a result of the
/// subtraction. The Z flag is set to 1 if (A) = ((H) (L)).
/// The CY flag is set to 1 if (A) < ((H) (L)).
fn opcode_cmp(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 0 | 1 | 1 | 1 | S | S | S
    // or
    // 1 | 0 | 1 | 1 | 1 | 1 | 1 | 0
    let cur_instruction = mem_map[state.prog_counter];
    let src = get_source_register_bit_pattern(cur_instruction);
    state.prog_counter += 1;

    let subtrahend = match state.get_reg_value(src) {
        Some(subtrahend) => subtrahend,
        None => state.get_mem_value(RPairBitPattern::HL, mem_map),
    };
    let twos_complement = subtrahend.wrapping_neg();

    // Add first four bits to detect carry to fifth.
    let low_add = (state.reg_a & 0x0f) + (twos_complement & 0x0f);
    let aux_carry = low_add & 0x10 != 0;

    let difference = state.reg_a.wrapping_add(twos_complement);

    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(difference); // To set S & P; Z is overwritten in next line.
    state.flags.set(ConditionFlags::Z, difference == 0);
    state
        .flags
        .set(ConditionFlags::CY, state.reg_a < subtrahend);
    state.flags.set(ConditionFlags::AC, aux_carry);
}

/// CPI data (Compare immediate)
/// (A) - (byte 2)
/// The content of the second byte of the instruction is
/// subtracted from the accumulator. The condition flags
/// are set by the result of the subtraction. The Z flag is
/// set to 1 if (A) = (byte 2). The CY flag is set to 1 if
/// (A) < (byte 2).
fn opcode_cpi(state: &mut ProcessorState, mem_map: &mut MemMap) {
    // 1 | 1 | 1 | 1 | 1 | 1 | 1 | 0
    let second_byte = mem_map[state.prog_counter + 1];
    state.prog_counter += 2;
    let twos_complement = second_byte.wrapping_neg();

    // Add first four bits to detect carry to fifth.
    let low_add = (state.reg_a & 0x0f) + (twos_complement & 0x0f);
    let aux_carry = low_add & 0x10 != 0;

    let difference = state.reg_a.wrapping_add(twos_complement);

    state.flags = ConditionFlags::empty();
    state.flags.set_zsp(difference); // To set S & P; Z is overwritten in next line.
    state.flags.set(ConditionFlags::Z, difference == 0);
    state
        .flags
        .set(ConditionFlags::CY, state.reg_a < second_byte);
    state.flags.set(ConditionFlags::AC, aux_carry);
}

/// RLC (Rotate Left)
/// (An+l) ~ (An) ; (AO) ~ (A7)
/// (CY) ~ (A7)
/// The content of the accumulator is rotated left one
/// position. The low order bit and the CY flag are both
/// set to the value shifted out of the high order bit
/// position. Only the CY flag is affected.
fn opcode_rlc(state: &mut ProcessorState) {
    // 0 | 0 | 0 | 0 | 0 | 1 | 1 | 1
    state.prog_counter += 1;
    state.reg_a = state.reg_a.rotate_left(1);
    state
        .flags
        .set(ConditionFlags::CY, (state.reg_a & 0x01) == 1);
}

/// RRC (Rotate Right)
/// (An) ~ (An-,); (A7) (AO)
/// (CY) (AO)
/// The content of the accumulator is rotated right one
/// position. The high order bit and the CY flag are both
/// set to the value shifted out of the low order bit
/// position. Only the CY flag is affected.
fn opcode_rrc(state: &mut ProcessorState) {
    // 0 | 0 | 0 | 0 | 1 | 1 | 1 | 1
    state.prog_counter += 1;
    state.reg_a = state.reg_a.rotate_right(1);
    state.flags.set(
        ConditionFlags::CY,
        (state.reg_a & 0b1000_0000) == 0b1000_0000,
    );
}

/// RAL (Rotate left through carry)
/// (An+1) ~ (An) ; (CY) ~ (A7)
/// (A0) ~ (CY)
/// The content of the accumulator is rotated left one
/// position through the CY flag. The low order bit is set
/// equal to the CY flag and the CY flag is set to the
/// value shifted out of the high order bit. Only the CY
/// flag is affected.
fn opcode_ral(state: &mut ProcessorState) {
    // 0 | 0 | 0 | 0 | 0 | 1 | 1 | 1
    state.prog_counter += 1;
    let high_bit = state.reg_a >> 7;
    state.reg_a <<= 1;
    state.reg_a |= (state.flags & ConditionFlags::CY != ConditionFlags::empty()) as u8;
    state.flags.set(ConditionFlags::CY, high_bit != 0);
}

/// RAR (Rotate right through carry)
/// (An) ~ (An+l); (CY) ~ (AO)
/// (A7) ~ (CY)
/// The content of the accumulator is rotated right one
/// position through the CY flag. The high order bit is set
/// to the CY flag and the CY flag is set to the value
/// shifted out of the low order bit. Only the CY flag is
/// affected
fn opcode_rar(state: &mut ProcessorState) {
    // 0 | 0 | 0 | 1 | 1 | 1 | 1 | 1
    state.prog_counter += 1;
    let low_bit = state.reg_a & 0x01;
    state.reg_a >>= 1;
    state.reg_a |= ((state.flags & ConditionFlags::CY != ConditionFlags::empty()) as u8) << 7;
    state.flags.set(ConditionFlags::CY, low_bit != 0);
}

/// CMA (Complement accumulator)
/// The contents of the accumulator are complemented-
/// (zero bits become 1, one bits become 0). No flags are
/// affected.
fn opcode_cma(state: &mut ProcessorState) {
    // 0 | 0 | 1 | 0 | 1 | 1 | 1 | 1
    state.prog_counter += 1;
    state.reg_a = !state.reg_a;
}

/// CMC (Complement carry)
/// The CY flag is complemented. No other flags are
/// affected.
fn opcode_cmc(state: &mut ProcessorState) {
    // 0 | 0 | 1 | 1 | 1 | 1 | 1 | 1
    state.prog_counter += 1;
    state.flags.toggle(ConditionFlags::CY)
}

/// STC (Set carry)
/// (CY) ~ 1
/// The CY flag is set to 1. No other flags are affected.
fn opcode_stc(state: &mut ProcessorState) {
    // 0 | 0 | 1 | 1 | 0 | 1 | 1 | 1
    state.prog_counter += 1;
    state.flags.set(ConditionFlags::CY, true)
}

// DDD or SSS REGISTER NAME
// 111 A
// 000 B
// 001 C
// 010 D
// 011 E
// 100 H
// 101 L

// Register pair bits
// 00 B-C
// 01 D-E
// 10 H-L
// 11 SP (not really a pair)
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
fn opcode_rst(state: &mut ProcessorState, mem_map: &mut MemMap, int_num: ProcessorInterrupt) {
    let rst_address = ((int_num as u8) << 3).into();
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

/// IN port (I nput)
/// (A) ~ (data)
/// The data placed on the eight bit bi-directional data
/// bus by the specified port is moved to register A.
fn opcode_in(state: &mut ProcessorState, mem_map: &mut MemMap, ports: Arc<Mutex<PortState>>) {
    let port_state = ports.lock().unwrap();
    let second_byte = mem_map[state.prog_counter + 1];

    match second_byte {
        0x01 => {
            state.reg_a = port_state.read_port_1;
        }
        0x02 => {
            state.reg_a = port_state.read_port_2;
        }
        0x03 => {
            state.reg_a = port_state.write_port_4 << port_state.write_port_2;
        }
        _ => {
            panic!("Unexpected port in opcode_in")
        }
    }
    state.prog_counter += 2;
}

/// OUT port (Output)
/// (data) ~ (A)
/// The content of register A is placed on the eight bit
/// bi-directional data bus for transmission to the spec-
/// ified port.
fn opcode_out(state: &mut ProcessorState, mem_map: &mut MemMap, ports: Arc<Mutex<PortState>>) {
    let mut port_state = ports.lock().unwrap();
    let second_byte = mem_map[state.prog_counter + 1];
    match second_byte {
        0x01 => {
            port_state.write_port_1 = state.reg_a;
        }
        0x02 => {
            port_state.write_port_2 = state.reg_a;
        }
        0x04 => {
            port_state.write_port_4 = state.reg_a;
        }
        // Sound port
        0x03 => {}
        // Sound port
        0x05 => {}
        // Todo: watchdog, do we even care to emulate this?
        0x06 => {}
        _ => {
            panic!("Unexpected port in opcode_out")
        }
    }
    state.prog_counter += 2;
}

/// EI (Enable interrupts)
/// The interrupt system is enabled following the execu-
/// tion of the next instruction.
fn opcode_ei(state: &mut ProcessorState, _mem_map: &mut MemMap) {
    // The reference implementation at emulators101 didn't care much
    // about the interrupt timer only being enabled after the next
    // instruction and neither will we.
    state.interrupts_enabled = true;
    state.prog_counter += 1;
}

/// 01 (Disable interrupts)
/// The interrupt system is disabled immediately fol-
/// lowing the execution of the 01 instruction.
fn opcode_di(state: &mut ProcessorState, _mem_map: &mut MemMap) {
    state.interrupts_enabled = false;
    state.prog_counter += 1;
}

/// HLT (Halt)
/// The processor is stopped. The registers and flags are
/// unaffected.
fn opcode_halt(_state: &mut ProcessorState, _mem_map: &mut MemMap) {
    std::process::exit(0);
}

// Todo: change visibility of attrs and methods to pub(crate)

// Todo: See if we can convert the Arc::Mutex to an Rc::RefCell, there is certainly
// a space invaders 8080 emulator  implementation in wasm32 that does this.

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::space_invaders_rom;
    use wasm_bindgen_test::*;

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn basic_nop_step() {
        let mut machine_state = MachineState::new();
        let cur_instruction_address = machine_state.processor_state.prog_counter;
        let test_rom = [0; 8192];
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(
            machine_state.processor_state.prog_counter,
            cur_instruction_address + 1
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn basic_jmp_step() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b1100_0011;
        let test_address: u16 = 0x0020;
        test_rom[1] = test_address as u8;
        test_rom[2] = (test_address >> 8) as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(test_address, machine_state.processor_state.prog_counter);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn jnz_step() {
        let mut machine_state = MachineState::new();
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
        machine_state.mem_map.rom = test_rom;

        // assert not equal
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, true);
        machine_state.iterate_processor_state();
        assert_ne!(machine_state.processor_state.prog_counter, test_address);

        // assert equal
        machine_state.processor_state.prog_counter = 0;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, false);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, test_address);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn jz_step() {
        let mut machine_state = MachineState::new();
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
        machine_state.mem_map.rom = test_rom;

        // assert not equal
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, false);
        machine_state.iterate_processor_state();
        assert_ne!(machine_state.processor_state.prog_counter, test_address);

        // assert equal
        machine_state.processor_state.prog_counter = 0;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, true);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, test_address);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn lxi_step_bc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair b-c is 00
        test_rom[0] = (0b00 << 4) | 0b0000_0001;
        test_rom[1] = 0xff;
        test_rom[2] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_b, test_rom[2]);
        assert_eq!(machine_state.processor_state.reg_c, test_rom[1]);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn lxi_step_de() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair d-e is 01
        test_rom[0] = (0b01 << 4) | 0b0000_0001;
        test_rom[1] = 0xff;
        test_rom[2] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_d, test_rom[2]);
        assert_eq!(machine_state.processor_state.reg_e, test_rom[1]);
    }
    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn lxi_step_hl() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair h-l is 10
        test_rom[0] = (0b10 << 4) | 0b0000_0001;
        test_rom[1] = 0xff;
        test_rom[2] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_h, test_rom[2]);
        assert_eq!(machine_state.processor_state.reg_l, test_rom[1]);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn lxi_step_sp() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register sp is 11
        test_rom[0] = (0b11 << 4) | 0b0000_0001;
        test_rom[1] = 0xff;
        test_rom[2] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(
            machine_state.processor_state.stack_pointer,
            ((test_rom[2] as u16) << 8) | (test_rom[1] as u16)
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn lxi_step_good_pc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register sp is 11
        test_rom[0] = (0b11 << 4) | 0b0000_0001;
        test_rom[1] = 0xff;
        test_rom[2] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, 3);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mvi_step_reg_a() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 111 is a
        test_rom[0] = 0b00_000_110 | (0b111 << 3);
        test_rom[1] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0xff);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mvi_step_reg_b() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 000 is b
        test_rom[0] = 0b00_000_110 | (0b000 << 3);
        test_rom[1] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_b, 0xff);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mvi_step_reg_c() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 001 is c
        test_rom[0] = 0b00_000_110 | (0b001 << 3);
        test_rom[1] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_c, 0xff);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mvi_step_reg_d() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 010 is d
        test_rom[0] = 0b00_000_110 | (0b010 << 3);
        test_rom[1] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_d, 0xff);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mvi_step_reg_e() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 011 is e
        test_rom[0] = 0b00_000_110 | (0b011 << 3);
        test_rom[1] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_e, 0xff);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mvi_step_reg_h() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 100 is h
        test_rom[0] = 0b00_000_110 | (0b100 << 3);
        test_rom[1] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_h, 0xff);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mvi_step_reg_l() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 010 is l
        test_rom[0] = 0b00_000_110 | (0b101 << 3);
        test_rom[1] = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_l, 0xff);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mvi_step_mem() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 110 is memory
        test_rom[0] = 0b00_000_110 | (0b110 << 3);
        test_rom[1] = 0xff;
        machine_state.processor_state.reg_h =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        machine_state.processor_state.reg_l =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(
            machine_state.mem_map[space_invaders_rom::SPACE_INVADERS_ROM.len() as u16],
            0xff
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mvi_step_good_pc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // 110 is memory
        test_rom[0] = 0b00_000_110 | (0b110 << 3);
        test_rom[1] = 0xff;
        machine_state.processor_state.reg_h =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        machine_state.processor_state.reg_l =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, 2);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn add_step_reg_a() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_000_000 | RegisterBitPattern::A as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1111_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b1111_1110);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::CY | ConditionFlags::S | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn add_step_reg_b() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_000_000 | RegisterBitPattern::B as u8;
        test_rom[0] = 0b10_000_000 | (0b000);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_0000;
        machine_state.processor_state.reg_b = 0b0000_0000;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::Z | ConditionFlags::P
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn add_step_reg_c() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_000_000 | RegisterBitPattern::C as u8;
        test_rom[0] = 0b10_000_000 | (0b001);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0101_0101;
        machine_state.processor_state.reg_c = 0b1010_1010;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b1111_1111);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::S | ConditionFlags::P
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn add_step_reg_d() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_000_000 | RegisterBitPattern::D as u8;
        test_rom[0] = 0b10_000_000 | (0b010);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_0000;
        machine_state.processor_state.reg_d = 0b1111_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b1111_1111);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::S | ConditionFlags::P
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn add_step_reg_e() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_000_000 | RegisterBitPattern::E as u8;
        test_rom[0] = 0b10_000_000 | (0b011);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1111_1111;
        machine_state.processor_state.reg_e = 0x01;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::Z | ConditionFlags::CY | ConditionFlags::AC | ConditionFlags::P
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn add_step_reg_h() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_000_000 | RegisterBitPattern::H as u8;
        test_rom[0] = 0b10_000_000 | (0b100);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1001_1001;
        machine_state.processor_state.reg_h = 0b1001_1001;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0011_0010);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::CY | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn add_step_reg_l() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_000_000 | RegisterBitPattern::L as u8;
        test_rom[0] = 0b10_000_000 | (0b101);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0x01;
        machine_state.processor_state.reg_l = 0b1111_1110;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b1111_1111);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::S | ConditionFlags::P
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn add_step_mem() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_000_000 | RegisterBitPattern::Other as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0x01;
        machine_state.processor_state.reg_h =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        machine_state.processor_state.reg_l =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        machine_state.processor_state.reg_l += 0b1000;
        let address = machine_state.processor_state.get_rp(RPairBitPattern::HL);
        machine_state.mem_map[address] = 0x10;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0001_0001);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::P)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn adi_step() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b11_000_110;
        test_rom[1] = 0x0f;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0x01;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0x10);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::AC)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn adc_step_reg_a() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_001_000 | (RegisterBitPattern::A as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.flags.bits = 0x01;
        machine_state.processor_state.reg_a = 0x01;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0011);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::P)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn adc_step_reg_b() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_001_000 | (RegisterBitPattern::B as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.flags.bits = 0x01;
        machine_state.processor_state.reg_a = 0x01;
        machine_state.processor_state.reg_b = 0x0f;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0001_0001);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn adc_step_reg_c() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_001_000 | (RegisterBitPattern::C as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.flags.bits = 0b0000_0000;
        machine_state.processor_state.reg_a = 0x01;
        machine_state.processor_state.reg_c = 0b1111_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::AC | ConditionFlags::CY | ConditionFlags::Z
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn adc_step_reg_d() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_001_000 | (RegisterBitPattern::D as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.flags.bits = 0x01;
        machine_state.processor_state.reg_a = 0b0000_0000;
        machine_state.processor_state.reg_d = 0b1111_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::AC | ConditionFlags::CY | ConditionFlags::Z
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn adc_step_reg_e() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_001_000 | (RegisterBitPattern::E as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.flags.bits = 0x01;
        machine_state.processor_state.reg_a = 0b1111_1111;
        machine_state.processor_state.reg_e = 0x01;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0x01);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::AC | ConditionFlags::CY
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn adc_step_reg_h() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_001_000 | (RegisterBitPattern::H as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.flags.bits = 0b0000_0000;
        machine_state.processor_state.reg_a = 0b1001_1001;
        machine_state.processor_state.reg_h = 0b1001_1001;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0011_0010);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::AC | ConditionFlags::CY
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn adc_step_reg_l() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_001_000 | (RegisterBitPattern::L as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.flags.bits = 0x01;
        machine_state.processor_state.reg_a = 0x01;
        machine_state.processor_state.reg_l = 0x10;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0001_0010);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::P)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn adc_step_mem() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_001_000 | (RegisterBitPattern::Other as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.flags.bits = 0x01;
        machine_state.processor_state.reg_a = 0x01;
        machine_state.processor_state.reg_h =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        machine_state.processor_state.reg_l =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        let address = ((machine_state.processor_state.reg_h as u16) << 8)
            + machine_state.processor_state.reg_l as u16;
        machine_state.mem_map[address.into()] = 0x10;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0001_0010);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::P)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn aci() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b11_001_110;
        test_rom[1] = 0x0f;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.flags.bits = 0x01;
        machine_state.processor_state.reg_a = 0x01;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0001_0001);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::AC | ConditionFlags::P
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sub_step_reg_a() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_010_000 | (RegisterBitPattern::A as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1111_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::Z | ConditionFlags::P | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sub_step_reg_b() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_010_000 | (RegisterBitPattern::B as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_0000;
        machine_state.processor_state.reg_b = 0b0000_0000;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::Z | ConditionFlags::P | ConditionFlags::CY
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sub_step_reg_c() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_010_000 | (RegisterBitPattern::C as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0101_0101;
        machine_state.processor_state.reg_c = 0b1010_1010;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b1010_1011);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::S | ConditionFlags::CY
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sub_step_reg_d() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_010_000 | (RegisterBitPattern::D as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_0000;
        machine_state.processor_state.reg_d = 0b1111_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0x01);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::CY)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sub_step_reg_e() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_010_000 | (RegisterBitPattern::E as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1111_1111;
        machine_state.processor_state.reg_e = 0x01;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b1111_1110);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::S | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sub_step_reg_h() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_010_000 | (RegisterBitPattern::H as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1001_1001;
        machine_state.processor_state.reg_h = 0b1001_1001;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::Z | ConditionFlags::P | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sub_step_reg_l() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_010_000 | (RegisterBitPattern::L as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0x01;
        machine_state.processor_state.reg_l = 0b1111_1110;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0011);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::CY
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sub_step_mem() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_010_000 | (RegisterBitPattern::Other as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0001_0001;
        machine_state.processor_state.reg_h =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        machine_state.processor_state.reg_l =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        let address = ((machine_state.processor_state.reg_h as u16) << 8)
            + machine_state.processor_state.reg_l as u16;
        machine_state.mem_map[address.into()] = 0x10;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0x01);
        assert_eq!(machine_state.processor_state.flags.bits, 0b0000_0000);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sbb_step_reg_c() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_011_000 | (RegisterBitPattern::C as u8);
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_0100;
        machine_state.processor_state.reg_c = 0b0000_0010;
        machine_state.processor_state.flags = ConditionFlags::CY;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0x01);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::AC)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sui() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b11_010_110;
        test_rom[1] = 0b0000_0100;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0x0f;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_1011);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::AC)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sbi() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b11_011_110;
        test_rom[1] = 0b0000_0010;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_0100;
        machine_state.processor_state.flags = ConditionFlags::CY;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0x01);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::AC)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn inr_reg_a() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_100 | (RegisterBitPattern::A as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_0100;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0101);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::P)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn inr_reg_b() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_100 | (RegisterBitPattern::B as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_b = 0b1111_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_b, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::Z | ConditionFlags::P | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn inr_reg_c() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_100 | (RegisterBitPattern::C as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_c = 0b0111_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_c, 0b1000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::S | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn inr_reg_d() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_100 | (RegisterBitPattern::D as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_d = 0x10;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_d, 0b0001_0001);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::P)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn inr_reg_e() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_100 | (RegisterBitPattern::E as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_e = 0x0f;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_e, 0x10);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::AC)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn inr_reg_h() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_100 | (RegisterBitPattern::H as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_h = 0b1000_0001;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_h, 0b1000_0010);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::S
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn inr_reg_l() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_100 | (RegisterBitPattern::L as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_l = 0b1111_0000;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_l, 0b1111_0001);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::S)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn inr_mem() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_100 | (RegisterBitPattern::Other as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_h =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        machine_state.processor_state.reg_l =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        let address = ((machine_state.processor_state.reg_h as u16) << 8)
            + machine_state.processor_state.reg_l as u16;
        machine_state.mem_map[address.into()] = 0b0000_0000;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.mem_map[address.into()], 0x01);
        assert_eq!(machine_state.processor_state.flags.bits, 0b0000_0000)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcr_reg_a() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_101 | (RegisterBitPattern::A as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_0100;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0011);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcr_reg_b() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_101 | (RegisterBitPattern::B as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_b = 0b1111_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_b, 0b1111_1110);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::AC | ConditionFlags::S
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcr_reg_c() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_101 | (RegisterBitPattern::C as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_c = 0b0111_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_c, 0b0111_1110);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::AC | ConditionFlags::P
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcr_reg_d() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_101 | (RegisterBitPattern::D as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_d = 0x10;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_d, 0x0f);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::P)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcr_reg_e() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_101 | (RegisterBitPattern::E as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_e = 0x0f;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_e, 0b0000_1110);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::AC)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcr_reg_h() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_101 | (RegisterBitPattern::H as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_h = 0b1000_0001;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_h, 0b1000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::S | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcr_reg_l() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_101 | (RegisterBitPattern::L as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_l = 0x01;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_l, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::Z | ConditionFlags::P | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcr_mem() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_101 | (RegisterBitPattern::Other as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_h =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        machine_state.processor_state.reg_l =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        let address = ((machine_state.processor_state.reg_h as u16) << 8)
            + machine_state.processor_state.reg_l as u16;
        machine_state.mem_map[address.into()] = 0b0000_0000;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.mem_map[address.into()], 0b1111_1111);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::S
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcr_debug_issue_1() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_101 | (RegisterBitPattern::B as u8) << 3;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0x00;
        machine_state.processor_state.reg_b = 0x08;
        machine_state.processor_state.reg_c = 0x1c;
        machine_state.processor_state.reg_d = 0x1f;
        machine_state.processor_state.reg_e = 0x31;
        machine_state.processor_state.reg_h = 0x24;
        machine_state.processor_state.reg_l = 0x3e;
        machine_state.processor_state.flags = ConditionFlags::Z | ConditionFlags::P;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_b, 0x07);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::AC)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcx_bc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_00_1011 | (RPairBitPattern::BC as u8) << 4;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .set_rp(0xFF00, RPairBitPattern::BC);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_b, 0b1111_1110);
        assert_eq!(machine_state.processor_state.reg_c, 0b1111_1111);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcx_de() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_00_1011 | (RPairBitPattern::DE as u8) << 4;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .set_rp(0x0000, RPairBitPattern::DE);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_d, 0b1111_1111);
        assert_eq!(machine_state.processor_state.reg_e, 0b1111_1111);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcx_hl() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_00_1011 | (RPairBitPattern::HL as u8) << 4;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .set_rp(0xFFFF, RPairBitPattern::HL);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_h, 0b1111_1111);
        assert_eq!(machine_state.processor_state.reg_l, 0b1111_1110);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dcx_sp() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_00_1011 | (RPairBitPattern::SP as u8) << 4;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .set_rp(0x0FF0, RPairBitPattern::SP);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.stack_pointer, 0x0FEF);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dad_bc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_00_1001 | (RPairBitPattern::BC as u8) << 4;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .set_rp(0x0000, RPairBitPattern::BC);
        machine_state
            .processor_state
            .set_rp(0x0000, RPairBitPattern::HL);
        machine_state.iterate_processor_state();
        assert_eq!(
            machine_state.processor_state.get_rp(RPairBitPattern::HL),
            0x0000
        );
        assert_eq!(machine_state.processor_state.flags.bits, 0b0000_0000)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dad_de() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_00_1001 | (RPairBitPattern::DE as u8) << 4;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .set_rp(0x0000, RPairBitPattern::DE);
        machine_state
            .processor_state
            .set_rp(0xFFFF, RPairBitPattern::HL);
        machine_state.iterate_processor_state();
        assert_eq!(
            machine_state.processor_state.get_rp(RPairBitPattern::HL),
            0xFFFF
        );
        assert_eq!(machine_state.processor_state.flags.bits, 0b0000_0000)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dad_hl() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_00_1001 | (RPairBitPattern::HL as u8) << 4;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .set_rp(0xFFFF, RPairBitPattern::HL);
        machine_state.iterate_processor_state();
        assert_eq!(
            machine_state.processor_state.get_rp(RPairBitPattern::HL),
            0xFFFE
        );
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::CY)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn dad_sp() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_00_1001 | (RPairBitPattern::SP as u8) << 4;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .set_rp(0xFFFF, RPairBitPattern::SP);
        machine_state
            .processor_state
            .set_rp(0x0001, RPairBitPattern::HL);
        machine_state.iterate_processor_state();
        assert_eq!(
            machine_state.processor_state.get_rp(RPairBitPattern::HL),
            0x0000
        );
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::CY)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn daa() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_100_111;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0001_0001;
        machine_state.processor_state.flags = ConditionFlags::AC;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0001_0111);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ana_reg_a() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_100_000 | RegisterBitPattern::A as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_0000;
        machine_state.processor_state.flags = ConditionFlags::CY | ConditionFlags::AC;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::Z | ConditionFlags::P
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ana_other_reg() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_100_000 | RegisterBitPattern::B as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1111_1100;
        machine_state.processor_state.reg_b = 0b0000_1111;
        machine_state.processor_state.flags = ConditionFlags::CY;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_1100);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::P)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ana_mem() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_100_000 | RegisterBitPattern::Other as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0xff;
        machine_state.processor_state.reg_h =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        machine_state.processor_state.reg_l =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        let address = ((machine_state.processor_state.reg_h as u16) << 8)
            + machine_state.processor_state.reg_l as u16;
        machine_state.mem_map[address.into()] = 0xff;
        machine_state.processor_state.flags = ConditionFlags::CY;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0xff);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::S
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ani() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b11_100_110;
        test_rom[1] = 0b0000_0100;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_1111;
        machine_state.processor_state.flags = ConditionFlags::AC;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0100);
        assert_eq!(machine_state.processor_state.flags.bits, 0b0000_0000)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn xra_reg_a() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_101_000 | RegisterBitPattern::A as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1111_1111;
        machine_state.processor_state.flags = ConditionFlags::CY | ConditionFlags::AC;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::Z | ConditionFlags::P
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn xra_other_reg() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_101_000 | RegisterBitPattern::B as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1111_1100;
        machine_state.processor_state.reg_b = 0b0000_1111;
        machine_state.processor_state.flags = ConditionFlags::CY;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b1111_0011);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::S
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn xra_mem() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_101_000 | RegisterBitPattern::Other as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0xff;
        machine_state.processor_state.reg_h =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        machine_state.processor_state.reg_l =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        let address = ((machine_state.processor_state.reg_h as u16) << 8)
            + machine_state.processor_state.reg_l as u16;
        machine_state.mem_map[address.into()] = 0xff;
        machine_state.processor_state.flags = ConditionFlags::CY;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0x00);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::Z
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn xri() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b11_101_110;
        test_rom[1] = 0b0000_0100;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_1111;
        machine_state.processor_state.flags = ConditionFlags::AC;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_1011);
        assert_eq!(machine_state.processor_state.flags.bits, 0b0000_0000)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ora_reg_a() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_110_000 | RegisterBitPattern::A as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_0000;
        machine_state.processor_state.flags = ConditionFlags::CY | ConditionFlags::AC;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_0000);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::Z | ConditionFlags::P
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ora_other_reg() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_110_000 | RegisterBitPattern::B as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1111_1100;
        machine_state.processor_state.reg_b = 0b0000_1111;
        machine_state.processor_state.flags = ConditionFlags::CY;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b1111_1111);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::S
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ora_mem() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_110_000 | RegisterBitPattern::Other as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0xff;
        machine_state.processor_state.reg_h =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        machine_state.processor_state.reg_l =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        let address = ((machine_state.processor_state.reg_h as u16) << 8)
            + machine_state.processor_state.reg_l as u16;
        machine_state.mem_map[address.into()] = 0xff;
        machine_state.processor_state.flags = ConditionFlags::CY;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0xff);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::P | ConditionFlags::S
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ori() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b11_110_110;
        test_rom[1] = 0b0000_0100;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_1111;
        machine_state.processor_state.flags = ConditionFlags::AC;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_1111);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::P)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn cmp_reg_a() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_111_000 | RegisterBitPattern::A as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_1111);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::Z | ConditionFlags::P | ConditionFlags::AC
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn cmp_other_reg() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_111_000 | RegisterBitPattern::D as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_1111;
        machine_state.processor_state.reg_d = 0b1111_1111;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_1111);
        assert_eq!(machine_state.processor_state.reg_d, 0b1111_1111);
        assert_eq!(
            machine_state.processor_state.flags,
            ConditionFlags::AC | ConditionFlags::CY
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn cmp_mem() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b10_111_000 | RegisterBitPattern::Other as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_h =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) >> 8) as u8;
        machine_state.processor_state.reg_l =
            ((space_invaders_rom::SPACE_INVADERS_ROM.len() as u16) & 0x00ff) as u8;
        let address = ((machine_state.processor_state.reg_h as u16) << 8)
            + machine_state.processor_state.reg_l as u16;
        machine_state.mem_map[address.into()] = 0b1000_0000;
        machine_state.processor_state.reg_a = 0b1000_0001;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b1000_0001);
        assert_eq!(machine_state.mem_map[address.into()], 0b1000_0000);
        assert_eq!(machine_state.processor_state.flags.bits, 0b0000_0000)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn cpi() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b11_111_110;
        test_rom[1] = 0b0000_1000;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b0000_1111;
        machine_state.processor_state.flags = ConditionFlags::AC;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_1111);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::AC)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn rlc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_000_111;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1010_1010;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0101_0101);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::CY)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn rrc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_001_111;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1010_1010;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0101_0101);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::empty())
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ral() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_010_111;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1010_1010;
        machine_state.processor_state.flags = ConditionFlags::CY;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0101_0101);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::CY)
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn rar() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_011_111;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1010_1010;
        machine_state.processor_state.flags = ConditionFlags::CY;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b1101_0101);
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::empty())
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn cma() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_101_111;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0b1111_0000;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0b0000_1111);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn cmc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_111_111;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.flags = ConditionFlags::CY;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::empty());
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn stc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        test_rom[0] = 0b00_110_111;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.flags, ConditionFlags::CY);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn call_uncon() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;
        let orig_stack_add = machine_state.processor_state.stack_pointer;
        // 0b1100_1101 is unconditional call
        test_rom[0] = 0b1100_1101;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, 0x0020);
        assert_eq!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        )
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn call_nz() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;
        let orig_stack_add = machine_state.processor_state.stack_pointer;
        // 0b11_000_100 is cnz
        test_rom[0] = 0b11_000_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, true);
        machine_state.iterate_processor_state();

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(machine_state.processor_state.prog_counter, 0x0020);
        assert_ne!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
        machine_state.processor_state.prog_counter = 0;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, false);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, 0x0020);
        assert_eq!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn call_z() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;
        let orig_stack_add = machine_state.processor_state.stack_pointer;
        // 0b11_001_100 is cz
        test_rom[0] = 0b11_001_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, false);
        machine_state.iterate_processor_state();

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(machine_state.processor_state.prog_counter, 0x0020);
        assert_ne!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
        machine_state.processor_state.prog_counter = 0;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, true);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, 0x0020);
        assert_eq!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn call_nc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;
        let orig_stack_add = machine_state.processor_state.stack_pointer;
        // 0b11_010_100 is cnc
        test_rom[0] = 0b11_010_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::CY, true);
        machine_state.iterate_processor_state();

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(machine_state.processor_state.prog_counter, 0x0020);
        assert_ne!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
        machine_state.processor_state.prog_counter = 0;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::CY, false);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, 0x0020);
        assert_eq!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn call_c() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;
        let orig_stack_add = machine_state.processor_state.stack_pointer;
        // 0b11_011_100 is cc
        test_rom[0] = 0b11_011_100;
        machine_state.processor_state.stack_pointer = 0x2400;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::CY, false);
        machine_state.iterate_processor_state();

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(machine_state.processor_state.prog_counter, 0x0020);
        assert_ne!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
        machine_state.processor_state.prog_counter = 0;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::CY, true);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, 0x0020);
        assert_eq!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
    }
    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn call_po() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;
        let orig_stack_add = machine_state.processor_state.stack_pointer;
        // 0b11_100_100 is cpo
        test_rom[0] = 0b11_100_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::P, true);
        machine_state.iterate_processor_state();

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(machine_state.processor_state.prog_counter, 0x0020);
        assert_ne!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
        machine_state.processor_state.prog_counter = 0;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::P, false);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, 0x0020);
        assert_eq!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
    }
    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn call_pe() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;
        let orig_stack_add = machine_state.processor_state.stack_pointer;
        // 0b11_101_100 is cpo
        test_rom[0] = 0b11_101_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::P, false);
        machine_state.iterate_processor_state();

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(machine_state.processor_state.prog_counter, 0x0020);
        assert_ne!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
        machine_state.processor_state.prog_counter = 0;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::P, true);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, 0x0020);
        assert_eq!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
    }
    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn call_p() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;
        let orig_stack_add = machine_state.processor_state.stack_pointer;
        // 0b11_110_100 is cp
        test_rom[0] = 0b11_110_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::S, true);
        machine_state.iterate_processor_state();

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(machine_state.processor_state.prog_counter, 0x0020);
        assert_ne!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
        machine_state.processor_state.prog_counter = 0;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::S, false);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, 0x0020);
        assert_eq!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
    }
    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn call_m() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;
        let orig_stack_add = machine_state.processor_state.stack_pointer;
        // 0b11_111_100 is cp
        test_rom[0] = 0b11_111_100;
        let some_rando_address = 0x0020;
        let second_byte = (some_rando_address & 0x00ff) as u8;
        let third_byte = ((some_rando_address & 0xff00) >> 8) as u8;
        test_rom[1] = second_byte;
        test_rom[2] = third_byte;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::S, false);
        machine_state.iterate_processor_state();

        // We want to make sure that it isn't doing the thing that it shouldn't
        assert_ne!(machine_state.processor_state.prog_counter, 0x0020);
        assert_ne!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
        machine_state.processor_state.prog_counter = 0;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::S, true);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.prog_counter, 0x0020);
        assert_eq!(
            machine_state.processor_state.stack_pointer,
            orig_stack_add - 2
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ldax_bc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair b-c is 00
        test_rom[0] = 0b00_00_1010;
        let some_rando_address = 0x0020;
        test_rom[some_rando_address] = 0xff;
        machine_state.processor_state.reg_b = (some_rando_address >> 8) as u8;
        machine_state.processor_state.reg_c = some_rando_address as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0xff);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ldax_de() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair d-e is 01
        test_rom[0] = 0b00_01_1010;
        let some_rando_address = 0x0020;
        test_rom[some_rando_address] = 0xff;
        machine_state.processor_state.reg_d = (some_rando_address >> 8) as u8;
        machine_state.processor_state.reg_e = some_rando_address as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0xff);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn verify_getset_rp_bc() {
        let mut machine_state = MachineState::new();
        let some_address: u16 = 0xfffe;
        machine_state
            .processor_state
            .set_rp(some_address, 0b00.into());
        assert_eq!(
            machine_state.processor_state.get_rp(0b00.into()),
            some_address
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn verify_getset_rp_de() {
        let mut machine_state = MachineState::new();
        let some_address: u16 = 0xfffe;
        machine_state
            .processor_state
            .set_rp(some_address, 0b01.into());
        assert_eq!(
            machine_state.processor_state.get_rp(0b01.into()),
            some_address
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn verify_getset_rp_hl() {
        let mut machine_state = MachineState::new();
        let some_address: u16 = 0xfffe;
        machine_state
            .processor_state
            .set_rp(some_address, 0b10.into());
        assert_eq!(
            machine_state.processor_state.get_rp(0b10.into()),
            some_address
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn verify_getset_rp_sp() {
        let mut machine_state = MachineState::new();
        let some_address: u16 = 0xfffe;
        machine_state
            .processor_state
            .set_rp(some_address, 0b11.into());
        assert_eq!(
            machine_state.processor_state.get_rp(0b11.into()),
            some_address
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn verify_push() {
        let mut machine_state = MachineState::new();
        machine_state.processor_state.stack_pointer = 0x2400;
        machine_state
            .processor_state
            .push_address(&mut machine_state.mem_map, 0xfffe);
        let address_in_stack =
            ((machine_state.mem_map[machine_state.processor_state.stack_pointer + 1] as u16) << 8)
                | machine_state.mem_map[machine_state.processor_state.stack_pointer] as u16;
        assert_eq!(0xfffe, address_in_stack);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn verify_pop() {
        let mut machine_state = MachineState::new();
        machine_state.processor_state.stack_pointer = 0x2400;
        machine_state
            .processor_state
            .push_address(&mut machine_state.mem_map, 0xfffe);
        assert_eq!(
            0xfffe,
            machine_state
                .processor_state
                .pop_address(&mut machine_state.mem_map)
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mov_a_mem() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair d-e is 01
        test_rom[0] = 0b01_110_111;
        let some_rando_address = 0x0020;
        machine_state.processor_state.reg_a = 0xff;
        machine_state.processor_state.reg_h = (some_rando_address >> 8) as u8;
        machine_state.processor_state.reg_l = some_rando_address as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(
            machine_state.processor_state.reg_a,
            machine_state.mem_map[some_rando_address]
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mov_mem_a() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b01_110_111;
        let some_rando_address = 0x0020;
        machine_state.processor_state.reg_a = 0xff;
        machine_state.processor_state.reg_h = (some_rando_address >> 8) as u8;
        machine_state.processor_state.reg_l = some_rando_address as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.mem_map[some_rando_address], 0xff);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn mov_a_b() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        // register pair d-e is 01
        test_rom[0] = 0b01_111_000;
        machine_state.processor_state.reg_b = 0xff;
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0xff);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn ret_uncon() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;
        let original_stack_pointer = machine_state.processor_state.stack_pointer;
        test_rom[0] = 0b11_001_001;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .push_address(&mut machine_state.mem_map, 0x0020);
        assert_ne!(0x0020, machine_state.processor_state.prog_counter);
        assert_ne!(
            original_stack_pointer,
            machine_state.processor_state.stack_pointer
        );
        machine_state.iterate_processor_state();
        assert_eq!(0x0020, machine_state.processor_state.prog_counter);
        assert_eq!(
            original_stack_pointer,
            machine_state.processor_state.stack_pointer
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn rz() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;
        test_rom[0] = 0b11_001_000;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .push_address(&mut machine_state.mem_map, 0x0020);
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, false);
        machine_state.iterate_processor_state();
        assert_ne!(0x0020, machine_state.processor_state.prog_counter);
        machine_state.processor_state.prog_counter = 0;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, true);
        machine_state.iterate_processor_state();
        assert_eq!(0x0020, machine_state.processor_state.prog_counter);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn rst() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;

        // RST to location 001, address 1000;
        test_rom[0] = 0b11_001_111;
        machine_state.mem_map.rom = test_rom;
        assert_ne!(0b1000, machine_state.processor_state.prog_counter);
        machine_state.iterate_processor_state();
        assert_eq!(0b1000, machine_state.processor_state.prog_counter);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn pchl() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        // RST to location 001, address 1000;
        test_rom[0] = 0b11_101_001;
        machine_state.mem_map.rom = test_rom;
        machine_state
            .processor_state
            .set_rp(0x0020, RPairBitPattern::HL);
        assert_ne!(0x0020, machine_state.processor_state.prog_counter);
        machine_state.iterate_processor_state();
        assert_eq!(0x0020, machine_state.processor_state.prog_counter);
        assert_ne!(machine_state.processor_state.prog_counter, 0);
    }
    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn push_byte() {
        let mut machine_state = MachineState::new();
        machine_state.processor_state.stack_pointer = 0x2400;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, true);
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::CY, true);
        let orig_stack_pointer = machine_state.processor_state.stack_pointer;
        machine_state.processor_state.push_byte(
            &mut machine_state.mem_map,
            machine_state.processor_state.flags.bits,
        );
        let mut test_flags = ConditionFlags {
            bits: (0b0000_0000),
        };
        test_flags.set(ConditionFlags::Z, true);
        test_flags.set(ConditionFlags::CY, true);
        assert_eq!(
            machine_state.mem_map[machine_state.processor_state.stack_pointer],
            test_flags.bits
        );
        assert_ne!(
            machine_state.processor_state.stack_pointer,
            orig_stack_pointer
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn pop_byte() {
        let mut machine_state = MachineState::new();
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, true);
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::CY, true);
        machine_state.processor_state.stack_pointer = 0x2400;
        machine_state.processor_state.push_byte(
            &mut machine_state.mem_map,
            machine_state.processor_state.flags.bits,
        );
        let orig_stack_pointer = machine_state.processor_state.stack_pointer;
        let mut test_flags = ConditionFlags {
            bits: (0b0000_0000),
        };
        test_flags.set(ConditionFlags::Z, true);
        test_flags.set(ConditionFlags::CY, true);
        assert_eq!(
            machine_state
                .processor_state
                .pop_byte(&mut machine_state.mem_map),
            test_flags.bits
        );
        assert_ne!(
            machine_state.processor_state.stack_pointer,
            orig_stack_pointer
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn push_pop_op_bc() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;

        // Push opcode for bc
        test_rom[0] = 0b11_00_0101;
        // Pop opcode for bc
        test_rom[1] = 0b11_00_0001;
        let original_stack_pointer = machine_state.processor_state.stack_pointer;

        machine_state
            .processor_state
            .set_rp(0xfffe, RPairBitPattern::BC);

        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_ne!(machine_state.processor_state.prog_counter, 0);
        assert_ne!(
            original_stack_pointer,
            machine_state.processor_state.stack_pointer
        );
        machine_state.processor_state.reg_b = 0;
        machine_state.processor_state.reg_c = 0;
        machine_state.iterate_processor_state();
        assert_ne!(machine_state.processor_state.prog_counter, 1);
        assert_eq!(
            machine_state.processor_state.get_rp(RPairBitPattern::BC),
            0xfffe
        );
        assert_eq!(
            original_stack_pointer,
            machine_state.processor_state.stack_pointer
        );
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn push_pop_op_a_flags() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;

        // Push opcode for flags and acc
        test_rom[0] = 0b11_11_0101;
        // Pop opcode for flags and acc
        test_rom[1] = 0b11_11_0001;
        let original_stack_pointer = machine_state.processor_state.stack_pointer;

        machine_state.processor_state.reg_a = 0xfe;
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::Z, true);
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::CY, true);
        machine_state
            .processor_state
            .flags
            .set(ConditionFlags::S, true);

        let mut test_flags = ConditionFlags {
            bits: (0b0000_0000),
        };
        test_flags.set(ConditionFlags::Z, true);
        test_flags.set(ConditionFlags::CY, true);
        test_flags.set(ConditionFlags::S, true);
        machine_state.mem_map.rom = test_rom;
        machine_state.iterate_processor_state();
        assert_ne!(machine_state.processor_state.prog_counter, 0);
        assert_ne!(
            original_stack_pointer,
            machine_state.processor_state.stack_pointer
        );
        machine_state.processor_state.reg_a = 0;
        machine_state.processor_state.flags = ConditionFlags {
            bits: (0b0000_0000),
        };
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0xfe);
        assert_eq!(machine_state.processor_state.flags, test_flags);
        assert_eq!(
            original_stack_pointer,
            machine_state.processor_state.stack_pointer
        );
        assert_ne!(machine_state.processor_state.prog_counter, 1);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn xthl() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.processor_state.stack_pointer = 0x2400;

        test_rom[0] = 0b11_100_011;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_h = 0xde;
        machine_state.processor_state.reg_l = 0xad;
        machine_state
            .processor_state
            .push_address(&mut machine_state.mem_map, 0xbeef);

        assert_ne!(
            machine_state.processor_state.get_rp(RPairBitPattern::HL),
            0xbeef
        );
        machine_state.iterate_processor_state();
        assert_eq!(
            machine_state.processor_state.get_rp(RPairBitPattern::HL),
            0xbeef
        );
        assert_ne!(machine_state.processor_state.prog_counter, 0);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sphl() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b11_111_001;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_h = 0xde;
        machine_state.processor_state.reg_l = 0xad;

        assert_ne!(
            machine_state.processor_state.get_rp(RPairBitPattern::HL),
            machine_state.processor_state.stack_pointer
        );
        machine_state.iterate_processor_state();
        assert_eq!(
            machine_state.processor_state.get_rp(RPairBitPattern::HL),
            machine_state.processor_state.stack_pointer
        );
        assert_ne!(machine_state.processor_state.prog_counter, 0);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn lda() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b00_111_010;
        let some_address: u16 = 0x1337;

        test_rom[1] = some_address as u8;
        test_rom[2] = (some_address >> 8) as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.mem_map[some_address] = 0xfe;

        assert_ne!(machine_state.processor_state.reg_a, 0xfe);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_a, 0xfe);
        assert_ne!(machine_state.processor_state.prog_counter, 0);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn sta() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b00_110_010;
        let some_address: u16 = 0x1337;

        test_rom[1] = some_address as u8;
        test_rom[2] = (some_address >> 8) as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_a = 0xfe;

        assert_ne!(machine_state.mem_map[some_address], 0xfe);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.mem_map[some_address], 0xfe);
        assert_ne!(machine_state.processor_state.prog_counter, 0);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn lhld() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b00_101_010;
        let some_address: u16 = 0x1337;

        test_rom[1] = some_address as u8;
        test_rom[2] = (some_address >> 8) as u8;
        machine_state.mem_map.rom = test_rom;
        machine_state.mem_map[some_address] = 0xbe;
        machine_state.mem_map[some_address + 1] = 0xef;

        assert_ne!(machine_state.processor_state.reg_l, 0xbe);
        assert_ne!(machine_state.processor_state.reg_h, 0xef);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_l, 0xbe);
        assert_eq!(machine_state.processor_state.reg_h, 0xef);
        assert_ne!(machine_state.processor_state.prog_counter, 0);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn shld() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        test_rom[0] = 0b00_100_010;
        let some_address: u16 = 0x1337;
        test_rom[1] = some_address as u8;
        test_rom[2] = (some_address >> 8) as u8;
        machine_state.processor_state.reg_l = 0xbe;
        machine_state.processor_state.reg_h = 0xef;
        machine_state.mem_map.rom = test_rom;

        assert_ne!(machine_state.mem_map[some_address], 0xbe);
        assert_ne!(machine_state.mem_map[some_address + 1], 0xef);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.mem_map[some_address], 0xbe);
        assert_eq!(machine_state.mem_map[some_address + 1], 0xef);
        assert_ne!(machine_state.processor_state.prog_counter, 0);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn stax() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];

        // Testing rp BC
        test_rom[0] = 0b00_00_0010;
        let some_address: u16 = 0x1337;
        machine_state.processor_state.reg_c = some_address as u8;
        machine_state.processor_state.reg_b = (some_address >> 8) as u8;
        machine_state.processor_state.reg_a = 0xfe;

        machine_state.mem_map.rom = test_rom;

        assert_ne!(machine_state.mem_map[some_address], 0xfe);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.mem_map[some_address], 0xfe);
        assert_ne!(machine_state.processor_state.prog_counter, 0);
    }

    #[cfg_attr(target_arch = "wasm32", wasm_bindgen_test)]
    #[cfg_attr(target_arch = "x86_64", test)]
    fn xchg() {
        let mut machine_state = MachineState::new();
        let mut test_rom = [0 as u8; space_invaders_rom::SPACE_INVADERS_ROM.len()];
        machine_state.mem_map.rom = test_rom;

        test_rom[0] = 0b1110_1011;
        machine_state.mem_map.rom = test_rom;
        machine_state.processor_state.reg_d = 0xbe;
        machine_state.processor_state.reg_e = 0xef;

        assert_eq!(machine_state.processor_state.reg_h, 0x00);
        assert_eq!(machine_state.processor_state.reg_l, 0x00);
        assert_eq!(machine_state.processor_state.reg_d, 0xbe);
        assert_eq!(machine_state.processor_state.reg_e, 0xef);
        machine_state.iterate_processor_state();
        assert_eq!(machine_state.processor_state.reg_h, 0xbe);
        assert_eq!(machine_state.processor_state.reg_l, 0xef);
        assert_eq!(machine_state.processor_state.reg_d, 0x00);
        assert_eq!(machine_state.processor_state.reg_e, 0x00);
        assert_ne!(machine_state.processor_state.prog_counter, 0);
    }
}
