#![allow(arithmetic_overflow)]

use std::{ops::{RangeInclusive, ShlAssign, ShrAssign}, arch::asm, collections::VecDeque};

use eframe::emath::Numeric;

use self::{instructions::{InstructionInfo, Instruction}, register::{Register, esfr_registers, sfr_registers}, gpr::GPRData, memory::Memory, instruction_impl::LogicOp, peripherals::Peripheraldata};

pub mod instructions;
pub mod register;
pub mod gpr;
pub mod macros;
pub mod memory;
pub mod instruction_impl;

mod peripherals;

pub const SEGMENT_SIZE: usize = 0x10000; // 64Kb
pub const DATA_PAGE_SIZE: usize = 0x4000; // 4Kb

/**
 * C166 ISM symbols
 *  ← (opY) is MOVED into (opX)
    + (opX) is ADDED to (opY)
    - (opY) is SUBTRACTED from (opX)
    × (opX) is MULTIPLIED by (opY)
    ÷ (opX) is DIVIDED by (opY)
    ∧ (opX) is logically ANDed with (opY)
    ∨ (opX) is logically ORed with (opY)
    ⊕ (opX) is logically EXCLUSIVELY ORed with (opY)
    ⇔ (opX) is COMPARED against (opY)
    ¬ (opX) is logically COMPLEMENTED (NOT)
    mod (opX) is divided MODULO (opY)

    MM MM - 16 bit address or caddr
    Long address mode - 'mem'

 */

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PSWFlag(u8);

impl PSWFlag {
    pub fn e(&self) -> u8 { (self.0 >> 4) & 0b1 }
    pub fn z(&self) -> u8 { (self.0 >> 3) & 0b1 }
    pub fn v(&self) -> u8 { (self.0 >> 2) & 0b1 }
    pub fn c(&self) -> u8 { (self.0 >> 1) & 0b1 }
    pub fn n(&self) -> u8 { (self.0 >> 0) & 0b1 }

    fn set_e(&mut self, e: bool) { self.0 = (self.0 & !(0b1 << 4)) | ((e as u8) << 4) }
    fn set_z(&mut self, z: bool) { self.0 = (self.0 & !(0b1 << 3)) | ((z as u8) << 3) }
    fn set_v(&mut self, v: bool) { self.0 = (self.0 & !(0b1 << 2)) | ((v as u8) << 2) }
    fn set_c(&mut self, c: bool) { self.0 = (self.0 & !(0b1 << 1)) | ((c as u8) << 1) }
    fn set_n(&mut self, n: bool) { self.0 = (self.0 & !(0b1 << 0)) | ((n as u8) << 0) }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CallsEntry {
    pub csp: u8,
    pub ip: u16
}

///! Infineon C167 emulator
#[derive(Debug, Clone)]
pub struct C167 {
    ip: u16,
    mem: Memory,
    flash: Vec<u8>,
    sfr_registers: [Option<Register>; 256],
    esfr_registers: [Option<Register>; 256],
    pub extr_value: u8,
    pub extp_value: u8,
    pub exts_value: u8,
    pub extp_page: u16,
    pub exts_seg: u16,
    gprs: GPRData,
    ins_exec: usize,
    peripherals_active: bool,
    pub calls_tracker: VecDeque<CallsEntry>,
    peripherals: Peripheraldata
}

unsafe impl Send for C167{}
unsafe impl Sync for C167{}

impl C167 {

    pub fn new(flash: Vec<u8>, eeprom: Vec<u8>) -> Self {

        let mut mem = Memory::new(eeprom);
        let sfr_registers = sfr_registers(&mut mem);
        let esfr_registers = esfr_registers(&mut mem);
        let gprs = GPRData::new(&mut mem);

        let mut f = Self {
            mem,
            ip: 0,
            flash: vec![0; 0x80000],
            sfr_registers,
            esfr_registers,
            extr_value: 0,
            gprs,
            ins_exec: 0,
            extp_page: 0,
            exts_seg: 0,
            extp_value: 0,
            exts_value: 0,
            peripherals_active: false,
            calls_tracker: VecDeque::new(),
            peripherals: Peripheraldata::new()
        };
        
        f.flash.extend_from_slice(&flash);
        f
    }

    pub fn get_mem(&self) -> &Memory {
        &self.mem
    }

    // call when using bitoff to get the position in memory to work with
    fn get_word_offset(&self, bitoff: u8) -> usize {
        if bitoff <= 0x7F {
            // RAM bit word offset
            0xFD00 + (2*bitoff as usize)
        } else if bitoff >= 0xF0 {
            // GPR word offset
            self.gprs.get_context_pointer() as usize  + (2*(bitoff & 0x0F) as usize)
        } else {
            // (E)SFR word offset
            if self.extr_value > 0 {
                // ESFR
                0xF100 + (2*(bitoff & 0x7F) as usize)
            } else {
                // SFR
                0xFF00 + (2*(bitoff & 0x7F) as usize)
            }
        }
    }


    fn get_mem_address(&self, mm_mm: u16) -> usize {
        let offset = (mm_mm & 0x3FFF) as u32; // 14 bits
        if self.extp_value != 0 {
            // 10 bits from extp_page
            let p = (self.extp_page & 0b1111_1111_11) as u32;
            let addr = p << 14 | offset;
            return addr as usize;
        } else if self.exts_value != 0 {
            let seg = (self.exts_seg as usize) << 16;
            return mm_mm as usize | seg
        } else { // Normal
            let dpp_selection = mm_mm >> 14 & 0b11;
            // only 10 bits from DPP are used
            let dpp_value = (self.get_sfr_reg(dpp_selection as u8).get_value_word_no_tracking(&self.mem) & 0b1111_1111_11) as u32;  
            let addr = dpp_value << 14 | offset;
            return addr as usize;
        }
    }

    pub fn exec_step(&mut self) -> Option<InstructionInfo> {
        let ret = InstructionInfo::from_bytes(&self.flash, self.get_code_seg_pointer(), self.ip);
        if let Some((instruction, new_offset)) = ret.as_ref() {
            if self.get_code_seg_pointer() < 8 && !(0xF200..=0xFE00).contains(&self.get_instruction_pointer()) {
                panic!("Code seg pointer is < 8, but we are not referencing IRAM! CSP:{} IP:{:08X?}",  self.get_code_seg_pointer(), new_offset);
            }
            self.ip = *new_offset;
            self.process_instruction(instruction)
        }
        ret.map(|x| x.0)
    }

    pub fn get_psw_flags<'a> (&'a self) -> PSWFlag {
        let r = self.sfr_registers[0x88].as_ref().unwrap().get_raw_no_tracking(&self.mem)[1];
        PSWFlag(r)
    }

    fn set_psw_flags<'a> (&'a mut self, flag: PSWFlag) {
        let mut old= self.sfr_registers[0x88].as_ref().unwrap().get_raw_no_tracking(&self.mem);
        old[1] = flag.0;
        self.sfr_registers[0x88].as_ref().unwrap().set_value_raw(&mut self.mem, old);
    }

    pub fn abort(&self, ins: &InstructionInfo) -> ! {
        
        todo!("Instruction {:02X?} not implemented. IP is {:08X?}", ins, self.ip);
    }

    fn process_instruction(&mut self, ins: &InstructionInfo) {  
        let i = ins.instruction;  
        match i {
            Instruction::ADD => self.logic_op_word(ins, LogicOp::ADD),
            Instruction::ADDB => self.logic_op_byte(ins, LogicOp::ADD),
            Instruction::ADDC => self.logic_op_word(ins, LogicOp::ADDC),
            Instruction::ADDCB => self.logic_op_byte(ins, LogicOp::ADDC),
            Instruction::AND => self.logic_op_word(ins, LogicOp::AND),
            Instruction::ANDB => self.logic_op_byte(ins, LogicOp::AND),
            Instruction::ASHR => self.abort(ins),
            Instruction::BAND => self.abort(ins),
            Instruction::BCLR => self.bclr(ins),
            Instruction::BCMP => self.abort(ins),
            Instruction::BFLDL => self.bfldl(ins),
            Instruction::BFLDH => self.bfldh(ins),
            Instruction::BMOV => self.bmov(ins),
            Instruction::BMOVN => self.bmovn(ins),
            Instruction::BOR => self.abort(ins),
            Instruction::BSET => self.bset(ins),
            Instruction::BXOR => self.abort(ins),
            Instruction::CALLA => self.calla(ins),
            Instruction::CALLI => self.abort(ins),
            Instruction::CALLR => self.abort(ins),
            Instruction::CALLS => self.calls(ins),
            Instruction::CMP => self.logic_op_word(ins, LogicOp::CMP),
            Instruction::CMPB => self.logic_op_byte(ins, LogicOp::CMP),
            Instruction::CMPD1 => self.abort(ins),
            Instruction::CMPD2 => self.abort(ins),
            Instruction::CMPL1 => self.abort(ins),
            Instruction::CMPL2 => self.abort(ins),
            Instruction::CPL => self.abort(ins),
            Instruction::CPLB => self.abort(ins),
            Instruction::DISWDT => {println!("Watchdog disabled!")},
            Instruction::DIV => self.abort(ins),
            Instruction::DIVL => self.abort(ins),
            Instruction::DIVLU => self.abort(ins),
            Instruction::DIVU => self.abort(ins),
            Instruction::EINIT => {println!("End of CPU initialization!")},
            Instruction::EXTR => self.extr(ins),
            Instruction::EXTS_OR_P => self.exts_or_p(ins),
            Instruction::IDLE => {panic!("Idle mode entered!")},
            Instruction::JB => self.jb(ins),
            Instruction::JBC => self.abort(ins),
            Instruction::JMPA => self.jmpa(ins),
            Instruction::JMPI => self.jmpi(ins),
            Instruction::JMPR => self.jmpr(ins),
            Instruction::JMPS => self.jmps(ins),
            Instruction::JNB => self.jnb(ins),
            Instruction::JNBS => self.abort(ins),
            Instruction::MOV => self.mov(ins),
            Instruction::MOVBZ => self.movbz(ins),
            Instruction::MOVBS => self.movbs(ins),
            Instruction::MOVB => self.movb(ins),
            Instruction::MUL => self.abort(ins),
            Instruction::MULU => self.abort(ins),
            Instruction::NEG => self.neg(ins),
            Instruction::NEGB => self.abort(ins),
            Instruction::NOP => { unsafe { asm!("nop") } },
            Instruction::OR => self.logic_op_word(ins, LogicOp::OR),
            Instruction::ORB => self.logic_op_byte(ins, LogicOp::OR),
            Instruction::PCALL => self.abort(ins),
            Instruction::POP => self.pop(ins),
            Instruction::PRIOR => self.abort(ins),
            Instruction::PUSH => self.push(ins),
            Instruction::PWRDN => self.abort(ins),
            Instruction::RET => self.ret(ins),
            Instruction::RETI => self.abort(ins),
            Instruction::RETP => self.abort(ins),
            Instruction::RETS => self.rets(ins),
            Instruction::ROL => self.abort(ins),
            Instruction::ROR => self.abort(ins),
            Instruction::SCXT => self.abort(ins),
            Instruction::SHL => self.shl(ins),
            Instruction::SHR => self.abort(ins),
            Instruction::SRST => self.abort(ins),
            Instruction::SRVWDT => {
                println!("Watchdog serviced!");
                let mut wdt = self.read_mem_word_no_tracking(0xFEAE as usize);
                let mut wdtcon = self.read_mem_word_no_tracking(0xFFAE as usize);

                let wdrel = wdtcon >> 8;
                // Clear wdt
                wdt = (wdrel << 8) & 0xFF00;
                // Clear wdtr
                wdtcon &= !(0b1 << 1);
                self.write_mem_word_no_tracking(0xFEAE, wdt);
                self.write_mem_word_no_tracking(0xFFAE, wdtcon);

            },
            Instruction::SUBC => self.logic_op_word(ins, LogicOp::SUBC),
            Instruction::SUBCB => self.logic_op_byte(ins, LogicOp::SUBC),
            Instruction::SUB => self.logic_op_word(ins, LogicOp::SUB),
            Instruction::SUBB => self.logic_op_byte(ins, LogicOp::SUB),
            Instruction::TRAP => self.abort(ins),
            Instruction::XOR => self.logic_op_word(ins, LogicOp::XOR),
            Instruction::XORB => self.logic_op_byte(ins, LogicOp::XOR),
        }
        self.ins_exec += 1;
        self.gprs.set_context_pointer(self.sfr_registers[8].as_ref().unwrap().get_value_word_no_tracking(&self.mem));
        if self.extr_value > 0 && i != Instruction::EXTR {
            self.extr_value -= 1;
        }

        if self.extp_value > 0 && i != Instruction::EXTS_OR_P {
            self.extp_value -= 1;
        }

        if self.exts_value > 0 && i != Instruction::EXTS_OR_P {
            self.exts_value -= 1;
        }

        self.mem.raw_access(|f| {
            f[0xF3F8] = 0xFF;
            f[0xF3F9] = 0xFF;

            f[0xF5AE] = 0x0B;
            f[0xF5AF] = 0xB8;
            self.peripherals.update(f);
        });
    }

    pub fn get_gpr_data<'a>(&'a self) -> &'a GPRData {
        &self.gprs
    }

    pub fn get_sfr_reg<'a> (&'a self, reg_id: u8) -> &'a Register {
        self.sfr_registers[reg_id as usize].as_ref().unwrap()
    }

    pub fn get_sfr_reg_list(&self) -> &[Option<Register>; 256] {
        &self.sfr_registers
    }

    pub fn get_esfr_reg_list(&self) -> &[Option<Register>; 256] {
        &self.esfr_registers
    }

    fn get_stack_pointer(&self) -> u16 {
        self.sfr_registers[9].as_ref().unwrap().get_value_word_no_tracking(&self.mem)
    }

    pub fn get_code_seg_pointer(&self) -> u8 {
        self.sfr_registers[4].as_ref().unwrap().get_raw_no_tracking(&self.mem)[0]
    }

    fn set_code_seg_pointer(&mut self, v: u8) {
        let reg = self.get_register(0x04);
        reg.set_value_byte(&mut self.mem, v)
    }

    pub fn get_instruction_pointer(&self) -> u16 {
        self.ip
    }

    fn set_stack_pointer(&mut self, sp: u16) {
        self.sfr_registers[9].as_mut().unwrap().set_value_word(&mut self.mem, sp);
    }

    pub fn get_register(&self, reg_id: u8) -> Register {
        if reg_id >= 0xF0 {
            return self.gprs.get_reg_by_id(reg_id)
        }

        match self.extr_value {
            0 => self.sfr_registers[reg_id as usize].expect(&format!("SFR REG {} does not exist", reg_id)),
            _ => self.esfr_registers[reg_id as usize].expect(&format!("ESFR REG {} does not exist", reg_id)),
        }
    }

    fn read_mem_word(&mut self, addr: usize) -> u16 {
        if addr > 0x80000 {
            u16::from_le_bytes(self.flash[addr..addr+2].try_into().unwrap())
        } else {
            u16::from_le_bytes(self.mem.read(addr as u16, 2).try_into().unwrap())
        }
    }

    fn read_mem_word_no_tracking(&mut self, addr: usize) -> u16 {
        if addr > 0x80000 {
            u16::from_le_bytes(self.flash[addr..addr+2].try_into().unwrap())
        } else {
            u16::from_le_bytes(self.mem.get_contents_no_access_tracking()[addr..addr+2].try_into().unwrap())
        }
    }

    fn read_mem_byte(&mut self, addr: usize) -> u8 {
        if addr > 0x80000 {
            self.flash[addr]
        } else {
            self.mem.read(addr as u16, 1)[0]
        }
    }

    fn write_mem_word(&mut self, addr: usize, input: u16) {
        // TODO (Can we write to flash?)
        self.mem.write(addr as u16, &input.to_le_bytes());
    }

    fn write_mem_word_no_tracking(&mut self, addr: usize, input: u16) {
        // TODO (Can we write to flash?)
        self.mem.set_word_buffer_no_tracking(addr as u16, input.to_le_bytes());
    }

    fn write_mem_byte(&mut self, addr: usize, input: u8) {
        // TODO (Can we write to flash?)
        self.mem.write(addr as u16, &[input]);
    }

    pub fn get_mem_region_no_tracking(&self, addr: usize, size: usize) -> Vec<u8> {
        if addr >= 0x80000 {
            self.flash[addr..addr as usize].to_vec()
        } else {
            self.mem.raw[addr..addr+size].to_vec()
        }
    }
}