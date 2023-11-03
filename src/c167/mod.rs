#![allow(arithmetic_overflow)]

use std::{ops::{RangeInclusive, ShlAssign, ShrAssign}, arch::asm, collections::VecDeque};

use self::{instructions::{InstructionInfo, Instruction}, register::{Register, esfr_registers, sfr_registers}, gpr::GPRData};

pub mod instructions;
pub mod register;
pub mod gpr;

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
    mod (opX) is divided MODULO (opY)

    MM MM - 16 bit address or caddr
    Long address mode - 'mem'

 */

fn carry_u16(op1: u16, op2: u16) -> bool {
    let mask = 0xFFFF;
    let cmp = op1.wrapping_add(op2);
    op1 > (cmp & mask)
}

fn carry_u8(op1: u8, op2: u8) -> bool {
    let mask = 0xFF;
    let cmp = op1.wrapping_add(op2);
    op1 > (cmp & mask)
}

fn scarry_u16(op1: u16, op2: u16) -> bool {
    let res = op1.wrapping_sub(op2);

    let mut a = (op1 >> 15) & 1;
    let b = (op2 >> 15) & 1;
    let mut r = (res >> 15) & 1;

    r ^= a;
    a ^= b;
    a ^= 1;
    r &= a;
    return r != 0;
}

fn scarry_u8(op1: u8, op2: u8) -> bool {
    let res = op1.wrapping_sub(op2);

    let mut a = (op1 >> 7) & 1;
    let b = (op2 >> 7) & 1;
    let mut r = (res >> 7) & 1;

    r ^= a;
    a ^= b;
    a ^= 1;
    r &= a;
    return r != 0;
}

fn sborrow_u16(op1: u16, op2: u16) -> bool {
    let res = op1.wrapping_sub(op2);

    let mut a = (op1 >> 15) & 1;
    let b = (op2 >> 15) & 1;
    let mut r = (res >> 15) & 1;

    a ^= r;
    r ^= b;
    r ^= 1;
    a &= r;

    return a != 0;
}

fn sborrow_u8(op1: u8, op2: u8) -> bool {
    let res = op1.wrapping_sub(op2);

    let mut a = (op1 >> 7) & 1;
    let b = (op2 >> 7) & 1;
    let mut r = (res >> 7) & 1;

    a ^= r;
    r ^= b;
    r ^= 1;
    a &= r;

    return a != 0;
}

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
pub enum LogicalOperation {
    CMP,
    OR,
    AND,
    XOR,
    NOT,
    ADD,
    ADDC, // With carry
    SUB,
    SUBC // With carry
}

fn cpu_check_jump(jump_nibble: u8, psw: &PSWFlag) -> bool {
    match jump_nibble {
        0x00 => true, // cc_UC
        0x01 => (psw.z() | psw.e()) == 0, // cc_NET
        0x02 => psw.z() != 0, // cc_Z
        0x03 => psw.z() == 0, // cc_NZ
        0x04 => psw.v() != 0, // cc_V
        0x05 => psw.v() == 0, // cc_NV
        0x06 => psw.n() != 0, // cc_N
        0x07 => psw.n() == 0, // cc_NN
        0x08 => psw.c() != 0, // cc_C
        0x09 => psw.c() == 0, // cc_NC
        0x0A => (psw.z() | (psw.n() ^ psw.v())) == 0, // cc_SGT
        0x0B => (psw.z() | (psw.n() ^ psw.v())) != 0, // cc_SLE
        0x0C => (psw.n() ^ psw.v()) != 0, // cc_SLT
        0x0D => (psw.n() ^ psw.v()) == 0, // cc_SGE
        0x0E => (psw.z() | psw.c()) == 0, // cc_UGT
        0x0F => (psw.z() | psw.c()) != 0, // cc_ULE
        _ => panic!("Invalid jump type {:02X?}", jump_nibble)   
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Memory {
    raw: [u8; 0x10000],
    last_write_start: u16,
    last_write_end: u16,
    
    last_read_start: u16,
    last_read_end: u16,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            raw: [0; 0x10000],
            last_write_start: 0,
            last_write_end: 0,
            last_read_start: 0,
            last_read_end: 0,
        }
    }

    pub fn write(&mut self, addr: u16, contents: &[u8]) {
        let len = contents.len();
        self.last_write_start = addr;
        self.last_write_end = addr + len as u16;
        self.raw[addr as usize..addr as usize + len].copy_from_slice(contents);
    }

    pub fn reset_region(&mut self, addr: u16, contents: &[u8]) {
        let len = contents.len();
        self.raw[addr as usize..addr as usize + len].copy_from_slice(contents);
    }

    pub fn read(&mut self, addr: u16, size: u16) -> &[u8] {
        self.last_read_start = addr;
        self.last_read_end = addr + size;
        &self.raw[addr as usize..(addr + size) as usize]
    }

    pub fn get_contents_no_access_tracking(&self) -> &[u8] {
        &self.raw
    }

    pub fn get_last_write(&self) -> RangeInclusive<u16> {
        self.last_write_start..=self.last_write_end
    }

    pub fn get_last_read(&self) -> RangeInclusive<u16> {
        self.last_read_start..=self.last_read_end
    }


    pub fn set_byte(&mut self, addr: u16, v: u8) {
        self.last_write_start = addr;
        self.last_write_end = addr;
        self.raw[addr as usize] = v;
    }

    pub fn set_word_buffer(&mut self, addr: u16, v: [u8; 2]) {
        self.last_write_start = addr;
        self.last_write_end = addr+1;
        self.raw[addr as usize..addr as usize + 2].copy_from_slice(&v);
    }

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
    pub extp_page: u32,
    gprs: GPRData,
    ins_exec: usize,
    peripherals_active: bool,
    pub calls_tracker: VecDeque<CallsEntry>
}

unsafe impl Send for C167{}
unsafe impl Sync for C167{}

impl C167 {

    pub fn new(flash: Vec<u8>) -> Self {

        let mut mem = Memory::new();
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
            extp_value: 0,
            peripherals_active: false,
            calls_tracker: VecDeque::new()
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
        // Can only be 0-3
        if self.extp_value == 0 {
            let dpp_selection = (mm_mm & 0b1100_0000_0000_0000) >> 14;
            let output_addr = (((self.get_sfr_reg(dpp_selection as u8).get_value_word_no_tracking(&self.mem) as usize) & 0x3FFF) << 14) | (mm_mm & 0x3FFF) as usize;
            return output_addr as usize & 0xFFFFFF
        } else {
            let addr = self.extp_page | (mm_mm as u32 & 0x3FFF);
            return addr as usize & 0xFFFFFF;
        }
    }

    pub fn exec_step(&mut self) -> Option<InstructionInfo> {
        let ret = InstructionInfo::from_bytes(&self.flash, self.get_code_seg_pointer(), self.ip);
        if let Some((instruction, new_offset)) = ret.as_ref() {
            if self.get_code_seg_pointer() < 8 {
                panic!("We should not be executing RAM! CSP:{} IP:{:08X?}",  self.get_code_seg_pointer(), new_offset);
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

    fn test_update_peripherals(&mut self) {
        {
            // T1 update code (Just counting)
            let mut v = self.get_register(41).get_value_word(&mut self.mem);
            v += 1;
            self.get_register(41).set_value_word(&mut self.mem, v);
        }
    }

    fn process_instruction(&mut self, ins: &InstructionInfo) {  
        let i = ins.instruction;  
        match i {
            Instruction::ADD => self.logical_op_word(ins, LogicalOperation::ADD),
            Instruction::ADDB => self.logical_op_byte(ins, LogicalOperation::ADD),
            Instruction::ADDC => self.logical_op_word(ins, LogicalOperation::ADDC),
            Instruction::ADDCB => self.logical_op_byte(ins, LogicalOperation::ADDC),
            Instruction::AND => self.logical_op_word(ins, LogicalOperation::AND),
            Instruction::ANDB => self.logical_op_byte(ins, LogicalOperation::AND),
            Instruction::ASHR => self.ashr(ins),
            Instruction::BAND => self.abort(ins),
            Instruction::BCLR => self.bclr(ins),
            Instruction::BCMP => self.abort(ins),
            Instruction::BFLDL => self.bfldl(ins),
            Instruction::BFLDH => self.bfldh(ins),
            Instruction::BMOV => self.bmov(ins, false),
            Instruction::BMOVN => self.bmov(ins, true),
            Instruction::BOR => self.abort(ins),
            Instruction::BSET => self.bset(ins),
            Instruction::BXOR => self.abort(ins),
            Instruction::CALLA => self.calla(ins),
            Instruction::CALLI => self.abort(ins),
            Instruction::CALLR => self.abort(ins),
            Instruction::CALLS => self.calls(ins),
            Instruction::CMP => self.logical_op_word(ins, LogicalOperation::CMP),
            Instruction::CMPB => self.logical_op_byte(ins, LogicalOperation::CMP),
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
            Instruction::EXTS_OR_P => self.extp(ins),
            Instruction::IDLE => {panic!("Idle mode entered!")},
            Instruction::JB => self.jb(ins),
            Instruction::JBC => self.abort(ins),
            Instruction::JMPA => self.jumpa(ins),
            Instruction::JMPI => self.jumpi(ins),
            Instruction::JMPR => self.jumpr(ins),
            Instruction::JMPS => self.jumps(ins),
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
            Instruction::OR => self.logical_op_word(ins, LogicalOperation::OR),
            Instruction::ORB => self.logical_op_byte(ins, LogicalOperation::OR),
            Instruction::PCALL => self.abort(ins),
            Instruction::POP => self.pop(ins),
            Instruction::PRIOR => self.abort(ins),
            Instruction::PUSH => self.push(ins),
            Instruction::PWRDN => self.abort(ins),
            Instruction::RET => self.ret(ins),
            Instruction::RETI => self.abort(ins),
            Instruction::RETP => self.abort(ins),
            Instruction::RETS => self.rets(ins),
            Instruction::ROL => self.rol(ins),
            Instruction::ROR => self.ror(ins),
            Instruction::SCXT => self.abort(ins),
            Instruction::SHL => self.shl(ins),
            Instruction::SHR => self.shr(ins),
            Instruction::SRST => self.abort(ins),
            Instruction::SRVWDT => {
                println!("Watchdog init!");
                self.peripherals_active = true;
            },
            Instruction::SUBC => self.logical_op_word(ins, LogicalOperation::SUBC),
            Instruction::SUBCB => self.logical_op_byte(ins, LogicalOperation::SUBC),
            Instruction::SUB => self.logical_op_word(ins, LogicalOperation::SUB),
            Instruction::SUBB => self.logical_op_byte(ins, LogicalOperation::SUB),
            Instruction::TRAP => self.abort(ins),
            Instruction::XOR => self.logical_op_word(ins, LogicalOperation::XOR),
            Instruction::XORB => self.logical_op_byte(ins, LogicalOperation::XOR),
        }
        self.ins_exec += 1;
        self.gprs.set_context_pointer(self.sfr_registers[8].as_ref().unwrap().get_value_word_no_tracking(&self.mem));
        if self.extr_value > 0 && i != Instruction::EXTR {
            self.extr_value -= 1;
        }

        if self.extp_value > 0 && i != Instruction::EXTS_OR_P {
            self.extp_value -= 1;
        }

        if self.peripherals_active {
            self.test_update_peripherals();
        }
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

    fn bmov(&mut self, ins: &InstructionInfo, negate: bool) {
        // QQ ZZ qz
        let src_byte_addr = self.get_word_offset(ins.bytes[1]);
        let dest_byte_addr = self.get_word_offset(ins.bytes[2]);

        let bitoffset_dest = ins.bytes[3] & 0x0F;
        let bitoffset_src = (ins.bytes[3] & 0xF0) >> 4;

        let mut dest = self.read_mem_byte(dest_byte_addr);
        let mut src = self.read_mem_byte(src_byte_addr);
        if negate {
            src = !src;
        }

        let src_bit = src >> bitoffset_src & 0b1;

        dest = dest & !(0b1 << bitoffset_dest); // clear old bit
        dest = dest | (src_bit << bitoffset_dest); // Set new bit

        self.write_mem_byte(dest_byte_addr, dest);

        
    }

    fn calla(&mut self, ins: &InstructionInfo) {
        // c0 mm mm
        if cpu_check_jump((ins.bytes[1] & 0xF0) >> 4, &self.get_psw_flags()) {

            let calls_data = CallsEntry {
                csp: self.get_code_seg_pointer(),
                ip: self.ip,
            };
            self.calls_tracker.push_back(calls_data);

            let mut sp = self.get_stack_pointer();
            sp -= 2; 
            self.set_stack_pointer(sp); // (SP) ← (SP) - 2
            self.write_mem_word(self.get_mem_address(sp), self.ip); // ((SP)) ← (IP)
            self.ip = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
            self.set_stack_pointer(sp); // (IP) ← op2
        }
    }

    fn calls(&mut self, ins: &InstructionInfo) {
        // Seg Caddr
        let seg = ins.bytes[1];
        let caddr = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
        println!("Branching to segment {}, caddr {:08X?}", seg, caddr);

        let sp = self.get_stack_pointer();
        let csp = self.get_code_seg_pointer();

        let calls_old_entry = CallsEntry {
            csp: self.get_code_seg_pointer(),
            ip: self.get_instruction_pointer()
        };
        self.calls_tracker.push_back(calls_old_entry);

        self.set_stack_pointer(sp - 2); // (Sp) <- (Sp) - 2
        self.write_mem_byte(self.get_mem_address(self.get_stack_pointer()), csp); // ((Sp)) <- (Csp)
        let sp = self.get_stack_pointer();
        self.set_stack_pointer(sp - 2); // (Sp) <- (Sp) - 2
        self.write_mem_word(self.get_mem_address(self.get_stack_pointer()), self.get_instruction_pointer()); // ((SP)) ← (IP)
        self.set_code_seg_pointer(seg); // (CSP) ← op1
        self.ip = caddr; // (IP) ← op2
    }

    fn extr(&mut self, ins: &InstructionInfo) {
        let mut irange = (ins.bytes[1] & 0b0011_0000) >> 4;
        irange += 1;
        self.extr_value = irange;
    }

    fn neg(&mut self, ins: &InstructionInfo) {
        match ins.bytes[0] {
            // Rwn Rwm
            0x81 => {
                let op1 = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_word(&mut self.mem);
                let mut psw = self.get_psw_flags();
                let res = 0u16.wrapping_sub(op1);
                psw.set_e(op1 != 0x8000);
                psw.set_z(res == 0);
                psw.set_v(sborrow_u16(0, op1));
                psw.set_c(op1 != 0); // anything other than 0 would cause underflow
                psw.set_n(0 > res as i16);
                self.set_psw_flags(psw);
                self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).set_value_word(&mut self.mem, res);
            },
            _ => todo!("Neg not implemented for bytes {:02X?}", ins.bytes)
        }
    }

    fn mov(&mut self, ins: &InstructionInfo) {
        let (dest_addr, to_move) = match ins.bytes[0] {
            // Rwn <- Rwm 
            0xF0 => {
                let addr_reg_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_addr();
                let value_reg_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                (addr_reg_n as usize, value_reg_m)
            }
            // Rwn <- #data4
            0xE0 => {
                let addr_reg_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_addr();
                let data_4 = (ins.bytes[1] & 0x0F) as u16;
                (addr_reg_n as usize, data_4)
            }
            // REG <- #data16
            0xE6 => {
                let addr_reg = self.get_register(ins.bytes[1]).get_addr();
                let data_16 = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                (addr_reg as usize, data_16)
            }
            // Rwn <- [Rwm]
            0xA8 => {
                let addr_reg_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_addr();
                let value_reg_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                println!("{:08X?} {:08X?}", value_reg_m, self.get_mem_address(value_reg_m));
                let value = self.read_mem_word(self.get_mem_address(value_reg_m));
                (addr_reg_n as usize, value)
            }
            // Rwn <- [Rwm+]
            0x98 => {
                let addr_reg_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_addr();
                let mut value_reg_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let value = self.read_mem_word(self.get_mem_address(value_reg_m));
                value_reg_m += 1; // post increase reg addr
                self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).set_value_word(&mut self.mem, value_reg_m);
                (addr_reg_n as usize, value)
            }
            // [Rwm] <- Rwn
            0xB8 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let reg_value_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_word(&mut self.mem);
                (self.get_mem_address(addr_ptr_m), reg_value_n)
            }
            // [-Rwm] <- Rwn
            0x88 => {
                let reg_value_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_word(&mut self.mem);
                let mut addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                addr_ptr_m -= 1;
                self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).set_value_word(&mut self.mem, addr_ptr_m);
                (self.get_mem_address(addr_ptr_m), reg_value_n)
            }
            // [Rwn] <- [Rwm]
            0xC8 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let addr_ptr_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_word(&mut self.mem);
                let value_at_m = self.read_mem_word(self.get_mem_address(addr_ptr_m));
                (self.get_mem_address(addr_ptr_n), value_at_m)
            }
            // [Rwn+] <- [Rwm]
            0xD8 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let addr_ptr_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_word(&mut self.mem);
                // Increase pointer after access
                self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).set_value_word(&mut self.mem, addr_ptr_n+1);
                let value_at_m = self.read_mem_word(self.get_mem_address(addr_ptr_m));
                (self.get_mem_address(addr_ptr_n), value_at_m)
            }
            // [Rwn] <- [Rwm+]
            0xE8 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let addr_ptr_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_word(&mut self.mem);
                let value_at_m = self.read_mem_word(self.get_mem_address(addr_ptr_m));
                // Increase pointer after access
                self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).set_value_word(&mut self.mem, addr_ptr_m+1);
                (self.get_mem_address(addr_ptr_n), value_at_m)
            }
            // Rwn <- [Rwm + data16]
            0xD4 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let addr_reg_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_addr();
                let inc = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                let value_at_m = self.read_mem_word(self.get_mem_address(addr_ptr_m));
                // Increase pointer after access
                self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).set_value_word(&mut self.mem, addr_ptr_m+inc);
                (addr_reg_n as usize, value_at_m)
            }
            // [Rwm + data16] <- Rwn
            0xC4 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let value_at_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_word(&mut self.mem);
                let inc = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                // Increase pointer after access
                self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).set_value_word(&mut self.mem, addr_ptr_m+inc);
                (self.get_mem_address(addr_ptr_m), value_at_n)
            }
            // [Rwn] <- mem
            0x84 => {
                let mem_address = self.get_mem_address(u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()));
                let value_in_mem = self.read_mem_word(mem_address);

                let addr_ptr_n = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);

                (self.get_mem_address(addr_ptr_n), value_in_mem)
            }
            0x94 => { panic!("TODO mem <- [Rwn]") }
            // REG <- mem
            0xF2 => {
                let mem_address = self.get_mem_address(u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()));
                let value_in_mem = self.read_mem_word(mem_address);

                let addr_reg = self.get_register(ins.bytes[1]).get_addr();
                (addr_reg as usize, value_in_mem)
            }
            // mem <- REG
            0xF6 => { 
                let addr = self.get_mem_address(u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()));
                let value = self.get_register(ins.bytes[1]).get_value_word(&mut self.mem);
                (addr, value)
             }

            _ => panic!("Invalid operand for MOV {:02X?}", ins.bytes)
        };
        let mut psw = self.get_psw_flags();
        psw.set_e(to_move == 0x8000);
        psw.set_z(to_move == 0);
        psw.set_n(0 > to_move as i16);
        self.set_psw_flags(psw);
        self.write_mem_word(dest_addr, to_move);
    }

    fn movb(&mut self, ins: &InstructionInfo) {
        let (dest_addr, to_move) = match ins.bytes[0] {
            // Rwn <- Rwm 
            0xF1 => {
                let addr_reg_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_addr();
                let value_reg_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_byte(&mut self.mem);
                (addr_reg_n as usize, value_reg_m)
            }
            // Rwn <- #data4
            0xE1 => {
                let addr_reg_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_addr();
                let data_4 = (ins.bytes[1] & 0x0F) as u8;
                (addr_reg_n as usize, data_4)
            }
            // REG <- #data8
            0xE7 => {
                let addr_reg = self.get_register(ins.bytes[1]).get_addr();
                let data_8 = ins.bytes[1];
                (addr_reg as usize, data_8)
            }
            // Rwn <- [Rwm]
            0xA9 => {
                let addr_reg_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_addr();
                let value_reg_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let value = self.read_mem_byte(self.get_mem_address(value_reg_m));
                (addr_reg_n as usize, value)
            }
            // Rwn <- [Rwm+]
            0x99 => {
                let addr_reg_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_addr();
                let mut value_reg_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let value = self.read_mem_byte(self.get_mem_address(value_reg_m));
                value_reg_m += 1; // post increase reg addr
                self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).set_value_word(&mut self.mem, value_reg_m);
                (addr_reg_n as usize, value)
            }
            // [Rwm] <- Rwn
            0xB9 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let reg_value_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_byte(&mut self.mem);
                (self.get_mem_address(addr_ptr_m), reg_value_n)
            }
            // [-Rwm] <- Rwn
            0x89 => {
                let reg_value_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_byte(&mut self.mem);
                let mut addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                addr_ptr_m -= 1;
                self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).set_value_word(&mut self.mem, addr_ptr_m);
                (self.get_mem_address(addr_ptr_m), reg_value_n)
            }
            // [Rwn] <- [Rwm]
            0xC9 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let addr_ptr_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_word(&mut self.mem);
                let value_at_m = self.read_mem_byte(self.get_mem_address(addr_ptr_m));
                (self.get_mem_address(addr_ptr_n), value_at_m)
            }
            // [Rwn+] <- [Rwm]
            0xD9 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let addr_ptr_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_word(&mut self.mem);
                // Increase pointer after access
                self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).set_value_word(&mut self.mem, addr_ptr_n+1);
                let value_at_m = self.read_mem_byte(self.get_mem_address(addr_ptr_m));
                (self.get_mem_address(addr_ptr_n), value_at_m)
            }
            // [Rwn] <- [Rwm+]
            0xE9 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let addr_ptr_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_word(&mut self.mem);
                let value_at_m = self.read_mem_byte(self.get_mem_address(addr_ptr_m));
                // Increase pointer after access
                self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).set_value_word(&mut self.mem, addr_ptr_m+1);
                (self.get_mem_address(addr_ptr_n), value_at_m)
            }
            // Rwn <- [Rwm + data16]
            0xF4 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let addr_reg_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_addr();
                let inc = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                let value_at_m = self.read_mem_byte(self.get_mem_address(addr_ptr_m));
                // Increase pointer after access
                self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).set_value_word(&mut self.mem, addr_ptr_m+inc);
                (addr_reg_n as usize, value_at_m)
            }
            // [Rwm + data16] <- Rwn
            0xE4 => {
                let addr_ptr_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
                let value_at_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_byte(&mut self.mem);
                let inc = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                // Increase pointer after access
                self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).set_value_word(&mut self.mem, addr_ptr_m+inc);
                (self.get_mem_address(addr_ptr_m), value_at_n)
            }
            // [Rwn] <- mem
            0xA4 => {
                let mem_address = self.get_mem_address(u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()));
                let value_in_mem = self.read_mem_byte(mem_address);

                let addr_ptr_n = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);

                (self.get_mem_address(addr_ptr_n), value_in_mem)
            }
            0xB4 => { panic!("TODO mem <- [Rwn]") }
            // REG <- mem
            0xF3 => {
                let mem_address = self.get_mem_address(u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()));
                let value_in_mem = self.read_mem_byte(mem_address);

                let addr_reg = self.get_register(ins.bytes[1]).get_addr();
                (addr_reg as usize, value_in_mem)
            }
            // mem <- REG
            0xF7 => { 
                let addr = self.get_mem_address(u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()));
                let value = self.get_register(ins.bytes[1]).get_byte(&mut self.mem);
                (addr, value)
             }

            _ => panic!("Invalid operand for MOV {:02X?}", ins.bytes)
        };
        let mut psw = self.get_psw_flags();
        psw.set_e(to_move == 0x80);
        psw.set_z(to_move == 0);
        psw.set_n(0 > to_move as i8);
        self.set_psw_flags(psw);
        self.write_mem_byte(dest_addr, to_move);
    }

    fn movbs(&mut self, ins: &InstructionInfo) {
        let (mut op1, dest_op1, op2) = match ins.bytes[0] {
            // Rwn <- Rbm
            0xD0 => {
                let reg_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4); // Dest
                let reg_n_v = reg_n.get_value_word(&mut self.mem);
                let reg_n_a = reg_n.get_addr() as usize;

                let reg_m_value = self.gprs.byte_reg_by_idx(ins.bytes[1] & 0x0F).get_byte(&mut self.mem); // Src
                (reg_n_v, reg_n_a as usize, reg_m_value)
            },
            // reg <- mem
            0xD2 => {
                let mm_mm = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                let addr = self.get_mem_address(mm_mm);
                let v_at_addr = self.read_mem_byte(addr);
                let reg = self.get_register(ins.bytes[1]);
                (reg.get_value_word(&mut self.mem), reg.get_addr() as usize, v_at_addr)

            }
            // mem <- reg
            0xD5 => {
                let mm_mm = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                let addr: usize = self.get_mem_address(mm_mm);
                let v_at_addr = self.read_mem_word(addr);
                let reg = self.get_register(ins.bytes[1]);
                (v_at_addr, addr, reg.get_byte(&mut self.mem))
            },
            
            _ => panic!("Invalid operand for MOVB {:02X?}", ins.bytes)
        };
        op1 = (op1 & 0xFF) | (op2 as u16);
        if (op2 >> 7) & 0b1 != 0 {
            op1 = (op1 & 0xFF00) | (op2 as u16) | 0xFF00;
        } else {
            op1 = (op1 & 0x0000) | (op2 as u16);
        }
        self.write_mem_word(dest_op1, op1);
        // Update PSW
        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(op2 == 0);
        psw.set_n(0 > op2 as i8);
        self.set_psw_flags(psw);

    }

    fn movbz(&mut self, ins: &InstructionInfo) {
        let (addr_op1, mut op1, op2) = match ins.bytes[0] {
            // Rwn <- Rwm
            0xC0 => {
                let reg_m_value = self.gprs.byte_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_byte(&mut self.mem);
                let reg_n = self.gprs.byte_reg_by_idx(ins.bytes[1] & 0x0F);
                (reg_n.get_addr() as usize, reg_n.get_raw(&mut self.mem), reg_m_value)
            },
            // Reg <- mem
            0xC2 => {
                // Reg is dest
                let addr_mem = self.get_mem_address(u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()));
                let mem_value = self.read_mem_byte(addr_mem);
                let reg = self.get_register(ins.bytes[1]);
                let op1 = reg.get_raw(&mut self.mem);
                (reg.get_addr() as usize, op1, mem_value)
            },
            // Reg -> mem
            0xC5 => {
                // Mem is dest
                let addr_mem = self.get_mem_address(u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()));
                let mem_value = self.read_mem_raw(addr_mem);
                let reg = self.get_register(ins.bytes[1]);
                let reg_value = reg.get_byte(&mut self.mem);
                (addr_mem, mem_value, reg_value)
            }
            _ => panic!("Invalid operand for MOVZ {:02X?}", ins.bytes)
        };

        op1[0] = op2;
        op1[1] = 0x00;

        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(op2 == 0);
        psw.set_v(false);
        psw.set_c(false);
        psw.set_n(false);
        self.set_psw_flags(psw);
        self.write_mem_raw(addr_op1, op1);

    }

    /**
     * Bitaddr: Word address withinin
     * bitoff: 
     */

    fn bclr(&mut self, ins: &InstructionInfo) {
        let bit = (ins.bytes[0] & 0xF0) >> 4;
        let addr = self.get_word_offset(ins.bytes[1]);
        let mut tmp = self.read_mem_word(addr);
        let old_bit = tmp >> bit & 0b1;
        tmp = tmp & !(1 << bit);
        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(!old_bit == 0);
        psw.set_v(false);
        psw.set_c(false);
        psw.set_n(old_bit == 0);
        self.set_psw_flags(psw);
        self.write_mem_word(addr, tmp);
    }

    fn bset(&mut self, ins: &InstructionInfo) {
        let addr = self.get_word_offset(ins.bytes[1]);
        let mut tmp = self.read_mem_word(addr);
        let bit = ins.bytes[0] & 0xF0;

        let old_bit = tmp >> bit & 0b1;

        tmp |= 0b1 << bit;

        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(!old_bit == 0);
        psw.set_v(false);
        psw.set_c(false);
        psw.set_n(old_bit == 0);
        self.set_psw_flags(psw);
        self.write_mem_word(addr, tmp);
    }

    fn bfldh(&mut self, ins: &InstructionInfo) {
        // ^ AND
        // v OR
        // -| - logically complemented
        let b_addr = self.get_word_offset(ins.bytes[1]) + 1; // High byte
        let mut tmp = self.read_mem_byte(b_addr);
        tmp = (tmp & (0xFF | ins.bytes[2])) | ins.bytes[3];
        self.write_mem_byte(b_addr, tmp);

        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(tmp == 0);
        psw.set_v(false);
        psw.set_c(false);
        psw.set_n(0 > tmp as i8);
        self.set_psw_flags(psw);
    }   

    fn bfldl(&mut self, ins: &InstructionInfo) {
        // ^ AND
        // v OR
        // -| - logically complemented
        let b_addr = self.get_word_offset(ins.bytes[1]); // Low byte
        let mut tmp = self.read_mem_byte(b_addr);
        tmp = (tmp & (0xFF | ins.bytes[2])) | ins.bytes[3];
        self.write_mem_byte(b_addr, tmp);

        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(tmp == 0);
        psw.set_v(false);
        psw.set_c(false);
        psw.set_n(0 > tmp as i8);
        self.set_psw_flags(psw);
    }

    fn extp(&mut self, ins: &InstructionInfo) {
        match ins.bytes[0] {
            // Rwm #irange2
            0xDC => {
                let mut irange = (ins.bytes[1] & 0b0011_0000) >> 4;
                irange += 1;
                let reg_v = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_raw(&mut self.mem);
                let page_bytes = u16::from_le_bytes(reg_v[0..2].try_into().unwrap()) & 0b0011_1111;
                let page = (page_bytes as u32) << 14;
                self.extp_value = irange;
                self.extp_page = page;
                println!("{} {:08X?}", irange, page);
            },
            // #Pag #irange2
            0xD7 => {
                let mut irange = (ins.bytes[1] & 0b0011_0000) >> 4;
                irange += 1;
                let page_bytes = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()) & 0b0011_1111;
                let page = (page_bytes as u32) << 14;
                self.extp_value = irange;
                self.extp_page = page;
            },
            _ => panic!("Invalid EXTP instruction {:02X?}", ins.bytes[0])
        }
    }

    fn jb(&mut self, ins: &InstructionInfo) {
        // QQ rr q0
        let value = self.read_mem_byte(self.get_word_offset(ins.bytes[1]));
        let bit = (ins.bytes[3] & 0xF0) >> 4;
        let relative_offset = (ins.bytes[2] as i8) * 2;

        if value & 1<< bit != 0 {
            // Set. Jump
            self.ip = self.ip.wrapping_add_signed(relative_offset as i16);
        }
    }

    fn jnb(&mut self, ins: &InstructionInfo) {
        // QQ rr q0
        let value = self.read_mem_byte(self.get_word_offset(ins.bytes[1]));
        let bit = (ins.bytes[3] & 0xF0) >> 4;
        let relative_offset = (ins.bytes[2] as i8) * 2;

        if value & 1<< bit == 0 {
            // Clear. Jump
            self.ip = self.ip.wrapping_add_signed(relative_offset as i16);
        }
    }

    fn jumpa(&mut self, ins: &InstructionInfo) {
        // CC, CADDR
        let caddr = u16::from_le_bytes(ins.bytes[2..4].try_into().unwrap());
        if cpu_check_jump((ins.bytes[1] & 0xF0) >> 4, &self.get_psw_flags()) {
            self.ip = caddr;
        }
    }

    fn jumps(&mut self, ins: &InstructionInfo) {
        // SEG, CADDR
        let seg = ins.bytes[1];
        let caddr = u16::from_le_bytes(ins.bytes[2..4].try_into().unwrap());
        self.set_code_seg_pointer(seg);
        self.ip = caddr;
    }

    fn jumpi(&mut self, ins: &InstructionInfo) {  
        let reg_value = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem);
        if cpu_check_jump((ins.bytes[1] & 0xF0) >> 4, &self.get_psw_flags()) {
            self.ip = reg_value;
        }
    }

    fn jumpr(&mut self, ins: &InstructionInfo) {
        let offset = (ins.bytes[1] as i8) as i16 * 2;
        if cpu_check_jump((ins.bytes[0] & 0xF0) >> 4, &self.get_psw_flags()) {
            self.ip = self.ip.wrapping_add_signed(offset as i16);
        }
    }

    fn logical_op_word(&mut self, ins: &InstructionInfo, logic_op: LogicalOperation) {
        let ((op1, op1_addr), op2) = match ins.bytes[0] & 0x0F {
            // Rwn Rwm
            0x00 => {
                let dest_reg = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4);
                (
                    (dest_reg.get_value_word(&mut self.mem), dest_reg.get_addr()),
                    self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_word(&mut self.mem),
                )
            },
            // RR MM MM
            0x02 => {
                let mm_mm = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                let addr = self.get_mem_address(mm_mm);
                let dest_reg = self.get_register(ins.bytes[1]);
                (
                    (dest_reg.get_value_word(&mut self.mem), dest_reg.get_addr()),
                    self.read_mem_word(addr),
                )
            },
            // RR MM MM (Reversed OR comparison to 0x72)
            0x04 => {
                let addr = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                let mem_addr = self.get_mem_address(addr);
                let mem_value = self.read_mem_word(mem_addr);
                (
                    (mem_value, addr as u16),
                    self.get_register(ins.bytes[1]).get_value_word(&mut self.mem)
                )
            },
            // RR DATA16
            0x06 => {
                let dest_reg = self.get_register(ins.bytes[1]);
                (
                    (dest_reg.get_value_word(&mut self.mem), dest_reg.get_addr()),
                    u16::from_le_bytes(ins.bytes[2..].try_into().unwrap())
                )
            },
            0x08 => {
                let h = (ins.bytes[1] & 0xF0) >> 4;
                let l = ins.bytes[1] & 0x0F;

                let h_v_reg = self.gprs.word_reg_by_idx(h);
                let h_v = h_v_reg.get_value_word(&mut self.mem);
                let h_v_addr = h_v_reg.get_addr();

                // Check low byte
                let l_v = if l & 0b1000 == 0b1000 {
                    // Rwn [Rwi]
                    let mm_mm = self.gprs.word_reg_by_idx(h & 0b11).get_value_word(&mut self.mem);
                    let addr = self.get_mem_address(mm_mm);
                    self.read_mem_word(addr)
                } else if l & 0b1100 == 0b1100 {
                    // Rbn [Rwi+]
                    let ii = self.gprs.word_reg_by_idx(h).get_byte(&mut self.mem);
                    todo!("Rwi+");
                } else {
                    // Rwn data3
                    (l & 0b0111) as u16
                };
                ((h_v, h_v_addr), l_v)
            }
            _ => panic!("Invalid Logical instruction instruction: {:02X?}", ins)
        };


        let mut psw = self.get_psw_flags();
        let result = match logic_op {
            LogicalOperation::OR => op1 | op2,
            LogicalOperation::AND => op1 & op2,
            LogicalOperation::SUB => op1.wrapping_sub(op2),
            LogicalOperation::CMP => op1.wrapping_sub(op2),
            LogicalOperation::ADD => op1.wrapping_add(op2),
            LogicalOperation::ADDC => op1.wrapping_add(op2).wrapping_add(psw.c() as u16).to_owned(),
            _ => panic!("Logic word op {:?} not implemented", logic_op)
        };

        match logic_op {
            LogicalOperation::OR | LogicalOperation::AND => {
                psw.set_e(op2 == 0x8000);
                psw.set_z(result == 0);
                psw.set_v(false);
                psw.set_c(false);
                psw.set_n(result >> 15 & 1 != 0)    
            },
            LogicalOperation::SUB | LogicalOperation::CMP => {
                psw.set_e(op2 == 0x8000);
                psw.set_z(result == 0);
                psw.set_v(sborrow_u16(op1, op2));
                psw.set_c(op1 < op2);
                psw.set_n(result >> 15 & 1 != 0);
            },
            LogicalOperation::ADD => {
                psw.set_e(op2 == 0x8000);
                psw.set_z(result == 0);
                psw.set_v(carry_u16(op1, op2));
                psw.set_c(scarry_u16(op1, op2));
                psw.set_n(result >> 15 & 1 != 0);
            },
            LogicalOperation::ADDC => {
                let c = psw.c() as u16;
                psw.set_e(op2 == 0x8000);
                psw.set_z(result == 0 && psw.z() != 0);
                psw.set_v(carry_u16(op1 + c, op2) || carry_u16(op1, op2 + c));
                psw.set_c(scarry_u16(op1 + c, op2) || scarry_u16(op1, op2 + c));
                psw.set_n(result >> 15 & 1 != 0);
            },
            _ => todo!("PSW flags not implemented for {:?}", logic_op)
        }
        
        self.set_psw_flags(psw);
        if logic_op != LogicalOperation::CMP {
            self.write_mem_word(op1_addr as usize, result as u16);
        }
    }

    fn logical_op_byte(&mut self, ins: &InstructionInfo, logic_op: LogicalOperation) {
        let ((op1, op1_addr), op2) = match ins.bytes[0] & 0x0F {
            // Rbn Rbm
            0x01 => {
                let dest_reg = self.gprs.byte_reg_by_idx((ins.bytes[1] & 0xF0) >> 4);
                (
                    (dest_reg.get_byte(&mut self.mem), dest_reg.get_addr()),
                    self.gprs.byte_reg_by_idx(ins.bytes[1] & 0x0F).get_byte(&mut self.mem),
                )
            },
            // RR MM MM (RR <- MMMM)
            0x03 => {
                let mm_mm = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                let addr = self.get_mem_address(mm_mm);
                let dest_reg = self.get_register(ins.bytes[1]);
                (
                    (dest_reg.get_byte(&mut self.mem), dest_reg.get_addr()),
                    self.read_mem_byte(addr),
                )
            },
            // RR MM MM (MMMM <- RR)
            0x05 => {
                let addr = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                let mem_addr = self.get_mem_address(addr);
                let mem_value = self.read_mem_byte(mem_addr);
                (
                    (mem_value, addr as u16),
                    self.get_register(ins.bytes[1]).get_byte(&mut self.mem)
                )
            },
            // RR DATA8 <IGNORED>
            0x07 => {
                let dest_reg = self.get_register(ins.bytes[1]);
                (
                    (dest_reg.get_byte(&mut self.mem), dest_reg.get_addr()),
                    ins.bytes[1]
                )
            },
            0x09 => {
                let h = (ins.bytes[1] & 0xF0) >> 4;
                let l = ins.bytes[1] & 0x0F;

                let h_v_reg = self.gprs.byte_reg_by_idx(h);
                let h_v = h_v_reg.get_byte(&mut self.mem);
                let h_v_addr = h_v_reg.get_addr();

                // Check low byte
                let l_v = if l & 0b1000 == 0b1000 {
                    // Rbn [Rwi]
                    let mm_mm = self.gprs.word_reg_by_idx(h & 0b11).get_value_word(&mut self.mem);
                    let addr = self.get_mem_address(mm_mm);
                    self.read_mem_byte(addr)
                } else if l & 0b1100 == 0b1100 {
                    // Rbn [Rwi+]
                    let ii = self.gprs.word_reg_by_idx(h).get_byte(&mut self.mem);
                    todo!("Rwi+");
                } else {
                    // Rbn data3
                    l & 0b0111
                };
                ((h_v, h_v_addr), l_v)
            }
            _ => panic!("Invalid Logical instruction instruction: {:02X?}", ins)
        };



        let mut psw = self.get_psw_flags();
        let result = match logic_op {
            LogicalOperation::OR => op1 | op2,
            LogicalOperation::AND => op1 & op2,
            LogicalOperation::SUB => op1.wrapping_sub(op2),
            LogicalOperation::CMP => op1.wrapping_sub(op2),
            LogicalOperation::ADD => op1.wrapping_add(op2),
            LogicalOperation::ADDC => op1.wrapping_add(op2).wrapping_add(psw.c() as u8),
            _ => panic!("Logic byte op {:?} not implemented", logic_op)
        };

        match logic_op {
            LogicalOperation::OR | LogicalOperation::AND => {
                psw.set_e(op2 == 0x80);
                psw.set_z(result == 0);
                psw.set_v(false);
                psw.set_c(false);
                psw.set_n(result >> 7 & 1 != 0)    
            },
            LogicalOperation::SUB | LogicalOperation::CMP => {
                psw.set_e(op2 == 0x80);
                psw.set_z(result == 0);
                psw.set_v(sborrow_u8(op1, op2));
                psw.set_c(op1 < op2);
                psw.set_n(result >> 7 & 1 != 0);
            },
            LogicalOperation::ADD => {
                psw.set_e(op2 == 0x80);
                psw.set_z(result == 0);
                psw.set_v(carry_u8(op1, op2));
                psw.set_c(scarry_u8(op1, op2));
                psw.set_n(result >> 7 & 1 != 0);
            },
            LogicalOperation::ADDC => {
                let c = psw.c();
                psw.set_e(op2 == 0x80);
                psw.set_z(result == 0 && psw.z() != 0);
                psw.set_v(carry_u8(op1 + c, op2) || carry_u8(op1, op2 + c));
                psw.set_c(scarry_u8(op1 + c, op2) || scarry_u8(op1, op2 + c));
                psw.set_n(result >> 7 & 1 != 0);
            },
            _ => todo!("PSW flags not implemented for {:?}", logic_op)
        }
        
        self.set_psw_flags(psw);
        if logic_op != LogicalOperation::CMP {
            self.write_mem_byte(op1_addr as usize, result);
        }
    }

    fn push(&mut self, ins: &InstructionInfo) {
        let mut sp = self.get_stack_pointer();
        let value = self.get_register(ins.bytes[1]).get_value_word(&mut self.mem);
        sp -= 2;

        let mut psw = self.get_psw_flags();
        psw.set_e(value as i16 != i16::MIN);
        psw.set_z(value == 0);
        psw.set_n(value & 1 << 15 != 0);
        self.set_psw_flags(psw);

        self.write_mem_word(sp as usize, value);
        self.set_stack_pointer(sp);
    }

    fn pop(&mut self, ins: &InstructionInfo) {
        let mut sp = self.get_stack_pointer();
        let value = self.read_mem_word(sp as usize);
        sp += 2;

        let mut psw = self.get_psw_flags();
        psw.set_e(value as i16 == i16::MIN);
        psw.set_z(value == 0);
        psw.set_n(value & 1 << 15 != 0);
        self.set_psw_flags(psw);

        self.set_stack_pointer(sp);
        self.get_register(ins.bytes[1]).set_value_word(&mut self.mem, value);
    }

    fn ret(&mut self, _ins: &InstructionInfo) {
        let calls_old = self.calls_tracker.pop_back().expect("RET called before CALLA!?");
        let sp = self.get_stack_pointer();

        let ip = self.read_mem_word(self.get_mem_address(sp)); // (IP) <- ((SP))
        self.set_stack_pointer(sp+2); // (SP) ← (SP) + 2
        let calls_now = CallsEntry {
            csp: self.get_code_seg_pointer(),
            ip: ip,
        };

        if calls_now != calls_old {
            panic!("CORRUPT STACK: Data pushed on CALLS: {:08X?}, Data popped in RETS: {:08X?}", calls_old, calls_now)
        } else {
            self.ip = ip;
        }
    }

    fn rets(&mut self, _ins: &InstructionInfo) {

        let calls_old = self.calls_tracker.pop_back().expect("RETS called before CALLS!?");

        let mut sp = self.get_stack_pointer();
        let ip = self.read_mem_word(self.get_mem_address(sp)); // (IP) <- ((SP))
        sp += 2;
        self.set_stack_pointer(sp); // (SP) ← (SP) + 2

        let csp = self.read_mem_byte(self.get_mem_address(self.get_stack_pointer()));
        sp += 2;
        self.set_stack_pointer(sp); // (SP) ← (SP) + 2

        let calls_now = CallsEntry {
            csp,
            ip,
        };
        if calls_now != calls_old {
            panic!("CORRUPT STACK: Data pushed on CALLS: {:08X?}, Data popped in RETS: {:08X?}", calls_old, calls_now)
        } else {
            self.ip = calls_now.ip;
            self.set_code_seg_pointer(calls_now.csp);
        }
    }

    fn rol(&mut self, ins: &InstructionInfo) {
        let (addr_dest, op1, op2) = match ins.bytes[0] {
            //Rwn Rwm
            0x0C => {
                let r_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_byte(&mut self.mem) & 0xF;
                let r_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4);
                (r_n.get_addr() as usize, r_n.get_value_word(&mut self.mem), r_m)
            },
            // Rwn #data4
            0x1C => {
                let r_n = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F);
                let data = (ins.bytes[1] & 0xF0) >> 4;
                (r_n.get_addr() as usize, r_n.get_value_word(&mut self.mem), data)
            }
            _ => panic!("Invalid ROR bytes: {:02X?}", ins)
        };

        let mut psw = self.get_psw_flags();
        let res = (op1 << op2) | (op1 >> (16-op2));
        psw.set_e(false);
        psw.set_z(res == 0);
        psw.set_v(false);
	    psw.set_c((op2 != 0) && ((op1 & (1 << (16 - op2))) != 0));
        psw.set_n(res >> 15 & 0b1 != 0);
        self.set_psw_flags(psw);
        self.write_mem_word(addr_dest, res);
    }

    fn ror(&mut self, ins: &InstructionInfo) {
        let (addr_dest, op1, op2) = match ins.bytes[0] {
            //Rwn Rwm
            0x2C => {
                let r_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_byte(&mut self.mem) & 0xF;
                let r_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4);
                (r_n.get_addr() as usize, r_n.get_value_word(&mut self.mem), r_m)
            },
            // Rwn #data4
            0x3C => {
                let r_n = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F);
                let data = (ins.bytes[1] & 0xF0) >> 4;
                (r_n.get_addr() as usize, r_n.get_value_word(&mut self.mem), data)
            }
            _ => panic!("Invalid ROR bytes: {:02X?}", ins)
        };

        let mut psw = self.get_psw_flags();
        let res = (op1 >> op2) | (op1 << (16-op2));
        psw.set_e(false);
        psw.set_z(res == 0);
        psw.set_v((op2 != 0) && ((op1 & ((1 << op1) - 1)) != 0));
	    psw.set_c((op2 != 0) && ((op1 & (1 << (op1 - 1))) != 0));
        psw.set_n(res >> 15 & 0b1 != 0);
        self.set_psw_flags(psw);
        self.write_mem_word(addr_dest, res);
    }

    fn ashr(&mut self, ins: &InstructionInfo) {
        let (addr_dest, mut op1, op2) = match ins.bytes[0] {
            //Rwn Rwm
            0xAC => {
                let r_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_byte(&mut self.mem) & 0xF;
                let r_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4);
                (r_n.get_addr() as usize, r_n.get_value_word(&mut self.mem), r_m)
            },
            // Rwn #data4
            0xBC => {
                let r_n = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F);
                let data = (ins.bytes[1] & 0xF0) >> 4;
                (r_n.get_addr() as usize, r_n.get_value_word(&mut self.mem), data)
            }
            _ => panic!("Invalid SHR bytes: {:02X?}", ins)
        };

        let mut psw = self.get_psw_flags();

        let mut count = op2;
        psw.set_e(false);
        psw.set_v(false);
        psw.set_c(false);
        let mut n = 0;
        while count != 0 {
            psw.set_v((psw.c() | psw.v()) != 0);
            psw.set_c(op1 & 0b1 != 0);
            
            op1 &= !(1 << n); // clear bit we want to set
            let b = (op1 >> (n+1)) & 0b1; // Bit we want to move
            op1 |= b << n; // (op1_n) ← (op1_(n+1)) [n = 0 … 14]

            count -= 1;
            n += 1;
        }
        psw.set_z(op1 as i16 == 0);
        self.set_psw_flags(psw);
        self.write_mem_word(addr_dest, op1);
    }

    fn shr(&mut self, ins: &InstructionInfo) {
        let (addr_dest, mut op1, op2) = match ins.bytes[0] {
            //Rwn Rwm
            0x6C => {
                let r_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_byte(&mut self.mem) & 0xF;
                let r_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4);
                (r_n.get_addr() as usize, r_n.get_value_word(&mut self.mem), r_m)
            },
            // Rwn #data4
            0x7C => {
                let r_n = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F);
                let data = (ins.bytes[1] & 0xF0) >> 4;
                (r_n.get_addr() as usize, r_n.get_value_word(&mut self.mem), data)
            }
            _ => panic!("Invalid SHR bytes: {:02X?}", ins)
        };

        let mut psw = self.get_psw_flags();
        let res = op1 >> op2;
        psw.set_e(false);
        psw.set_z(res == 0);
        psw.set_v((op2 != 0) && ((op1 & ((1 << op2) - 1)) != 0));
	    psw.set_c((op2 != 0) && ((op1 & (1 << (op2 - 1))) != 0));
        psw.set_n(res >> 15 & 0b1 != 0);
        self.set_psw_flags(psw);
        self.write_mem_word(addr_dest, res);

    }

    fn shl(&mut self, ins: &InstructionInfo) {
        let (addr_dest, op1, op2) = match ins.bytes[0] {
            //Rwn Rwm
            0x4C => {
                let r_m = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_byte(&mut self.mem) & 0xF;
                let r_n = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4);
                (r_n.get_addr() as usize, r_n.get_value_word(&mut self.mem), r_m)
            },
            // Rwn #data4
            0x5C => {
                let r_n = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F);
                let data = (ins.bytes[1] & 0xF0) >> 4;
                (r_n.get_addr() as usize, r_n.get_value_word(&mut self.mem), data)
            }
            _ => panic!("Invalid SHL bytes: {:02X?}", ins)
        };

        let mut psw = self.get_psw_flags();
        let res = op1 << op2;
        psw.set_e(false);
        psw.set_z(res == 0);
        psw.set_v(false);
        psw.set_c(op2 != 0 && (op1 & (1 << (16 - op2))) != 0);
        psw.set_n(res >> 15 & 0b1 != 0);
        self.set_psw_flags(psw);
        self.write_mem_word(addr_dest, res);
    }

    fn read_mem_word(&mut self, addr: usize) -> u16 {
        if addr > 0x80000 {
            u16::from_le_bytes(self.flash[addr..addr+2].try_into().unwrap())
        } else {
            u16::from_le_bytes(self.mem.read(addr as u16, 2).try_into().unwrap())
        }
    }

    fn read_mem_raw(&mut self, addr: usize) -> [u8; 2] {
        if addr > 0x80000 {
            self.flash[addr..addr+2].try_into().unwrap()
        } else {
            self.mem.read(addr as u16, 2).try_into().unwrap()
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

    fn write_mem_raw(&mut self, addr: usize, input: [u8; 2]) {
        // TODO (Can we write to flash?)
        self.mem.write(addr as u16, &input);
    }

    fn write_mem_byte(&mut self, addr: usize, input: u8) {
        // TODO (Can we write to flash?)
        self.mem.write(addr as u16, &[input]);
    }

    pub fn get_mem_region(&mut self, addr: usize, size: usize) -> Vec<u8> {
        if addr >= 0x80000 {
            self.flash[addr..addr as usize].to_vec()
        } else {
            self.mem.read(addr as u16, size as u16).to_vec()
        }
    }

    pub fn get_mem_region_no_tracking(&self, addr: usize, size: usize) -> Vec<u8> {
        if addr >= 0x80000 {
            self.flash[addr..addr as usize].to_vec()
        } else {
            self.mem.raw[addr..addr+size].to_vec()
        }
    }

}