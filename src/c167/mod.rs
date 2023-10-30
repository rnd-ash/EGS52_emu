use std::{cell::RefCell, rc::Rc, borrow::BorrowMut, mem::MaybeUninit};

use self::{instructions::{InstructionInfo, Instruction}, register::{Register, esfr_registers, sfr_registers}, gpr::GPRData};

pub mod instructions;
pub mod register;
pub mod gpr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PSWFlag {
    pub e: bool,
    pub z: bool,
    pub v: bool,
    pub c: bool,
    pub n: bool
}

///! Infineon C167 emulator

#[derive(Debug, Clone)]
pub struct C167 {
    ip: usize,
    mem: Rc<RefCell<[u8; 0xFFFF]>>,
    flash: Vec<u8>,
    sfr_registers: [Option<Register>; 256],
    esfr_registers: [Option<Register>; 256],
    extr_value: u8,
    extp_value: u8,
    extp_page: u16,
    gprs: GPRData,
    ins_exec: usize
}

unsafe impl Send for C167{}
unsafe impl Sync for C167{}

impl C167 {

    pub fn new(flash: Vec<u8>) -> Self {

        let mem = [0; 0xFFFF];
        let rc = Rc::new(RefCell::new(mem));

        let mut sfr_registers = sfr_registers(rc.clone());

        let esfr_registers = esfr_registers(rc.clone());


        let mut f = Self {
            mem: rc.clone(),
            ip: 0x80000,
            flash: vec![0; 0x80000],
            sfr_registers,
            esfr_registers,
            extr_value: 0,
            gprs: GPRData::new(rc),
            ins_exec: 0,
            extp_page: 0,
            extp_value: 0
        };
        
        f.flash.extend_from_slice(&flash);
        f
    }

    pub fn exec_step(&mut self) -> Option<InstructionInfo> {
        let ret = InstructionInfo::from_bytes(&self.flash, self.ip);
        if let Some((instruction, new_offset)) = ret.as_ref() {
            self.ip = *new_offset;
            self.process_instruction(instruction)
        }
        ret.map(|x| x.0)
    }

    pub fn get_psw_flags<'a> (&'a self) -> PSWFlag {
        let r = self.sfr_registers[0x88].as_ref().unwrap().get_raw()[1];
        PSWFlag { 
            e: r & (0b1 << 4) != 0, 
            z: r & (0b1 << 3) != 0, 
            v: r & (0b1 << 2) != 0, 
            c: r & (0b1 << 1) != 0, 
            n: r & (0b1 << 0) != 0 
        }

    }

    fn set_psw_flags<'a> (&'a mut self, flag: PSWFlag) {
        let nibble: u8 = 
            (flag.e as u8) << 4 |
            (flag.z as u8) << 3 |
            (flag.v as u8) << 2 |
            (flag.c as u8) << 1 |
            (flag.n as u8) << 0;

        let mut old= self.sfr_registers[0x88].as_mut().unwrap().get_raw();
        old[1] &= 0xF0;
        old[1] |= nibble;

        self.sfr_registers[0x88].as_mut().unwrap().set_value(old)
    }

    pub fn dump_state(&mut self) -> ! {
        println!("Executed {} instructions before exception", self.ins_exec);
        println!("--GPRS--");
        for x in 0..16 {
            let gpr = self.gprs.get_reg_by_id(x);
            println!("{} - Addr: 0x{:04X}, Content: {:02X?}", gpr.name, gpr.get_addr(), gpr.get_raw())
        }
        println!("--SFR REGISTERS--");
        for reg in &self.sfr_registers {
            if let Some(r) = reg {
                println!("{} - Addr: 0x{:04X}, Content: {:02X?}", r.name, r.get_addr(), r.get_raw())
            }
        }
        todo!();
    }

    fn process_instruction(&mut self, ins: &InstructionInfo) {  
        let i = ins.instruction;  
        match i {
            Instruction::ADD => self.add(ins),
            Instruction::ADDB => todo!(),
            Instruction::ADDC => todo!(),
            Instruction::ADDCB => todo!(),
            Instruction::AND => todo!(),
            Instruction::ANDB => todo!(),
            Instruction::ASHR => todo!(),
            Instruction::BAND => todo!(),
            Instruction::BCLR => self.bclr(ins),
            Instruction::BCMP => todo!(),
            Instruction::BFLDL => self.bfldl(ins),
            Instruction::BFLDH => self.bfldh(ins),
            Instruction::BMOV => todo!(),
            Instruction::BMOVN => todo!(),
            Instruction::BOR => todo!(),
            Instruction::BSET => self.bset(ins),
            Instruction::BXOR => todo!(),
            Instruction::CALLA => self.calla(ins),
            Instruction::CALLI => todo!(),
            Instruction::CALLR => todo!(),
            Instruction::CALLS => todo!(),
            Instruction::CMP => self.cmp(ins),
            Instruction::CMPB => self.cmpb(ins),
            Instruction::CMPD1 => todo!(),
            Instruction::CMPD2 => todo!(),
            Instruction::CMPL1 => todo!(),
            Instruction::CMPL2 => todo!(),
            Instruction::CPL => todo!(),
            Instruction::CPLB => todo!(),
            Instruction::DISWDT => todo!(),
            Instruction::DIV => todo!(),
            Instruction::DIVL => todo!(),
            Instruction::DIVLU => todo!(),
            Instruction::DIVU => todo!(),
            Instruction::EINIT => {println!("End of CPU initialization!")},
            Instruction::EXTR => self.extr(ins),
            Instruction::EXTS_OR_P => self.extp(ins),
            Instruction::IDLE => todo!(),
            Instruction::JB => self.jb(ins),
            Instruction::JBC => todo!(),
            Instruction::JMPA => self.jumpa(ins),
            Instruction::JMPI => todo!(),
            Instruction::JMPR => self.jumpr(ins),
            Instruction::JMPS => self.jumps(ins),
            Instruction::JNB => self.jnb(ins),
            Instruction::JNBS => todo!(),
            Instruction::MOV => self.mov(ins),
            Instruction::MOVBZ => todo!(),
            Instruction::MOVBS => todo!(),
            Instruction::MOVB => self.movb(ins),
            Instruction::MUL => todo!(),
            Instruction::MULU => todo!(),
            Instruction::NEG => self.neg(ins),
            Instruction::NEGB => todo!(),
            Instruction::NOP => {},
            Instruction::OR => todo!(),
            Instruction::ORB => todo!(),
            Instruction::PCALL => todo!(),
            Instruction::POP => self.pop(ins),
            Instruction::PRIOR => todo!(),
            Instruction::PUSH => self.push(ins),
            Instruction::PWRDN => todo!(),
            Instruction::RET => self.ret(ins),
            Instruction::RETI => todo!(),
            Instruction::RETP => todo!(),
            Instruction::RETS => todo!(),
            Instruction::ROR => todo!(),
            Instruction::SCXT => todo!(),
            Instruction::SHL => todo!(),
            Instruction::SHR => todo!(),
            Instruction::SRST => todo!(),
            Instruction::SRVWDT => {println!("Watchdog init!")},
            Instruction::SUBC => todo!(),
            Instruction::SUBCB => todo!(),
            Instruction::SUB => todo!(),
            Instruction::SUBB => todo!(),
            Instruction::TRAP => todo!(),
            Instruction::ROL => todo!(),
            Instruction::XOR => todo!(),
            Instruction::XORB => todo!(),
        }
        self.ins_exec += 1;
        self.gprs.set_context_pointer(self.sfr_registers[8].as_ref().unwrap().get_value_u16());
        if self.extr_value > 0 && i != Instruction::EXTR {
            self.extr_value -= 1;
        }

        if self.extp_value > 0 && i != Instruction::EXTS_OR_P {
            self.extp_value -= 1;
        }
    }

    pub fn get_gpr_data<'a>(&'a self) -> &'a GPRData {
        &self.gprs
    }

    pub fn get_sfr_reg<'a> (&'a self, reg_id: u8) -> &'a Register {
        self.sfr_registers[reg_id as usize].as_ref().unwrap()
    }

    fn get_stack_pointer(&self) -> u16 {
        self.sfr_registers[9].as_ref().unwrap().get_value_u16()
    }

    fn set_stack_pointer(&mut self, sp: u16) {
        self.sfr_registers[9].as_mut().unwrap().set_value(sp.to_le_bytes());
    }

    pub fn get_register<'a> (&'a mut self, reg_id: u8) -> &'a mut Register {

        if reg_id >= 0xF0 {
            return self.gprs.get_reg_by_id(reg_id)
        }

        match self.extr_value {
            0 => self.sfr_registers[reg_id as usize].as_mut().unwrap(),
            _ => self.esfr_registers[reg_id as usize].as_mut().unwrap(),
        }
    }

    fn calla(&mut self, ins: &InstructionInfo) {
        // c0 mm mm
        let should_jump = match ins.bytes[0] & 0xF0 {
            // Unconditional
            0xC0 => {
                true
            }
            _ => panic!("Unknown CALLA for {:02X?}", ins)
        };

        if should_jump {
            let mut sp = self.get_stack_pointer();
            sp -= 2;
            self.u16_into_mem(sp, (self.ip - 0x80000) as u16);
            self.ip = 0x80000 + u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()) as usize;
            self.set_stack_pointer(sp);
        }
    }

    fn cmp(&mut self, ins: &InstructionInfo) {
        let (op1, op2) = match ins.bytes[0] {
            0x40 => {
                (self.get_register((ins.bytes[1] & 0xF0) >> 4).get_value_i16(),
                self.get_register(ins.bytes[1] & 0x0F).get_value_i16())
            },
            0x42 => {
                (self.get_register(ins.bytes[1]).get_value_i16(),
                self.mem_to_u16(u16::from_le_bytes(ins.bytes[2..].try_into().unwrap())) as i16)
            },
            0x46 => {
                (self.get_register(ins.bytes[1]).get_value_i16(),
                i16::from_le_bytes(ins.bytes[2..].try_into().unwrap()))
            }
            0x48 => {
                let h = (ins.bytes[1] & 0xF0) >> 4;
                let l = ins.bytes[1] & 0x0F;

                let h_v = self.gprs.word_reg_by_idx(h).get_value_i16();
                

                // Check low byte
                let l_v = if l & 0b1000 == 0b1000 {
                    // Rwn [Rwi]
                    let ii = self.gprs.word_reg_by_idx(h).get_value_u16();
                    self.mem_to_u16(ii) as i16
                } else if l & 0b1100 == 0b1100 {
                    // Rwn [Rwi+]
                    let ii = self.gprs.word_reg_by_idx(h).get_value_u16();
                    todo!("Rwi+");
                } else {
                    // Rwn data3
                    (l & 0b0111) as i16
                };

                (h_v, l_v)

            },
            _ => panic!("Invalid CMP instruction: {:02X?}", ins)
        };

        let (result, overflowed) = op1.overflowing_sub(op2);
        let mut psw = self.get_psw_flags();

        psw.e = op2 == i16::MIN;
        psw.z = result == 0;
        psw.v = overflowed;
        psw.c = (op1 & (1 << 15) != 0) && (op2 & (1 << 15) != 0);
        psw.n = result & (1 << 15) != 0;

        self.set_psw_flags(psw);


    }

    fn cmpb(&mut self, ins: &InstructionInfo) {
        let (op1, op2) = match ins.bytes[0] {
            // Rbn Rbm
            0x41 => {
                (
                    self.gprs.byte_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_u8(),
                    self.gprs.byte_reg_by_idx(ins.bytes[1] & 0x0F).get_u8()
                )
            },
            // RR MM MM
            0x43 => {
                (
                    self.get_register(ins.bytes[1]).get_u8(),
                    self.mem.borrow()[u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()) as usize]
                )
            },
            // RR data8 <IGNORED>
            0x47 => {
                (
                    self.gprs.byte_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_u8(),
                    ins.bytes[2]
                )
            },
            0x49 => {
                let h = (ins.bytes[1] & 0xF0) >> 4;
                let l = ins.bytes[1] & 0x0F;

                let h_v = self.gprs.byte_reg_by_idx(h).get_u8();
                

                // Check low byte
                let l_v = if l & 0b1000 == 0b1000 {
                    // Rbn [Rwi]
                    let ii = self.gprs.word_reg_by_idx(h).get_value_u16();
                    self.mem.borrow()[ii as usize]
                } else if l & 0b1100 == 0b1100 {
                    // Rbn [Rwi+]
                    let ii = self.gprs.byte_reg_by_idx(h).get_u8();
                    todo!("Rwi+");
                } else {
                    // Rwn data3
                    l & 0b0111
                };
                (h_v, l_v)
            }
            _ => panic!("Invalid CMPB instruction: {:02X?}", ins)
        };

        let (result, overflowed) = op1.overflowing_sub(op2);
        let mut psw = self.get_psw_flags();

        psw.e = op2 as i8 == i8::MIN;
        psw.z = result == 0;
        psw.v = overflowed;
        psw.c = (op1 & (1 << 7) != 0) && (op2 & (1 << 7) != 0);
        psw.n = result & (1 << 7) != 0;

        self.set_psw_flags(psw);
    }

    fn extr(&mut self, ins: &InstructionInfo) {
        let v = (ins.bytes[1] & 0b00110000) >> 4;
        // TODO - Disable interrupts
        if v >= 1 && v <= 4 {
            self.extr_value = v;
        } else {
            self.extr_value = 1;
        }
    }

    fn add(&mut self, ins: &InstructionInfo) {
        match ins.bytes[0] {
            // Rw Rw
            0x00 => {
                let op1 = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_i16();
                let op2 = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_i16();

                let (res, overflow) = op1.overflowing_add(op2);
                let mut psw = self.get_psw_flags();

                psw.e = op2 == i16::MIN;
                psw.z = res == 0;
                psw.v = overflow;
                psw.c = (op1 & (1 << 15) != 0) && (op2 & (1 << 15) != 0);
                psw.n = res & (1 << 15) != 0;
 
                self.set_psw_flags(psw);
                self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).set_value(res.to_le_bytes());
            },
            // Reg, Data16
            0x06 => {
                let op1 = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_i16();
                let op2 = i16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                
                let (res, overflow) = op1.overflowing_add(op2);
                let mut psw = self.get_psw_flags();

                psw.e = op2 == i16::MIN;
                psw.z = res == 0;
                psw.v = overflow;
                psw.c = (op1 & (1 << 15) != 0) && (op2 & (1 << 15) != 0);
                psw.n = res & (1 << 15) != 0;
 
                self.set_psw_flags(psw);
                self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).set_value(res.to_le_bytes());
            }
            _ => todo!("Add not implemented for bytes {:02X?}", ins.bytes)
        }
    }

    fn neg(&mut self, ins: &InstructionInfo) {
        match ins.bytes[0] {
            0x81 => {
                let v1 = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_u16();
                let mut psw = self.get_psw_flags();
                let (res, overflow) = 0u16.overflowing_sub(v1);
                psw.e = v1 == 0x8000;
                psw.z = res == 0;
                psw.v = overflow;
                psw.c = false; // TODO
                psw.n = res & (1 << 15) != 0;
                self.set_psw_flags(psw);
                self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).set_value(res.to_le_bytes());
            },
            _ => todo!("Neg not implemented for bytes {:02X?}", ins.bytes)
        }
    }

    fn mov(&mut self, ins: &InstructionInfo) {
        match ins.bytes[0] {
            // Reg, Data16
            0xE6 => {
                let value = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());

                let mut psw = self.get_psw_flags();
                psw.e = value == 0x8000; // i16 min
                psw.z = value == 0;
                psw.n = value & (1 << 15) != 0;
                self.set_psw_flags(psw);

                let reg = self.get_register(ins.bytes[1]);
                reg.set_value(value.to_le_bytes())
            },
            // Rw, Rw
            0xF0 => {
                let value = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_u16();

                let mut psw = self.get_psw_flags();
                psw.e = value == 0x8000; // i16 min
                psw.z = value == 0;
                psw.n = value & (1 << 15) != 0;
                self.set_psw_flags(psw);
                self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).set_value(value.to_le_bytes())
            }
            // Rb, Rb
            0xF1 => {
                let value = self.gprs.byte_reg_by_idx(ins.bytes[1] & 0x0F).get_value_u16();

                let mut psw = self.get_psw_flags();
                psw.e = value == 0x8000; // i16 min
                psw.z = value == 0;
                psw.n = value & (1 << 15) != 0;
                self.set_psw_flags(psw);
                self.gprs.byte_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).set_value(value.to_le_bytes())
            }
            0xF2 => {
                let value = self.get_register(ins.bytes[1]).get_value_u16();

                let mut psw = self.get_psw_flags();
                psw.e = value == 0x8000; // i16 min
                psw.z = value == 0;
                psw.n = value & (1 << 15) != 0;
                self.set_psw_flags(psw);
                let addr = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
                self.u16_into_mem(addr, value);
            }
            // Rw [Rw]
            0xA8 => {
                let addr = self.gprs.word_reg_by_idx(ins.bytes[1] & 0x0F).get_value_u16();
                let value = self.mem_to_u16(addr);
                let mut psw = self.get_psw_flags();
                psw.e = value == 0x8000; // i16 min
                psw.z = value == 0;
                psw.n = value & (1 << 15) != 0;
                self.set_psw_flags(psw);
                self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).set_value(value.to_le_bytes())


            }
            _ => panic!("Invalid operand for MOV {:02X?}", ins.bytes)
        }
    }

    fn movb(&mut self, ins: &InstructionInfo) {
        match ins.bytes[0] {
            // Reg mem mem
            0xF3 => {
                let op2 = self.mem.borrow()[u16::from_le_bytes(ins.bytes[2..].try_into().unwrap()) as usize] as i8;
                let mut psw = self.get_psw_flags();
                psw.e = op2 == i8::MIN;
                psw.z = op2 == 0;
                psw.n = op2 & (1 << 7) != 0;
                self.set_psw_flags(psw);
                self.get_register(ins.bytes[1]).set_u8(op2 as u8);

            }
            // [Rw] Rb
            0xB9 => {
                let op2 = self.gprs.byte_reg_by_idx(ins.bytes[1] & 0x0F).get_u8() as i8;
                let mut psw = self.get_psw_flags();
                psw.e = op2 == i8::MIN;
                psw.z = op2 == 0;
                psw.n = op2 & (1 << 7) != 0;
                self.set_psw_flags(psw);
                let addr = self.gprs.word_reg_by_idx((ins.bytes[1] & 0xF0) >> 4).get_value_u16();
                self.u8_into_mem(addr, op2 as u8);

            }
            // Rbn #data4
            0xE1 => {
                let op2 = ((ins.bytes[1] & 0xF0) >> 4) as i8;
                let mut psw = self.get_psw_flags();
                psw.e = op2 == i8::MIN;
                psw.z = op2 == 0;
                psw.n = op2 & (1 << 7) != 0;
                self.set_psw_flags(psw);
                self.gprs.byte_reg_by_idx(ins.bytes[1] & 0x0F).set_u8(op2 as u8);
            }
            _ => panic!("Invalid operand for MOVB {:02X?}", ins.bytes)
        }
    }

    fn bclr(&mut self, ins: &InstructionInfo) {
        let reg = self.get_register(ins.bytes[1]);
    }

    fn bset(&mut self, ins: &InstructionInfo) {
        let addr: u16 = 0xFD00 + (ins.bytes[1] as u16 * 2);
        let mut tmp = self.mem_to_u16(addr);
        let bit = ins.bytes[0] & 0xF0;

        let old = tmp >> bit;

        tmp |= 0b1 << bit as usize;

        let mut psw = self.get_psw_flags();
        psw.e = false;
        psw.z = old == 0;
        psw.v = false;
        psw.c = false;
        psw.n = old != 0;
        self.set_psw_flags(psw);

        self.u16_into_mem(addr, tmp);
    }

    fn bfldh(&mut self, ins: &InstructionInfo) {
        // ^ AND
        // v OR
        // -| - logically complemented
        let reg = self.get_register(ins.bytes[1]);
        let mut tmp = reg.get_raw();
        tmp[1] = (tmp[1] & (0xFF | ins.bytes[2])) | ins.bytes[3];
        reg.set_value(tmp);
    }   

    fn bfldl(&mut self, ins: &InstructionInfo) {
        // ^ AND
        // v OR
        // -| - logically complemented
        let reg = self.get_register(ins.bytes[1]);
        let mut tmp = reg.get_raw();
        tmp[0] = (tmp[0] & (0xFF | ins.bytes[2])) | ins.bytes[3];
        reg.set_value(tmp);
    }

    fn extp(&mut self, ins: &InstructionInfo) {
        match ins.bytes[0] {
            // Rwm #irange2
            0xDC => {
                panic!("TODO {:02X?}", ins);
            },
            // #Pag #irange2
            0xD7 => {
                let mut irange = (ins.bytes[1] & 0b00110000) >> 4;
                if irange < 1 {
                    irange = 1;
                } else if irange > 4 {
                    irange = 4;
                }
                let page_bytes = [ins.bytes[2], ins.bytes[3] & 0b00111111];
                let page = u16::from_le_bytes(page_bytes);
                self.extp_value = irange;
                self.extp_page = page;
            },
            _ => panic!("Invalid EXTP instruction {:02X?}", ins.bytes[0])
        }
    }

    fn jb(&mut self, ins: &InstructionInfo) {
        // QQ rr q0
        let value = self.get_register(ins.bytes[1]).get_value_u16();
        let bit = (ins.bytes[3] & 0xF0) >> 4;
        let relative_offset = ins.bytes[2] as i8;

        if value & 1<< bit != 0 {
            // Set. Jump
            self.ip = self.ip.wrapping_add_signed(relative_offset as isize);
        }
    }

    fn jnb(&mut self, ins: &InstructionInfo) {
        // QQ rr q0
        let value = self.get_register(ins.bytes[1]).get_value_u16();
        let bit = (ins.bytes[3] & 0xF0) >> 4;
        let relative_offset = ins.bytes[2] as i8;

        if value & 1<< bit == 0 {
            // Clear. Jump
            self.ip = self.ip.wrapping_add_signed(relative_offset as isize);
        }
    }

    fn jumpa(&mut self, ins: &InstructionInfo) {
        // CC, CADDR
        assert!(ins.bytes.len() == 4);
        let cc = ins.bytes[1];
        let caddr = u16::from_le_bytes(ins.bytes[2..4].try_into().unwrap());
    }

    fn jumps(&mut self, ins: &InstructionInfo) {
        // SEG, CADDR
        assert!(ins.bytes.len() == 4);
        let seg = ins.bytes[1];
        let caddr = u16::from_le_bytes(ins.bytes[2..4].try_into().unwrap());
        let offset = (0x10000u32*(seg as u32)) + caddr as u32;
        self.ip = offset as usize;
    }

    fn jumpr(&mut self, ins: &InstructionInfo) {
        let flag = self.get_psw_flags();
        let offset = (ins.bytes[1] as i8) as i16 * 2;
        match ins.bytes[0] & 0xF0 {
            0x00 => { // Unconditional
                self.ip = self.ip.wrapping_add_signed(offset as isize);
            }
            0x20 => { // cc_EQ or cc_Z
                if flag.z == true {
                    // Branch
                    self.ip = self.ip.wrapping_add_signed(offset as isize);
                }
            }
            0x30 => { // CC_NZ/CC_NE
                if flag.z == false {
                    // Branch
                    self.ip = self.ip.wrapping_add_signed(offset as isize);
                }
            },
            _ => todo!("JUMPR not implemented for bytes {:02X?}", ins.bytes)
        }
    }

    fn push(&mut self, ins: &InstructionInfo) {
        let mut sp = self.get_stack_pointer();
        let value = self.get_register(ins.bytes[1]).get_value_u16();
        sp -= 2;

        let mut psw = self.get_psw_flags();
        psw.e = value == 0x8000;
        psw.z = value == 0;
        psw.n = value & 1 << 15 != 0;
        self.set_psw_flags(psw);

        self.u16_into_mem(sp, value);
        self.set_stack_pointer(sp);
    }

    fn pop(&mut self, ins: &InstructionInfo) {
        let mut sp = self.get_stack_pointer();
        let value = self.mem_to_u16(sp);
        sp += 2;

        let mut psw = self.get_psw_flags();
        psw.e = value == 0x8000;
        psw.z = value == 0;
        psw.n = value & 1 << 15 != 0;
        self.set_psw_flags(psw);

        self.set_stack_pointer(sp);
        self.get_register(ins.bytes[1]).set_value(value.to_le_bytes());
    }

    fn ret(&mut self, _ins: &InstructionInfo) {
        let mut sp = self.get_stack_pointer();
        self.ip = 0x80000 + (self.mem_to_u16(sp) as usize);
        sp += 2;
        self.set_stack_pointer(sp);
    }

    fn mem_to_u16(&self, addr: u16) -> u16 {
        let b = self.mem.borrow();
        let buf = b.as_ref()[addr as usize..addr as usize +2].try_into().unwrap();
        u16::from_le_bytes(buf)
    }

    fn u16_into_mem(&mut self, addr: u16, input: u16) {
        self.mem.borrow_mut().replace_with(|mut old| {
            old[addr as usize..addr as usize +2].copy_from_slice(&input.to_le_bytes());
            *old
        });
    }

    fn u8_into_mem(&mut self, addr: u16, input: u8) {
        self.mem.borrow_mut().replace_with(|mut old| {
            old[addr as usize] = input;
            *old
        });
    }

}