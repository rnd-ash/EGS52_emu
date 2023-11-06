use super::{instructions::InstructionInfo, gpr::IndirectType, PSWFlag};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LogicOp {
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

pub fn check_overflow(op1: i16, op2: i16) -> bool {
    op1.checked_add(op2).is_none()
}

pub fn check_overflowb(op1: i8, op2: i8) -> bool {
    op1.checked_add(op2).is_none()
}

pub fn check_underflow(op1: i16, op2: i16) -> bool {
    op1.checked_sub(op2).is_none()
}

pub fn check_underflowb(op1: i8, op2: i8) -> bool {
    op1.checked_sub(op2).is_none()
}

pub fn check_carry_sig_bit(op1: u16, op2: u16) -> bool {
    let res = op1.wrapping_add(op2);
    return res < op1;
}

pub fn check_carry_sig_bitb(op1: u8, op2: u8) -> bool {
    let res = op1.wrapping_add(op2);
    return res < op1;
}

pub fn check_borrow(op1: u16, op2: u16) -> bool {
    let res = op1.wrapping_add(op2);
    return res > op1;
}

pub fn check_borrowb(op1: u8, op2: u8) -> bool {
    let res = op1.wrapping_add(op2);
    return res > op1;
}

impl super::C167 {

    pub fn bclr(&mut self, ins: &InstructionInfo) {
        // (op1) ← 0
        //
        // bitaddrQ.q       qE QQ
        let bit_loc = ins.h(0);
        let addr = self.get_word_offset(ins.bytes[1]);
        let mut tmp = self.read_mem_word(addr);

        let bit = tmp >> bit_loc & 0b1;
        let mask = !(1 << bit_loc);
        tmp &= mask;
        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(!bit == 1);
        psw.set_v(false);
        psw.set_c(false);
        psw.set_n(bit == 1);
        self.set_psw_flags(psw);

        self.write_mem_word(addr, tmp)
    }

    pub fn bfldh(&mut self, ins: &InstructionInfo) {
        // (tmp) ← (op1)
        // (high byte (tmp)) ← ((high byte (tmp) ∧ ¬op2) ∨ op3)
        // (op1) ← (tmp)
        //
        // bitoffQ, #mask8, #data8      1A QQ ## @@
        let bitoff = ins.bytes[1]; // QQ
        let data8 = ins.bytes[2]; // ##
        let mask8 = ins.bytes[3]; // @@

        let addr = self.get_word_offset(bitoff);
        let mut tmp = self.read_mem_word(addr);

        let mut hb = (tmp & 0xFF00 >> 8) as u8; // Save high byte
        tmp &= 0x00FF; // Clear out old high byte
        hb = (hb & !mask8) | data8;
        tmp |= (hb as u16) << 8;

        // Update PSW
        let mut psw = self.get_psw_flags();
        psw.set_e(tmp == 0x8000);
        psw.set_z(tmp == 0);
        psw.set_v(false);
        psw.set_c(false);
        psw.set_n(((tmp >> 15) & 0b1) != 0);
        self.set_psw_flags(psw);

        self.write_mem_word(addr, tmp);
    }

    pub fn bfldl(&mut self, ins: &InstructionInfo) {
        // (tmp) ← (op1)
        // (low byte (tmp)) ← ((low byte (tmp) ∧ ¬op2) ∨ op3)
        // (op1) ← (tmp)
        //
        //  bitoffQ, #mask8, #data8      0A QQ @@ ##
        let bitoff = ins.bytes[1]; // QQ
        let data8 = ins.bytes[3]; // ##
        let mask8 = ins.bytes[2]; // @@

        let addr = self.get_word_offset(bitoff);
        let mut tmp = self.read_mem_word(addr);

        let mut hb = (tmp & 0x00FF) as u8; // Save high byte
        tmp &= 0xFF00; // Clear out old high byte
        hb = (hb & !mask8) | data8;
        tmp |= hb as u16;

        // Update PSW
        let mut psw = self.get_psw_flags();
        psw.set_e(tmp == 0x8000);
        psw.set_z(tmp == 0);
        psw.set_v(false);
        psw.set_c(false);
        psw.set_n(((tmp >> 15) & 0b1) != 0);
        self.set_psw_flags(psw);

        self.write_mem_word(addr, tmp);
    }

    pub fn bmov(&mut self, ins: &InstructionInfo) {
        //  BMOV bitaddrZ.z, bitaddrQ.q         4A QQ ZZ qz
        // (op1) ← (op2)
        let addr_q = self.get_word_offset(ins.bytes[1]);
        let addr_z = self.get_word_offset(ins.bytes[2]);
        let bit_q = ins.h(3);
        let bit_z = ins.l(3);
        let val_q = self.read_mem_word(addr_q);
        let mut val_z = self.read_mem_word(addr_z);

        let old_bit = val_z >> bit_z & 0b1;

        // Clear old bit
        val_z &= !(0b1 << bit_z);
        // Set bit
        val_z |= (val_q >> bit_q & 0b1) << bit_z;
        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(old_bit != 1);
        psw.set_v(false);
        psw.set_c(false);
        psw.set_n(old_bit == 1);
        self.set_psw_flags(psw);
        self.write_mem_word(addr_z, val_z);


    }

    pub fn bmovn(&mut self, ins: &InstructionInfo) {
        //  BMOVN bitaddrZ.z, bitaddrQ.q        3A QQ ZZ qz
        // (op1) ← ¬(op2)
        let addr_q = self.get_word_offset(ins.bytes[1]);
        let addr_z = self.get_word_offset(ins.bytes[2]);
        let bit_q = ins.h(3);
        let bit_z = ins.l(3);
        let val_q = self.read_mem_word(addr_q);
        let mut val_z = self.read_mem_word(addr_z);

        let old_bit = val_z >> bit_z & 0b1;

        // Clear old bit
        val_z &= !(0b1 << bit_z);
        // Set bit (inverted)
        val_z |= (!(val_q >> bit_q & 0b1)) << bit_z;
        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(old_bit != 1);
        psw.set_v(false);
        psw.set_c(false);
        psw.set_n(old_bit == 1);
        self.set_psw_flags(psw);
        self.write_mem_word(addr_z, val_z);

    }

    pub fn bset(&mut self, ins: &InstructionInfo) {
        // (op1) ← 1
        //
        // bitaddrQ.q       qF QQ
        let bit_loc = ins.h(0);
        let addr = self.get_word_offset(ins.bytes[1]);
        let mut tmp = self.read_mem_word(addr);

        let bit = tmp >> bit_loc & 0b1;
        tmp |= 1 << bit_loc;
        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(!bit == 1);
        psw.set_v(false);
        psw.set_c(false);
        psw.set_n(bit == 1);
        self.set_psw_flags(psw);

        self.write_mem_word(addr, tmp)
    }

    pub fn calla(&mut self, ins: &InstructionInfo) {
        // CALLA cc, caddr       CA c0 MM MM 
        if cpu_check_jump(ins.h(1), &self.get_psw_flags()) {
            let mut sp = self.get_stack_pointer();
            sp -= 2;
            self.set_stack_pointer(sp);
            self.write_mem_word(sp as usize, self.ip);
            self.ip = ins.u16(2);
        }
    }

    pub fn calls(&mut self, ins: &InstructionInfo) {
        // CALLS seg, caddr     DA SS MM MM
        let seg = ins.bytes[1];
        let ip = ins.u16(2);
        
        let mut sp = self.get_stack_pointer();
        sp -= 2;
        self.write_mem_word(sp as usize, self.get_code_seg_pointer() as u16);
        sp -= 2;
        self.write_mem_word(sp as usize, self.get_instruction_pointer());
        self.set_stack_pointer(sp);
        self.set_code_seg_pointer(seg);
        self.ip = ip;
    }

    pub fn extr(&mut self, ins: &InstructionInfo) {
        // EXTR #irang2         D1 :10##-0 
        let irange = ins.h(1) & 0b0011;
        self.extr_value = irange + 1;
    }

    pub fn exts_or_p(&mut self, ins: &InstructionInfo) {
        let t = ins.bytes[1] >> 6 & 0b11;
        match t {
            0b01 => {
                // EXTP
                let page = match ins.bytes[0] {
                    // EXTP Rwm, #irang2        DC :01##-m
                    0xDC => {
                        let r = self.gprs.get_reg_by_id(ins.l(1));
                        r.get_value_word(&mut self.mem)
                    },
                    // EXTP #pag, #irang2       D7 :01##-0 pp 0:00pp
                    0xD7 => {
                        ins.u16(2)
                    }
                    _ => panic!("Invlid EXTP instruction: {:02X?}", ins)
                } & 0b1111_1111_11; // 10 bits
                let irange = (ins.bytes[1] >> 4 & 0b11) + 1; // ##
                self.extp_page = page;
                self.extp_value = irange;

            },
            0b00 => {
                // EXTS
                let segment = match ins.bytes[0] {
                    // EXTS Rwm, #irang2        DC :01##-m
                    0xDC => {
                        let r = self.gprs.get_reg_by_id(ins.l(1));
                        r.get_value_word(&mut self.mem)
                    },
                    // EXTS #pag, #irang2       D7 :01##-0 ss 0:00ss
                    0xD7 => {
                        ins.u16(2)
                    }
                    _ => panic!("Invlid EXTP instruction: {:02X?}", ins)
                } & 0b1111_1111_11; // 10 bits
                let irange = (ins.bytes[1] >> 4 & 0b11) + 1; // ##
                self.exts_seg = segment;
                self.exts_value = irange;

            },
            _ => panic!("T is {:02b}. Not implemented in EXTS_OR_P. Ins: {:?}", t, ins)
        }
    }

    pub fn mov(&mut self, ins: &InstructionInfo) {
        // (op1) ← (op2)
        let (dest_addr, op2) = match ins.bytes[0] {
            // Rwn, Rwm         F0 nm
            0xF0 => {
                let addr_n = self.gprs.word_reg_by_idx(ins.h(1)).get_addr();
                let value_m = self.gprs.word_reg_by_idx(ins.l(1)).get_value_word(&mut self.mem);
                (addr_n as usize, value_m)
            }
            // Rwn, #data4      E0 #n
            0xE0 => {
                let addr_n = self.gprs.word_reg_by_idx(ins.l(1)).get_addr();
                let data4 = ins.h(1) as u16;
                (addr_n as usize, data4)
            }
            // reg, #data16         E6 RR ## ##
            0xE6 => {
                let addr_reg = self.get_register(ins.bytes[1]).get_addr();
                (addr_reg as usize, ins.u16(2))
            }
            // Rwn, [Rwm]       A8 nm
            0xA8 => {
                let addr_n = self.gprs.word_reg_by_idx(ins.h(1)).get_addr();
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                (addr_n as usize, self.read_mem_word(self.get_mem_address(ptr_m)))
            }
            // Rwn, [Rwm+]      98 nm
            0x98 => {
                let addr_n = self.gprs.word_reg_by_idx(ins.h(1)).get_addr();
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::PostIncrease);
                (addr_n as usize, self.read_mem_word(self.get_mem_address(ptr_m)))
            }
            // [Rwm], Rwn       B8 nm
            0xB8 => {
                let value_n = self.gprs.word_reg_by_idx(ins.h(1)).get_value_word(&mut self.mem);
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                (self.get_mem_address(ptr_m), value_n)
            }
            // [-Rwm], Rwn      88 nm
            0x88 => {
                let value_n = self.gprs.word_reg_by_idx(ins.h(1)).get_value_word(&mut self.mem);
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::PreDecrement);
                (self.get_mem_address(ptr_m), value_n)
            }
            // [Rwn], [Rwm]         C8 nm
            0xC8 => {
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                let value_ptr_m = self.read_mem_word(self.get_mem_address(ptr_m));
                let ptr_n = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.h(1), IndirectType::Normal);
                (self.get_mem_address(ptr_n), value_ptr_m)
            }
            // [Rwn+], [Rwm]        D8 nm
            0xD8 => {
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                let value_ptr_m = self.read_mem_word(self.get_mem_address(ptr_m));
                let ptr_n = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.h(1), IndirectType::PostIncrease);
                (self.get_mem_address(ptr_n), value_ptr_m)
            }
            // [Rwn], [Rwm+]        E8 nm
            0xE8 => {
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::PostIncrease);
                let value_ptr_m = self.read_mem_word(self.get_mem_address(ptr_m));
                let ptr_n = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.h(1), IndirectType::Normal);
                (self.get_mem_address(ptr_n), value_ptr_m)
            }
            // Rwn, [Rwm+#data16]       D4 nm ## ##
            0xD4 => {
                let addr_n = self.gprs.word_reg_by_idx(ins.h(1)).get_addr();
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::PlusConstant(ins.u16(2)));
                (addr_n as usize, self.read_mem_word(self.get_mem_address(ptr_m)))
            }
            // [Rwm+#data16], Rwn       C4 nm ## ##
            0xC4 => {
                let value_n = self.gprs.word_reg_by_idx(ins.h(1)).get_value_word(&mut self.mem);
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::PlusConstant(ins.u16(2)));
                (self.get_mem_address(ptr_m), value_n)
            }
            // [Rwn], mem       84 0n MM MM
            0x84 => {
                let mem = self.get_mem_address(ins.u16(2));
                let value_mem = self.read_mem_word(mem);
                let ptr_reg = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                (self.get_mem_address(ptr_reg), value_mem)
            }
            // mem, [Rwn]       94 0n MM MM
            0x94 => {
                let mem = self.get_mem_address(ins.u16(2));
                let ptr_reg = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                (mem, self.read_mem_word(self.get_mem_address(ptr_reg)))
            }
            // reg, mem         F2 RR MM MM
            0xF2 => {
                let mem = self.get_mem_address(ins.u16(2));
                let value_mem = self.read_mem_word(mem);
                let addr_reg = self.get_register(ins.bytes[1]).get_addr();
                (addr_reg as usize, value_mem)
            }
            // mem, reg         F6 RR MM MM
            0xF6 => {
                let mem = self.get_mem_address(ins.u16(2));
                let value_reg = self.get_register(ins.bytes[1]).get_value_word(&mut self.mem);
                (mem, value_reg)
            }
            _ => panic!("Invalid ins for MOV: {:02X?}", ins)
        };
        
        // Update PSW
        let mut psw = self.get_psw_flags();
        psw.set_e(op2 == 0x8000);
        psw.set_z(op2 == 0);
        psw.set_n(((op2 >> 15) & 0b1) != 0);
        self.set_psw_flags(psw);
        // Write value
        self.write_mem_word(dest_addr, op2)
    }

    pub fn movb(&mut self, ins: &InstructionInfo) {
        // (op1) ← (op2)
        let (dest_addr, op2) = match ins.bytes[0] {
            // Rbn, Rbm         F1 nm
            0xF1 => {
                let addr_n = self.gprs.byte_reg_by_idx(ins.h(1)).get_addr();
                let value_m = self.gprs.byte_reg_by_idx(ins.l(1)).get_byte(&mut self.mem);
                (addr_n as usize, value_m)
            }
            // Rbn, #data4      E1 #n
            0xE1 => {
                let addr_n = self.gprs.byte_reg_by_idx(ins.l(1)).get_addr();
                let data4 = ins.h(1) as u8;
                (addr_n as usize, data4)
            }
            // reg, #data8         E6 RR ## xx
            0xE7 => {
                let addr_reg = self.get_register(ins.bytes[1]).get_addr();
                (addr_reg as usize, ins.bytes[2])
            }
            // Rbn, [Rwm]       A9 nm
            0xA9 => {
                let addr_n = self.gprs.byte_reg_by_idx(ins.h(1)).get_addr();
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                (addr_n as usize, self.read_mem_byte(self.get_mem_address(ptr_m)))
            }
            // Rbn, [Rwm+]      99 nm
            0x99 => {
                let addr_n = self.gprs.byte_reg_by_idx(ins.h(1)).get_addr();
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::PostIncrease);
                (addr_n as usize, self.read_mem_byte(self.get_mem_address(ptr_m)))
            }
            // [Rwm], Rbn       B9 nm
            0xB9 => {
                let value_n = self.gprs.byte_reg_by_idx(ins.h(1)).get_byte(&mut self.mem);
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                (self.get_mem_address(ptr_m), value_n)
            }
            // [-Rwm], Rbn      89 nm
            0x89 => {
                let value_n = self.gprs.byte_reg_by_idx(ins.h(1)).get_byte(&mut self.mem);
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::PreDecrement);
                (self.get_mem_address(ptr_m), value_n)
            }
            // [Rwn], [Rwm]         C9 nm
            0xC9 => {
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                let value_ptr_m = self.read_mem_byte(self.get_mem_address(ptr_m));
                let ptr_n = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.h(1), IndirectType::Normal);
                (self.get_mem_address(ptr_n), value_ptr_m)
            }
            // [Rwn+], [Rwm]        D9 nm
            0xD9 => {
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                let value_ptr_m = self.read_mem_byte(self.get_mem_address(ptr_m));
                let ptr_n = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.h(1), IndirectType::PostIncrease);
                (self.get_mem_address(ptr_n), value_ptr_m)
            }
            // [Rwn], [Rwm+]        E9 nm
            0xE9 => {
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::PostIncrease);
                let value_ptr_m = self.read_mem_byte(self.get_mem_address(ptr_m));
                let ptr_n = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.h(1), IndirectType::Normal);
                (self.get_mem_address(ptr_n), value_ptr_m)
            }
            // Rbn, [Rwm+#data16]       F4 nm ## ##
            0xF4 => {
                let addr_n = self.gprs.byte_reg_by_idx(ins.h(1)).get_addr();
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::PlusConstant(ins.u16(2) & 0xFF));
                (addr_n as usize, self.read_mem_byte(self.get_mem_address(ptr_m)))
            }
            // [Rwm+#data16], Rbn       E4 nm ## ##
            0xE4 => {
                let value_n = self.gprs.byte_reg_by_idx(ins.h(1)).get_byte(&mut self.mem);
                let ptr_m = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::PlusConstant(ins.u16(2) & 0xFF));
                (self.get_mem_address(ptr_m), value_n)
            }
            // [Rwn], mem       A4 0n MM MM
            0xA4 => {
                let mem = self.get_mem_address(ins.u16(2));
                let value_mem = self.read_mem_byte(mem);
                let ptr_reg = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                (self.get_mem_address(ptr_reg), value_mem)
            }
            // mem, [Rwn]       B4 0n MM MM
            0xB4 => {
                let mem = self.get_mem_address(ins.u16(2));
                let ptr_reg = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
                (mem, self.read_mem_byte(self.get_mem_address(ptr_reg)))
            }
            // reg, mem         F3 RR MM MM
            0xF3 => {
                let mem = self.get_mem_address(ins.u16(2));
                let value_mem = self.read_mem_byte(mem);
                let addr_reg = self.get_register(ins.bytes[1]).get_addr();
                (addr_reg as usize, value_mem)
            }
            // mem, reg         F7 RR MM MM
            0xF7 => {
                let mem = self.get_mem_address(ins.u16(2));
                let value_reg = self.get_register(ins.bytes[1]).get_byte(&mut self.mem);
                (mem, value_reg)
            }
            _ => panic!("Invalid ins for MOV: {:02X?}", ins)
        };
        
        // Update PSW
        let mut psw = self.get_psw_flags();
        psw.set_e(op2 == 0x80);
        psw.set_z(op2 == 0);
        psw.set_n(((op2 >> 7) & 0b1) != 0);
        self.set_psw_flags(psw);
        // Write value
        self.write_mem_byte(dest_addr, op2)
    }

    pub fn movbs(&mut self, ins: &InstructionInfo) {
        let (addr_op1, op1, op2) = match ins.bytes[0] {
            // Rwn, Rbm     D0 mn
            0xD0 => {
                let reg_m = self.gprs.byte_reg_by_idx(ins.h(1));
                let reg_n = self.gprs.word_reg_by_idx(ins.l(1));
                (reg_n.get_addr() as usize, reg_n.get_value_word(&mut self.mem), reg_m.get_byte(&mut self.mem))
            },
            // reg, mem     D2 RR MM MM
            0xD2 => {
                let reg = self.get_register(ins.bytes[1]);
                let mm_mm = self.get_mem_address(ins.u16(2));
                (reg.get_addr() as usize, reg.get_value_word(&mut self.mem), self.read_mem_byte(mm_mm))
            },
            // mem, reg     D5 RR MM MM
            0xD5 => {
                let reg = self.get_register(ins.bytes[1]);
                let mm_mm = self.get_mem_address(ins.u16(2));
                (mm_mm, self.read_mem_word(mm_mm), reg.get_byte(&mut self.mem))
            },
            _ => panic!("Invalid instruction for MOVBS: {:02X?}", ins)
        };

        let mut res = op1 & 0xFF00; // Clear low byte
        res |= op2 as u16; // Low byte set
        if op2 >> 7 & 0b1 != 0 {
            res |= 0xFF00;
        } else {
            res &= 0x00FF;
        }

        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(op2 == 0);
        psw.set_n(op2 >> 7 & 0b1 != 0);
        self.set_psw_flags(psw);
        self.write_mem_word(addr_op1, res);
        
    }

    pub fn movbz(&mut self, ins: &InstructionInfo) {
        let (addr_op1, op1, op2) = match ins.bytes[0] {
            // Rwn, Rbm     C0 mn
            0xC0 => {
                let reg_m = self.gprs.byte_reg_by_idx(ins.h(1));
                let reg_n = self.gprs.word_reg_by_idx(ins.l(1));
                (reg_n.get_addr() as usize, reg_n.get_value_word(&mut self.mem), reg_m.get_byte(&mut self.mem))
            },
            // reg, mem     C2 RR MM MM
            0xC2 => {
                let reg = self.get_register(ins.bytes[1]);
                let mm_mm = self.get_mem_address(ins.u16(2));
                (reg.get_addr() as usize, reg.get_value_word(&mut self.mem), self.read_mem_byte(mm_mm))
            },
            // mem, reg     C5 RR MM MM
            0xC5 => {
                let reg = self.get_register(ins.bytes[1]);
                let mm_mm = self.get_mem_address(ins.u16(2));
                (mm_mm, self.read_mem_word(mm_mm), reg.get_byte(&mut self.mem))
            },
            _ => panic!("Invalid instruction for MOVBS: {:02X?}", ins)
        };

        let res = (op1 & 0xFF00) | op2 as u16;

        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_z(op2 == 0);
        psw.set_n(false);
        self.set_psw_flags(psw);
        self.write_mem_word(addr_op1, res);
        
    }

    pub fn neg(&mut self, ins: &InstructionInfo) {
        // (op1) ← 0 - (op1)
        // Rwn      81 n0
        let reg_n = self.gprs.word_reg_by_idx(ins.h(1));
        let mut op1 = reg_n.get_value_word(&mut self.mem);
        let mut o1 = op1 as i16;
        let res = 0 - o1;
        let mut psw = self.get_psw_flags();
        psw.set_e(res == i16::MIN);
        psw.set_z(res == 0);
        psw.set_v(check_underflow(0, o1)); // Underflow
        psw.set_c(check_borrow(0, op1)); // Borrow
        psw.set_n(res >> 15 & 0b1 != 0);
        self.set_psw_flags(psw);
        reg_n.set_value_word(&mut self.mem, res as u16);
    }

    pub fn jb(&mut self, ins: &InstructionInfo) {
        // JB bitaddrQ.q, rel       8A QQ rr q0
        let bitoff = self.get_word_offset(ins.bytes[1]);
        let relative = ins.bytes[2] as i8;
        let bit = ins.h(3);
        if self.read_mem_word(bitoff) >> bit & 0b1 != 0 {
            // JUMP
            if relative > 0 {
                self.ip += relative as u16 * 2;
            } else {
                self.ip -= (relative * -1) as u16 * 2
            }
        }
    }

    pub fn jmpa(&mut self, ins: &InstructionInfo) {
        // JMPA  cc, caddr EA c0    MM MM 
        if cpu_check_jump(ins.h(1), &self.get_psw_flags()) {
            self.ip = ins.u16(2);
        }
    }

    pub fn jmpi(&mut self, ins: &InstructionInfo) {
        // JMPI cc, [Rwn]       9C cn
        if cpu_check_jump(ins.h(1), &self.get_psw_flags()) {
            let addr = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1), IndirectType::Normal);
            self.ip = addr; //self.read_mem_word(self.get_mem_address(addr));
        }
    }
 
    pub fn jmpr(&mut self, ins: &InstructionInfo) {
        // JMPR cc, rel       cD rr
        if cpu_check_jump(ins.h(0), &self.get_psw_flags()) {
            let relative = ins.bytes[1] as i8;
            if relative > 0 {
                self.ip += relative as u16 * 2;
            } else {
                self.ip -= (relative * -1) as u16 * 2
            }
        }
    }

    pub fn jmps(&mut self, ins: &InstructionInfo) {
        // JMPS seg, caddr -- FA SS MM MM 
        let seg = ins.bytes[1];
        let mem = u16::from_le_bytes(ins.bytes[2..].try_into().unwrap());
        self.set_code_seg_pointer(seg);
        self.ip = mem;
    }

    pub fn jnb(&mut self, ins: &InstructionInfo) {
        // JNB bitaddrQ.q, rel      9A QQ rr q0
        let bitoff = self.get_word_offset(ins.bytes[1]);
        let relative = ins.bytes[2] as i8;
        let bit = ins.h(3);
        if self.read_mem_word(bitoff) >> bit & 0b1 == 0 {
            // JUMP
            if relative > 0 {
                self.ip += relative as u16 * 2;
            } else {
                self.ip -= (relative * -1) as u16 * 2
            }
        }
    }

    pub fn pop(&mut self, ins: &InstructionInfo) {
        // POP reg      FC RR
        let mut sp = self.get_stack_pointer();
        let value = self.read_mem_word(sp as usize);
        sp += 2;
        self.set_stack_pointer(sp);

        let mut psw = self.get_psw_flags();
        psw.set_e(value == 0x8000);
        psw.set_z(value == 0);
        psw.set_n(value >> 15 & 0b1 != 0);
        self.set_psw_flags(psw);

        self.get_register(ins.bytes[1]).set_value_word(&mut self.mem, value);
    }

    pub fn push(&mut self, ins: &InstructionInfo) {
        // PUSH reg     EC RR
        let value = self.get_register(ins.bytes[1]).get_value_word(&mut self.mem);
        let mut sp = self.get_stack_pointer();
        sp -= 2;
        self.set_stack_pointer(sp);

        let mut psw = self.get_psw_flags();
        psw.set_e(value == 0x8000);
        psw.set_z(value == 0);
        psw.set_n(value >> 15 & 0b1 != 0);
        self.set_psw_flags(psw);

        self.write_mem_word(sp as usize, value);
    }

    pub fn ret(&mut self, _ins: &InstructionInfo) {
        let mut sp = self.get_stack_pointer();
        self.ip = self.read_mem_word(sp as usize);
        sp += 2;
        self.set_stack_pointer(sp);
    }

    pub fn rets(&mut self, _ins: &InstructionInfo) {
        let mut sp = self.get_stack_pointer();
        self.ip = self.read_mem_word(sp as usize);
        sp += 2;
        let csp = (self.read_mem_word(sp as usize) & 0xFF) as u8;
        self.set_code_seg_pointer(csp);
        sp += 2;
        self.set_stack_pointer(sp);
    }

    pub fn shl(&mut self, ins: &InstructionInfo) {
        let (addr_op1, mut op1, shift_count) = match ins.bytes[0] {
            // Rwn, Rwm     4C nm
            0x4C => {
                let reg_n = self.gprs.word_reg_by_idx(ins.l(1));
                let shift_count = (self.gprs.word_reg_by_idx(ins.h(1)).get_value_word(&mut self.mem) & 0b1111) as u8;
                (reg_n.get_addr(), reg_n.get_value_word(&mut self.mem), shift_count)
            },
            // Rwn, #data4      5C #n
            0x5C => {
                let reg_n = self.gprs.word_reg_by_idx(ins.l(1));
                let shift_count = ins.h(1) & 0b1111;
                (reg_n.get_addr(), reg_n.get_value_word(&mut self.mem), shift_count)
            },
            _ => panic!("Invalid instruction for SHL: {:02X?}", ins)
        };
        let mut psw = self.get_psw_flags();
        psw.set_e(false);
        psw.set_v(false);

        psw.set_c(false);
        let mut n = 1;
        let mut count = shift_count;
        while count != 0 {
            psw.set_c(op1 >> 15 & 0b1 != 0);
            
            op1 &= !(0b1 << n); // Clear old bit
            op1 |= (op1 >> (n-1)) << n; // Set new bit

            op1 &= 0b1111_1110; // Op1_0 <- 0
            count -= 1;
            n += 1;
        }
        psw.set_n(count != 0 && op1 >> 15 != 0);
        self.write_mem_word(addr_op1 as usize, op1);
    }


    // TO HANDLE MULTIPLE INTRUCTIONS
    pub fn logic_op_word(&mut self, ins: &InstructionInfo, ty: LogicOp) {
        // (op1) ← (op1) <LOGIC OP> (op2)

        // Order of match statements
        // ADD | ADDC | AND | CMP | OR | SUB| SUBC
        let (addr_op1, mut op1, op2) = match ins.bytes[0] {
            // ADD Rwn, Rwm         00 nm
            0x00 | 0x10 | 0x60 | 0x40 | 0x70 | 0x20 | 0x30 => {
                let value_m = self.gprs.word_reg_by_idx(ins.l(1)).get_value_word(&mut self.mem);
                let reg_n = self.gprs.word_reg_by_idx(ins.h(1));
                (reg_n.get_addr() as usize, reg_n.get_value_word(&mut self.mem), value_m)
            }
            0x08 | 0x18 | 0x68 | 0x48 | 0x78 | 0x28 | 0x38 => {
                let reg_n = self.gprs.word_reg_by_idx(ins.h(1));
                let reg_n_addr = reg_n.get_addr() as usize;
                let reg_n_value = reg_n.get_value_word(&mut self.mem);
                let ty = ins.l(1) & 0b1100 >> 2;
                let rhs = match ty {
                    // ADD Rwn, [Rwi+]      08 n:11ii
                    0b11 => {
                        let addr = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1) & 0b11, IndirectType::PostIncrease);
                        self.read_mem_word(self.get_mem_address(addr))
                    },
                    // ADD Rwn, [Rwi]       08 n:10ii
                    0b10 => {
                        let addr = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1) & 0b11, IndirectType::Normal);
                        self.read_mem_word(self.get_mem_address(addr))
                    },
                    // ADD Rwn, #data3      08 n:0###
                    _ => {
                        (ins.l(1) & 0b111) as u16
                    }
                };
                (reg_n_addr, reg_n_value, rhs)
            }
            // ADD reg, #data16         06 RR ## ##
            0x06 | 0x16 | 0x66 | 0x46 | 0x76 | 0x26 | 0x36 => {
                let data16 = ins.u16(2);
                let reg = self.get_register(ins.bytes[1]);
                (reg.get_addr() as usize, reg.get_value_word(&mut self.mem), data16)
            }
            // ADD reg, mem         02 RR MM MM
            0x02 | 0x12 | 0x62 | 0x42 | 0x72 | 0x22 | 0x32 => {
                let mem = self.get_mem_address(ins.u16(2));
                let value_at_mem = self.read_mem_word(mem);
                let reg = self.get_register(ins.bytes[1]);
                (reg.get_addr() as usize, reg.get_value_word(&mut self.mem), value_at_mem)
            }
            // ADD mem, reg         04 RR MM MM
            0x04 | 0x14 | 0x64 | 0x74 | 0x24 | 0x34 => { // CMP is skipped
                let mem = self.get_mem_address(ins.u16(2));
                let value_at_mem = self.read_mem_word(mem);
                let reg_value = self.get_register(ins.bytes[1]).get_value_word(&mut self.mem);
                (mem, value_at_mem, reg_value)
            }
            _ => panic!("Invalid logical op ins. {:?} - {:02X?}", ty, ins)
        };

        let mut psw = self.get_psw_flags();
        
        match ty {
            LogicOp::ADD => {
                let o1 = op1 as i16;
                let o2 = op2 as i16;
                let res = o1.wrapping_add(o2);
                psw.set_e(op2 == 0x8000);
                psw.set_z(res == 0);
                psw.set_v(check_overflow(o1, o2));
                psw.set_c(check_carry_sig_bit(op1, op2));
                psw.set_n(res >> 15 & 0b1 != 0);
                op1 = res as u16;
            },
            LogicOp::ADDC => {
                let c = psw.c() as u16;
                let o1 = op1 as i16;
                let o2 = op2 as i16;
                let res = o1.wrapping_add(o2).wrapping_add(c as i16);
                psw.set_e(op2 == 0x8000);
                psw.set_z(res == 0 && psw.z() != 0);
                psw.set_v(check_overflow(o1, o2+c as i16));
                psw.set_c(check_carry_sig_bit(op1, op2+c));
                psw.set_n(res >> 15 & 0b1 != 0);
                op1 = res as u16;
            },
            LogicOp::AND => {
                let res = op1 & op2;
                psw.set_e(op2 == 0x8000);
                psw.set_z(res == 0);
                psw.set_v(false);
                psw.set_c(false);
                psw.set_n(res >> 15 & 0b1 != 0);
                op1 = res as u16;
            },
            LogicOp::CMP | LogicOp::SUB => {
                let o1 = op1 as i16;
                let o2 = op2 as i16;
                let res = o1.wrapping_sub(o2);
                psw.set_e(op2 == 0x8000);
                psw.set_z(res == 0);
                psw.set_v(check_underflow(o1, o2));
                psw.set_c(check_borrow(op1, op2));
                psw.set_n(res >> 15 & 0b1 != 0);
                op1 = res as u16;
            },
            LogicOp::OR => {
                let res = op1 | op2;
                psw.set_e(op2 == 0x8000);
                psw.set_z(res == 0);
                psw.set_v(false);
                psw.set_c(false);
                psw.set_n(res >> 15 & 0b1 != 0);
                op1 = res as u16;
            },
            LogicOp::SUBC => {
                let o1 = op1 as i16;
                let o2 = op2 as i16;
                let res = o1.wrapping_sub(o2).wrapping_sub(psw.c() as i16);
                psw.set_e(op2 == 0x8000);
                psw.set_z(res == 0 && psw.z() == 1);
                psw.set_v(check_underflow(o1, o2+1));
                psw.set_c(check_borrow(op1, op2));
                psw.set_n(res >> 15 & 0b1 != 0);
                op1 = res as u16;
            },
            _ => todo!("Logic op {:?} not implemented", ty)
        };

        
        self.set_psw_flags(psw);
        // Write value
        if ty != LogicOp::CMP {
            self.write_mem_word(addr_op1, op1);
        }

    }

    pub fn logic_op_byte(&mut self, ins: &InstructionInfo, ty: LogicOp) {
        // (op1) ← (op1) <LOGIC OP> (op2)

        // Order of match statements
        // ADDB | CMPB 
        let (addr_op1, mut op1, op2) = match ins.bytes[0] {
            // ADD Rbn, Rbm         01 nm
            0x01 | 0x41 => {
                let value_m = self.gprs.byte_reg_by_idx(ins.l(1)).get_byte(&mut self.mem);
                let reg_n = self.gprs.byte_reg_by_idx(ins.h(1));
                (reg_n.get_addr() as usize, reg_n.get_byte(&mut self.mem), value_m)
            }
            0x09 | 0x49 => {
                let reg_n = self.gprs.byte_reg_by_idx(ins.h(1));
                let reg_n_addr = reg_n.get_addr() as usize;
                let reg_n_value = reg_n.get_byte(&mut self.mem);
                let ty = ins.l(1) & 0b1100 >> 2;
                let rhs = match ty {
                    // ADD Rbn, [Rwi+]      09 n:11ii
                    0b11 => {
                        let addr = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1) & 0b11, IndirectType::PostIncrease);
                        self.read_mem_byte(self.get_mem_address(addr))
                    },
                    // ADD Rbn, [Rwi]       09 n:10ii
                    0b10 => {
                        let addr = self.gprs.rw_get_indirect_addr(&mut self.mem, ins.l(1) & 0b11, IndirectType::Normal);
                        self.read_mem_byte(self.get_mem_address(addr))
                    },
                    // ADD Rbn, #data3      09 n:0###
                    _ => {
                        (ins.l(1) & 0b111) as u8
                    }
                };
                (reg_n_addr, reg_n_value, rhs)
            }
            // ADD reg, #data8         07 RR ## xx
            0x07 | 0x47 => {
                let data8 = ins.bytes[2];
                let reg = self.get_register(ins.bytes[1]);
                (reg.get_addr() as usize, reg.get_byte(&mut self.mem), data8)
            }
            // ADD reg, mem         03 RR MM MM
            0x03 | 0x43 => {
                let mem = self.get_mem_address(ins.u16(2));
                let value_at_mem = self.read_mem_byte(mem);
                let reg = self.get_register(ins.bytes[1]);
                (reg.get_addr() as usize, reg.get_byte(&mut self.mem), value_at_mem)
            }
            // ADD mem, reg         05 RR MM MM
            0x05 => {
                let mem = self.get_mem_address(ins.u16(2));
                let value_at_mem = self.read_mem_byte(mem);
                let reg_value = self.get_register(ins.bytes[1]).get_byte(&mut self.mem);
                (mem, value_at_mem, reg_value)
            }
            _ => panic!("Invalid logical op ins. {:?} - {:02X?}", ty, ins)
        };

        let mut psw = self.get_psw_flags();
        
        match ty {
            LogicOp::ADD => {
                let o1 = op1 as i8;
                let o2 = op2 as i8;
                let res = o1.wrapping_add(o2);
                psw.set_e(op2 == 0x80);
                psw.set_z(res == 0);
                psw.set_v(check_overflowb(o1, o2));
                psw.set_c(check_carry_sig_bitb(op1, op2));
                psw.set_n(res >> 7 & 0b1 != 0);
                op1 = res as u8;
            },
            LogicOp::CMP | LogicOp::SUB => {
                let o1 = op1 as i8;
                let o2 = op2 as i8;
                let res = o1.wrapping_add(o2);
                psw.set_e(op2 == 0x80);
                psw.set_z(res == 0);
                psw.set_v(check_underflowb(o1, o2));
                psw.set_c(check_borrowb(op1, op2));
                psw.set_n(res >> 7 & 0b1 != 0);
                op1 = res as u8;
            },
            _ => todo!("Logic op {:?} not implemented", ty)
        };

        
        self.set_psw_flags(psw);
        // Write value
        if ty != LogicOp::CMP {
            self.write_mem_byte(addr_op1, op1);
        }

    }
}