use std::{cell::RefCell, rc::Rc};

use super::register::Register;


const GPR_REG_BYTE_NAMES: &'static [&'static str] = &[
    "RL0",
    "RH0",
    "RL1",
    "RH1",
    "RL2",
    "RH2",
    "RL3",
    "RH3",
    "RL4",
    "RH4",
    "RL5",
    "RH5",
    "RL6",
    "RH6",
    "RL7",
    "RH7"
];

const GPR_REG_WORD_NAMES: &'static [&'static str] = &[
    "R0",
    "R1",
    "R2",
    "R3",
    "R4",
    "R5",
    "R6",
    "R7",
    "R8",
    "R9",
    "R10",
    "R11",
    "R12",
    "R13",
    "R14",
    "R15"
];

#[derive(Debug, Clone)]
pub struct GPRData {
    pub addr: u16,
    pub words: [Register; 16],
    pub bytes: [Register; 16]
}

impl GPRData {
    pub fn new(mem: Rc<RefCell<[u8; 0xFFFF]>>) -> Self {
        Self {
            bytes: [
                Register::new("RL0", None, 0xFC00, mem.clone()),
                Register::new("RH0", None, 0xFC01, mem.clone()),
                Register::new("RL1", None, 0xFC02, mem.clone()),
                Register::new("RH1", None, 0xFC03, mem.clone()),
                Register::new("RL2", None, 0xFC04, mem.clone()),
                Register::new("RH2", None, 0xFC05, mem.clone()),
                Register::new("RL3", None, 0xFC06, mem.clone()),
                Register::new("RH3", None, 0xFC07, mem.clone()),
                Register::new("RL4", None, 0xFC18, mem.clone()),
                Register::new("RH4", None, 0xFC19, mem.clone()),
                Register::new("RL5", None, 0xFC1A, mem.clone()),
                Register::new("RH5", None, 0xFC1B, mem.clone()),
                Register::new("RL6", None, 0xFC1C, mem.clone()),
                Register::new("RH6", None, 0xFC1D, mem.clone()),
                Register::new("RL7", None, 0xFC1E, mem.clone()),
                Register::new("RH7", None, 0xFC1F, mem.clone())
            ],
            words: [
                Register::new("R0", None, 0xFC00, mem.clone()),
                Register::new("R1", None, 0xFC02, mem.clone()),
                Register::new("R2", None, 0xFC04, mem.clone()),
                Register::new("R3", None, 0xFC06, mem.clone()),
                Register::new("R4", None, 0xFC08, mem.clone()),
                Register::new("R5", None, 0xFC0A, mem.clone()),
                Register::new("R6", None, 0xFC0C, mem.clone()),
                Register::new("R7", None, 0xFC0E, mem.clone()),
                Register::new("R8", None, 0xFC10, mem.clone()),
                Register::new("R9", None, 0xFC12, mem.clone()),
                Register::new("R10", None, 0xFC14, mem.clone()),
                Register::new("R11", None, 0xFC16, mem.clone()),
                Register::new("R12", None, 0xFC18, mem.clone()),
                Register::new("R13", None, 0xFC1A, mem.clone()),
                Register::new("R14", None, 0xFC1C, mem.clone()),
                Register::new("R15", None, 0xFC1E, mem.clone())
            ],
            addr: 0xFC00
        }
    }

    pub fn get_reg_by_id<'a>(&'a mut self, rid: u8) -> &'a mut Register {
        let idx = (rid & 0x0F) as usize;
        &mut self.bytes[idx]
    }

    pub fn byte_reg_by_idx<'a>(&'a mut self, id: u8) -> &'a mut Register {
        &mut self.bytes[id as usize]
    }

    pub fn word_reg_by_idx<'a>(&'a mut self, id: u8) -> &'a mut Register {
        &mut self.words[id as usize]
    }

    pub fn set_context_pointer(&mut self, cp: u16) {
        // Reload registers
        let mut x = 0;
        self.addr = cp;
        for f in 0..16 {
            self.words[f].set_addr(cp + x);
            self.bytes[f].set_addr(cp + (x/2));
            x += 2;
        }
    }
}