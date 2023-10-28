use self::instructions::{InstructionInfo, OperandTy};

mod instructions;

///! Infineon C167 emulator


pub struct C167 {
    current_offset: usize,
    flash: Vec<u8>,
    
}

impl C167 {

    pub fn new(flash: Vec<u8>) -> Self {
        let mut f = Self {
            current_offset: 0x80000,
            flash: vec![0; 0x80000]
        };
        f.flash.extend_from_slice(&flash);
        f
    }

    pub fn run(&mut self) {
        while let Some((instruction, new_offset)) = InstructionInfo::from_bytes(&self.flash, self.current_offset) {
            println!("{:08X?} - {:02X?}", self.current_offset, instruction);
            self.current_offset = new_offset;
            self.process_instruction(instruction)
        }
    }

    pub fn process_instruction(&mut self, ins: InstructionInfo) {
        match ins.instruction {
            instructions::Instruction::MOV => todo!(),
            instructions::Instruction::ADD => todo!(),
            instructions::Instruction::ADDB => todo!(),
            instructions::Instruction::MUL => todo!(),
            instructions::Instruction::ROL => todo!(),
            instructions::Instruction::JMPA => self.jumpa(ins),
            instructions::Instruction::JMPS => self.jumps(ins),
            instructions::Instruction::JMPR => self.jmp(ins),
            instructions::Instruction::BCLR => todo!(),
            instructions::Instruction::BSET => todo!(),
            instructions::Instruction::BFLDL => todo!(),
            instructions::Instruction::BFLDH => todo!(),
            instructions::Instruction::MULU => todo!(),
            instructions::Instruction::SUB => todo!(),
            instructions::Instruction::SUBB => todo!(),
            instructions::Instruction::BCMP => todo!(),
            instructions::Instruction::PRIOR => todo!(),
            instructions::Instruction::ROR => todo!(),
            instructions::Instruction::SUBC => todo!(),
            instructions::Instruction::SUBCB => todo!(),
            instructions::Instruction::BMOVN => todo!(),
            instructions::Instruction::CMP => todo!(),
            instructions::Instruction::XOR => todo!(),
            instructions::Instruction::AND => todo!(),
            instructions::Instruction::OR => todo!(),
            instructions::Instruction::CMPB => todo!(),
            instructions::Instruction::XORB => todo!(),
            instructions::Instruction::ANDB => todo!(),
            instructions::Instruction::SHL => todo!(),
            instructions::Instruction::SHR => todo!(),
        }
    }


    pub fn jumpa(&mut self, ins: InstructionInfo) {
        // CC, CADDR
        assert!(ins.instruction_raw.len() == 4);
        let cc = ins.instruction_raw[1];
        let caddr = u16::from_le_bytes(ins.instruction_raw[2..4].try_into().unwrap());
    }

    pub fn jumps(&mut self, ins: InstructionInfo) {
        // SEG, CADDR
        assert!(ins.instruction_raw.len() == 4);
        let seg = ins.instruction_raw[1];
        let caddr = u16::from_le_bytes(ins.instruction_raw[2..4].try_into().unwrap());
        let offset = (0x10000u32*(seg as u32)) + caddr as u32;
        self.current_offset = offset as usize;
    }

    pub fn jmp(&mut self, ins: InstructionInfo) {
        if let OperandTy::RelOffset(x) = ins.operands[0] {
            self.current_offset = self.current_offset.wrapping_add_signed(x as isize);
        } else {
            panic!("JMP instruction, but operand was {:?}", ins.operands)
        }
    }
}