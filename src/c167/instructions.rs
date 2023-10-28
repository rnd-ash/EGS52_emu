
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OperandTy {
    // Word register
    Rw,
    // Byte register
    Rb,
    // Register in SFR/ESFR
    Reg(u8),
    // Memory
    Mem,
    CC,
    Caddr(u16),
    Seg(u8),
    // Raw data (16 bits)
    Data16(u16),
    Data8,
    Data4,
    Data3,
    Mask8,
    BitOffset,
    RelOffset(i16)
}

impl OperandTy {
    pub fn rel_offset(mem: &[u8]) -> Self {
        let x = i16::from_le_bytes(mem[0..2].try_into().unwrap());
        Self::RelOffset(x)
    }

    pub fn reg(mem: &[u8]) -> Self {
        Self::Reg(mem[0])
    }

    pub fn seg(mem: &[u8]) -> Self {
        Self::Seg(mem[0])
    }

    pub fn caddr(mem: &[u8]) -> Self {
        Self::Caddr(u16::from_le_bytes(mem[0..2].try_into().unwrap()))
    }

    pub fn data16(mem: &[u8]) -> Self {
        Self::Data16(u16::from_le_bytes(mem[0..2].try_into().unwrap()))
    }
}

impl OperandTy {
    pub fn get_size_bytes(&self) -> usize {
        match self {
            Self::Mem => 2,
            Self::Data16(_) => 2,
            Self::RelOffset(_) => 2,
            Self::Caddr(_) => 2,
            _ => 1
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum JumpType {
    UC,
    Z,
    NZ,
    V,
    NV,
    N,
    NN,
    C,
    NC,
    EQ,
    NE,
    ULT,
    ULE,
    UGE,
    UGT,
    SLE,
    SGE,
    SGT,
    NET
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instruction {
    MOV,
    ADD,
    ADDB,
    MUL,
    ROL,
    JMPA,
    JMPS,
    JMPR,
    BCLR,
    BSET,
    BFLDL,
    BFLDH,
    MULU,
    SUB,
    SUBB,
    BCMP,
    PRIOR,
    ROR,
    SUBC,
    SUBCB,
    BMOVN,
    CMP,
    XOR,
    AND,
    OR,
    CMPB,
    XORB,
    ANDB,
    SHL,
    SHR,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionInfo {
    pub size: usize,
    pub instruction_raw: Vec<u8>,
    pub instruction: Instruction,
    pub operands: Vec<OperandTy>
}

impl InstructionInfo {
    pub fn from_bytes(src: &[u8], offset: usize) -> Option<(Self, usize)> {

        let ptr = &src[offset..];

        let i = ptr.get(0)?;
        let (instruction, operands) = match *i {
            0x00 => (Instruction::ADD, vec![OperandTy::Rw, OperandTy::Rw]),
            0xE6 => (Instruction::MOV, vec![OperandTy::reg(&ptr[1..]), OperandTy::data16(&ptr[2..])]),
            x if x & 0x0F == 0x0D => { (Instruction::JMPR, vec![OperandTy::rel_offset(&ptr[1..])]) }
            x if x & 0x0F == 0x0E => { (Instruction::BCLR, vec![OperandTy::BitOffset]) }
            x if x & 0x0F == 0x0F => { (Instruction::BSET, vec![OperandTy::BitOffset]) }

            0xEA => (Instruction::JMPA, vec![ OperandTy::CC, OperandTy::caddr(&ptr[2..]) ]),
            0xFA => (Instruction::JMPS, vec![ OperandTy::seg(&ptr[1..]), OperandTy::caddr(&ptr[2..]) ]),

            _ => return None
        };

        let mut size = 1;
        for o in &operands {
            size += o.get_size_bytes();
        }
        Some((Self {
            size,
            instruction_raw: ptr[0..size].to_vec(),
            instruction,
            operands,
        }, offset + size))
    }
}
