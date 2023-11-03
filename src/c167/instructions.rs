use super::SEGMENT_SIZE;

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

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instruction {
    ADD,
    ADDB,
    ADDC,
    ADDCB,
    AND,
    ANDB,
    ASHR,

    BAND,
    BCLR,
    BCMP,
    BFLDL,
    BFLDH,
    BMOV,
    BMOVN,
    BOR,
    BSET,
    BXOR,

    CALLA,
    CALLI,
    CALLR,
    CALLS,
    CMP,
    CMPB,
    CMPD1,
    CMPD2,
    CMPL1,
    CMPL2,
    CPL,
    CPLB,

    DISWDT,
    DIV,
    DIVL,
    DIVLU,
    DIVU,

    EINIT,
    EXTR,
    EXTS_OR_P,

    IDLE,

    JB,
    JBC,
    JMPA,
    JMPI,
    JMPR,
    JMPS,
    JNB,
    JNBS,

    MOV,
    MOVBZ,
    MOVBS,
    MOVB,
    MUL,
    MULU,
    

    NEG,
    NEGB,
    NOP,

    OR,
    ORB,

    PCALL,
    POP,
    PRIOR,
    PUSH,
    PWRDN,
    
    RET,
    RETI,
    RETP,
    RETS,
    ROR,

    SCXT,
    SHL,
    SHR,
    SRST,
    SRVWDT,
    SUBC,
    SUBCB,
    SUB,
    SUBB,

    TRAP,

    ROL,

    XOR,
    XORB,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionInfo {
    pub offset: usize,
    pub size: usize,
    pub bytes: Vec<u8>,
    pub instruction: Instruction,
}

impl InstructionInfo {
    pub fn from_bytes(src: &[u8], csp: u8, ip: u16) -> Option<(Self, u16)> {

        let offset = (csp as usize * SEGMENT_SIZE) + ip as usize;

        let ptr = &src[offset..];
        let i = ptr.get(0)?;

        let size = match *i & 0x0F {
            0x02..=0x07 => 4,
            0x0A => 4,
            _ => 2
        };

        let instruction = match *i {
            0x00 => Instruction::ADD,
            0x02 => Instruction::ADD,
            0x04 => Instruction::ADD,
            0x06 => Instruction::ADD,
            0x08 => Instruction::ADD,

            0x01 => Instruction::ADDB,
            0x03 => Instruction::ADDB,
            0x05 => Instruction::ADDB,
            0x07 => Instruction::ADDB,
            0x09 => Instruction::ADDB,

            0x0A => Instruction::BFLDL,
            0x0B => Instruction::MUL,
            0x0C => Instruction::ROL,

            0x10 => Instruction::ADDC,
            0x12 => Instruction::ADDC,
            0x14 => Instruction::ADDC,
            0x16 => Instruction::ADDC,
            0x18 => Instruction::ADDC,

            0x11 => Instruction::ADDCB,
            0x13 => Instruction::ADDCB,
            0x15 => Instruction::ADDCB,
            0x17 => Instruction::ADDCB,
            0x19 => Instruction::ADDCB,
            0x1A => Instruction::BFLDH,
            0x1B => Instruction::MULU,
            0x1C => Instruction::ROL,

            0x20 => Instruction::SUB,
            0x21 => Instruction::SUBB,
            0x22 => Instruction::SUB,
            0x23 => Instruction::SUBB,
            0x24 => Instruction::SUB,
            0x25 => Instruction::SUBB,
            0x26 => Instruction::SUB,
            0x27 => Instruction::SUBB,
            0x28 => Instruction::SUB,
            0x29 => Instruction::SUBB,
            0x2A => Instruction::BCMP,
            0x2B => Instruction::PRIOR,
            0x2C => Instruction::ROR,

            0x30 => Instruction::SUBC,
            0x31 => Instruction::SUBCB,
            0x32 => Instruction::SUBC,
            0x33 => Instruction::SUBCB,
            0x34 => Instruction::SUBC,
            0x35 => Instruction::SUBCB,
            0x36 => Instruction::SUBC,
            0x37 => Instruction::SUBCB,
            0x38 => Instruction::SUBC,
            0x39 => Instruction::SUBCB,
            0x3A => Instruction::BMOVN,
            //0x3B
            0x3C => Instruction::ROR,

            0x40 => Instruction::CMP,
            0x41 => Instruction::CMPB,
            0x42 => Instruction::CMP,
            0x43 => Instruction::CMPB,
            //0x44,
            //0x45,
            0x46 => Instruction::CMP,
            0x47 => Instruction::CMPB,
            0x48 => Instruction::CMP,
            0x49 => Instruction::CMPB,
            0x4A => Instruction::BMOV,
            0x4B => Instruction::DIV,
            0x4C => Instruction::SHL,

            0x50 => Instruction::XOR,
            0x51 => Instruction::XORB,
            0x52 => Instruction::XOR,
            0x53 => Instruction::XORB,
            0x54 => Instruction::XOR,
            0x55 => Instruction::XORB,
            0x56 => Instruction::XOR,
            0x57 => Instruction::XORB,
            0x58 => Instruction::XOR,
            0x59 => Instruction::XORB,
            0x5A => Instruction::BOR,
            0x5B => Instruction::DIVU,
            0x5C => Instruction::SHL,

            0x60 => Instruction::AND,
            0x61 => Instruction::ANDB,
            0x62 => Instruction::AND,
            0x63 => Instruction::ANDB,
            0x64 => Instruction::AND,
            0x65 => Instruction::ANDB,
            0x66 => Instruction::AND,
            0x67 => Instruction::ANDB,
            0x68 => Instruction::AND,
            0x69 => Instruction::ANDB,
            0x6A => Instruction::BAND,
            0x6B => Instruction::DIVL,
            0x6C => Instruction::SHR,

            0x70 => Instruction::OR,
            0x71 => Instruction::ORB,
            0x72 => Instruction::OR,
            0x73 => Instruction::ORB,
            0x74 => Instruction::OR,
            0x75 => Instruction::ORB,
            0x76 => Instruction::OR,
            0x77 => Instruction::ORB,
            0x78 => Instruction::OR,
            0x79 => Instruction::ORB,
            0x7A => Instruction::BXOR,
            0x7B => Instruction::DIVLU,
            0x7C => Instruction::SHR,

            0x80 => Instruction::CMPL1,
            0x81 => Instruction::NEG,
            0x82 => Instruction::CMPL1,
            //0x83
            0x84 => Instruction::MOV,
            //0x85
            0x86 => Instruction::CMPL1,
            0x87 => Instruction::IDLE,
            0x88 => Instruction::MOV,
            0x89 => Instruction::MOVB,
            0x8A => Instruction::JB,
            //0x8B
            //0x8C

            0x90 => Instruction::CMPL2,
            0x91 => Instruction::CPL,
            0x92 => Instruction::CMPL2,
            //0x93
            0x94 => Instruction::MOV,
            //0x95
            0x96 => Instruction::CMPL2,
            0x97 => Instruction::PWRDN,
            0x98 => Instruction::MOV,
            0x99 => Instruction::MOVB,
            0x9A => Instruction::JNB,
            0x9B => Instruction::TRAP,
            0x9C => Instruction::JMPI,

            0xA0 => Instruction::CMPD1,
            0xA1 => Instruction::NEGB,
            0xA2 => Instruction::CMPD1,
            //0xA3
            0xA4 => Instruction::MOVB,
            0xA5 => Instruction::DISWDT,
            0xA6 => Instruction::CMPD1,
            0xA7 => Instruction::SRVWDT,
            0xA8 => Instruction::MOV,
            0xA9 => Instruction::MOVB,
            0xAA => Instruction::JBC,
            0xAB => Instruction::CALLI,
            0xAC => Instruction::ASHR,

            0xB0 => Instruction::CMPD2,
            0xB1 => Instruction::CPLB,
            0xB2 => Instruction::CMPD2,
            //0xB3
            0xB4 => Instruction::MOVB,
            0xB5 => Instruction::EINIT,
            0xB6 => Instruction::CMPD2,
            0xB7 => Instruction::SRST,
            0xB8 => Instruction::MOV,
            0xB9 => Instruction::MOVB,
            0xBA => Instruction::JNBS,
            0xBB => Instruction::CALLR,
            0xBC => Instruction::ASHR,

            0xC0 => Instruction::MOVBZ,
            //0xC1
            0xC2 => Instruction::MOVBZ,
            //0xC3
            0xC4 => Instruction::MOV,
            0xC5 => Instruction::MOVBZ,
            0xC6 => Instruction::SCXT,
            //0xC7
            0xC8 => Instruction::MOV,
            0xC9 => Instruction::MOVB,
            0xCA => Instruction::CALLA,
            0xCB => Instruction::RET,
            0xCC => Instruction::NOP,

            0xD0 => Instruction::MOVBS,
            0xD1 => Instruction::EXTR,
            0xD2 => Instruction::MOVBS,
            //0xD3
            0xD4 => Instruction::MOV,
            0xD5 => Instruction::MOVBS,
            0xD6 => Instruction::SCXT,
            0xD7 => Instruction::EXTS_OR_P,
            0xD8 => Instruction::MOV,
            0xD9 => Instruction::MOVB,
            0xDA => Instruction::CALLS,
            0xDB => Instruction::RETS,
            0xDC => Instruction::EXTS_OR_P,

            0xE0 => Instruction::MOV,
            0xE1 => Instruction::MOVB,
            0xE2 => Instruction::PCALL,
            //0xE3 => Instruction::ORB,
            0xE4 => Instruction::MOVB,
            //0xE5 => Instruction::ORB,
            0xE6 => Instruction::MOV,
            0xE7 => Instruction::MOVB,
            0xE8 => Instruction::MOV,
            0xE9 => Instruction::MOVB,
            0xEA => Instruction::JMPA,
            0xEB => Instruction::RETP,
            0xEC => Instruction::PUSH,

            0xF0 => Instruction::MOV,
            0xF1 => Instruction::MOVB,
            0xF2 => Instruction::MOV,
            0xF3 => Instruction::MOVB,
            0xF4 => Instruction::MOVB,
            //0xF5 => Instruction::ORB,
            0xF6 => Instruction::MOV,
            0xF7 => Instruction::MOVB,
            //0xF8 => Instruction::OR,
            //0xF9 => Instruction::ORB,
            0xFA => Instruction::JMPS,
            0xFB => Instruction::RETI,
            0xFC => Instruction::POP,

            x if x & 0x0F == 0x0D => Instruction::JMPR,
            x if x & 0x0F == 0x0E => Instruction::BCLR,
            x if x & 0x0F == 0x0F => Instruction::BSET,
            x => {
                eprintln!("Unknown Instruction 0x{:02X?}", x);
                return None
            }
        };  
        
        Some((Self {
            offset,
            size: size as usize,
            bytes: ptr[0..size as usize].to_vec(),
            instruction,
        },  ip + size))
    }
}
