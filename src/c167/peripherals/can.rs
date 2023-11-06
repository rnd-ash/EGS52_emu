
const CSR_REG_OFFSET: usize = 0x00;
const IR_REG_OFFSET: usize = 0x02;
const BTR_REG_OFFSET: usize = 0x04;
const GMS_REG_OFFSET: usize = 0x06;
const LGML_REG_OFFSET: usize = 0x08;
const UGML_REG_OFFSET: usize = 0x0A;
const LMLM_REG_OFFSET: usize = 0x0C;
const UMLM_REG_OFFSET: usize = 0x0E;

#[derive(Debug, Clone)]
pub struct CAN {
    base_addr: usize
}

impl CAN {
    pub fn new(base_addr: usize) -> Self {
        Self {
            base_addr
        }
    }
}

fn read_u16(base_addr: usize, off: usize, mem: &[u8]) -> u16 {
    u16::from_le_bytes(mem[base_addr+off..base_addr+off+2].try_into().unwrap())
}

impl super::Peripheral for CAN {
    fn update(&mut self, mem: &mut [u8]) {
        let csr = read_u16(self.base_addr, CSR_REG_OFFSET, &mem);
        let ir = read_u16(self.base_addr, IR_REG_OFFSET, &mem);
        let btr = read_u16(self.base_addr, BTR_REG_OFFSET, &mem);
        let gms = read_u16(self.base_addr, GMS_REG_OFFSET, &mem);
    }
}