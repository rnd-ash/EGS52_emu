use std::ops::RangeInclusive;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Memory {
    pub (crate) raw: [u8; 0x10000],
    last_write_start: u16,
    last_write_end: u16,
    
    last_read_start: u16,
    last_read_end: u16,
}

impl Memory {
    pub fn new(eeprom: Vec<u8>) -> Self {
        let mut s = Self {
            raw: [0; 0x10000],
            last_write_start: 0,
            last_write_end: 0,
            last_read_start: 0,
            last_read_end: 0,
        };
        // EEPROM is mapped in memory
        s.raw[0..eeprom.len()].copy_from_slice(&eeprom);
        s
    }

    pub fn raw_access<T: FnMut(&mut [u8])>(&mut self, mut f: T) {
        f(&mut self.raw)
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
        if (self.last_read_start..=self.last_read_end).contains(&0xEF00) {
            // CAN - Reading the upper half of the Control Register (status partition) will clear the
            //       Status Change Interrupt value in the Interrupt Register
            self.raw[0xEF02] = 0x00; // Clear interrupt value
            
        }
        if (self.last_read_start..=self.last_read_end).contains(&0xEE00) {
            // CAN - Reading the upper half of the Control Register (status partition) will clear the
            //       Status Change Interrupt value in the Interrupt Register
            self.raw[0xEE02] = 0x00; // Clear interrupt value
            
        }
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

    pub fn set_word_buffer_no_tracking(&mut self, addr: u16, v: [u8; 2]) {
        self.raw[addr as usize..addr as usize + 2].copy_from_slice(&v);
    }

}