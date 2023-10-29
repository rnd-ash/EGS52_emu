use std::{borrow::BorrowMut, cell::{RefCell, Ref}, rc::Rc};

#[derive(Debug)]
pub struct Register {
    pub name: &'static str,
    addr_16: u16,
    mem_buffer: Rc<RefCell<[u8; 0xFFFF]>>
}



impl Register {
    pub fn new(name: &'static str, inital_value: Option<u16>, addr_16: u16, mem: Rc<RefCell<[u8; 0xFFFF]>>) -> Self {
        let mut v = Self {
            name,
            addr_16,
            mem_buffer: mem.clone(),
        };
        if let Some(reset_value) = inital_value {
            v.set_value(reset_value.to_le_bytes());
        }
        v
    }

    pub fn get_raw(&self) -> [u8; 2] {
        let b = self.mem_buffer.borrow();
        b.as_ref()[self.addr_16 as usize..self.addr_16 as usize +2].try_into().unwrap()
    }

    pub fn set_value(&mut self, v: [u8; 2]) {
        self.mem_buffer.borrow_mut().replace_with(|mut old| {
            old[self.addr_16 as usize..self.addr_16 as usize +2].copy_from_slice(&v);
            *old
        });
        
    }

    pub fn get_value_u16(&self) -> u16 {
        u16::from_le_bytes(self.get_raw().try_into().unwrap())
    }

    pub fn get_value_i16(&self) -> i16 {
        i16::from_le_bytes(self.get_raw().try_into().unwrap())
    }

    pub fn add_signed(&mut self, v: i16) {
        let mut old_value = self.get_value_i16();
        old_value = old_value.wrapping_add(v);
        self.set_value(old_value.to_le_bytes());
    }

    pub fn add_unsigned(&mut self, v: u16) {
        let mut old_value = self.get_value_u16();
        old_value = old_value.wrapping_add(v);
        self.set_value(old_value.to_le_bytes());
    }
}

pub fn sfr_registers<'a>(mem: Rc<RefCell<[u8; 0xFFFF]>>) -> [Option<Register>; 256] {
    [
        Some(Register::new("DPP0", Some(0x0000), 0xFE00, mem.clone())), // 0xFE00
        Some(Register::new("DPP1", Some(0x0001), 0xFE02, mem.clone())), // 0xFE02
        Some(Register::new("DPP2", Some(0x0002), 0xFE04, mem.clone())), // 0xFE04
        Some(Register::new("DPP3", Some(0x0003), 0xFE06, mem.clone())), // 0xFE06
        None, // 0xFE08
        None, // 0xFE0A
        Some(Register::new("MDH", Some(0x0000), 0xFE0C, mem.clone())), // 0xFE0C
        Some(Register::new("MDL", Some(0x0000), 0xFE0E, mem.clone())), // 0xFE0E

        Some(Register::new("CP", Some(0xFC00), 0xFE10, mem.clone())), // 0xFE10
        Some(Register::new("SP", Some(0xFC00), 0xFE12, mem.clone())), // 0xFE12
        Some(Register::new("STKOV", Some(0xFA00), 0xFE14, mem.clone())), // 0xFE14
        Some(Register::new("STKUN", Some(0xFC00), 0xFE16, mem.clone())), // 0xFE16
        Some(Register::new("ADDRSEL1", Some(0x0000), 0xFE18, mem.clone())), // 0xFE18
        Some(Register::new("ADDRSEL2", Some(0x0000), 0xFE1A, mem.clone())), // 0xFE1A
        Some(Register::new("ADDRSEL3", Some(0x0000), 0xFE1C, mem.clone())), // 0xFE1C
        Some(Register::new("ADDRSEL4", Some(0x0000), 0xFE1E, mem.clone())), // 0xFE1E
        
        None, // 0xFE20
        None, // 0xFE22
        None, // 0xFE24
        None, // 0xFE26
        None, // 0xFE28
        None, // 0xFE2A
        None, // 0xFE2C
        None, // 0xFE2E

        Some(Register::new("PW0", Some(0x0000), 0xFE30, mem.clone())), // 0xFE30
        Some(Register::new("PW1", Some(0x0000), 0xFE32, mem.clone())), // 0xFE32
        Some(Register::new("PW2", Some(0x0000), 0xFE34, mem.clone())), // 0xFE34
        Some(Register::new("PW3", Some(0x0000), 0xFE36, mem.clone())), // 0xFE36
        None, // 0xFE38
        None, // 0xFE3A
        None, // 0xFE3C
        None, // 0xFE3E

        Some(Register::new("T2", Some(0x0000), 0xFE40, mem.clone())), // 0xFE40
        Some(Register::new("T3", Some(0x0000), 0xFE42, mem.clone())), // 0xFE42
        Some(Register::new("T4", Some(0x0000), 0xFE44, mem.clone())), // 0xFE44
        Some(Register::new("T5", Some(0x0000), 0xFE46, mem.clone())), // 0xFE46
        Some(Register::new("T6", Some(0x0000), 0xFE48, mem.clone())), // 0xFE48
        Some(Register::new("CAPREL", Some(0x0000), 0xFE4A, mem.clone())), // 0xFE4A
        None, // 0xFE4C
        None, // 0xFE4E

        Some(Register::new("T0", Some(0x0000), 0xFE50, mem.clone())), // 0xFE50
        Some(Register::new("T1", Some(0x0000), 0xFE52, mem.clone())), // 0xFE52
        Some(Register::new("T0REL", Some(0x0000), 0xFE54, mem.clone())), // 0xFE54
        Some(Register::new("T1REL", Some(0x0000), 0xFE56, mem.clone())), // 0xFE56
        None, // 0xFE58
        None, // 0xFE5A
        None, // 0xFE5C
        None, // 0xFE5E

        Some(Register::new("CC16", Some(0x0000), 0xFE60, mem.clone())), // 0xFE60
        Some(Register::new("CC17", Some(0x0000), 0xFE62, mem.clone())), // 0xFE62
        Some(Register::new("CC18", Some(0x0000), 0xFE64, mem.clone())), // 0xFE64
        Some(Register::new("CC19", Some(0x0000), 0xFE66, mem.clone())), // 0xFE66
        Some(Register::new("CC20", Some(0x0000), 0xFE68, mem.clone())), // 0xFE68
        Some(Register::new("CC21", Some(0x0000), 0xFE6A, mem.clone())), // 0xFE6A
        Some(Register::new("CC22", Some(0x0000), 0xFE6C, mem.clone())), // 0xFE6C
        Some(Register::new("CC23", Some(0x0000), 0xFE6E, mem.clone())), // 0xFE6E

        Some(Register::new("CC24", Some(0x0000), 0xFE70, mem.clone())), // 0xFE70
        Some(Register::new("CC25", Some(0x0000), 0xFE72, mem.clone())), // 0xFE72
        Some(Register::new("CC26", Some(0x0000), 0xFE74, mem.clone())), // 0xFE74
        Some(Register::new("CC27", Some(0x0000), 0xFE76, mem.clone())), // 0xFE76
        Some(Register::new("CC28", Some(0x0000), 0xFE78, mem.clone())), // 0xFE78
        Some(Register::new("CC29", Some(0x0000), 0xFE7A, mem.clone())), // 0xFE7A
        Some(Register::new("CC30", Some(0x0000), 0xFE7C, mem.clone())), // 0xFE7C
        Some(Register::new("CC31", Some(0x0000), 0xFE7E, mem.clone())), // 0xFE7E

        Some(Register::new("CC0", Some(0x0000), 0xFE80, mem.clone())), // 0xFE80
        Some(Register::new("CC1", Some(0x0000), 0xFE82, mem.clone())), // 0xFE82
        Some(Register::new("CC2", Some(0x0000), 0xFE84, mem.clone())), // 0xFE84
        Some(Register::new("CC3", Some(0x0000), 0xFE86, mem.clone())), // 0xFE86
        Some(Register::new("CC4", Some(0x0000), 0xFE88, mem.clone())), // 0xFE88
        Some(Register::new("CC5", Some(0x0000), 0xFE8A, mem.clone())), // 0xFE8A
        Some(Register::new("CC6", Some(0x0000), 0xFE8C, mem.clone())), // 0xFE8C
        Some(Register::new("CC7", Some(0x0000), 0xFE8E, mem.clone())), // 0xFE8E

        Some(Register::new("CC8", Some(0x0000), 0xFE90, mem.clone())), // 0xFE90
        Some(Register::new("CC9", Some(0x0000), 0xFE92, mem.clone())), // 0xFE92
        Some(Register::new("CC10", Some(0x0000), 0xFE94, mem.clone())), // 0xFE94
        Some(Register::new("CC11", Some(0x0000), 0xFE96, mem.clone())), // 0xFE96
        Some(Register::new("CC12", Some(0x0000), 0xFE98, mem.clone())), // 0xFE98
        Some(Register::new("CC13", Some(0x0000), 0xFE9A, mem.clone())), // 0xFE9A
        Some(Register::new("CC14", Some(0x0000), 0xFE9C, mem.clone())), // 0xFE9C
        Some(Register::new("CC15", Some(0x0000), 0xFE9E, mem.clone())), // 0xFE9E

        Some(Register::new("ADDAT", Some(0x0000), 0xFEA0, mem.clone())), // 0xFEA0
        None, // 0xFEA2
        Some(Register::new("P1DIDIS", Some(0x0000), 0xFEA4, mem.clone())), // 0xFEA4
        None, // 0xFEA6
        None, // 0xFEA8
        None, // 0xFEAA
        None, // 0xFEAC
        Some(Register::new("WDT", Some(0x0000), 0xFEAE, mem.clone())), // 0xFEAE

        Some(Register::new("S0TBUF", Some(0x0000), 0xFEB0, mem.clone())), // 0xFEB0
        Some(Register::new("S0RBUF", Some(0x0000), 0xFEB2, mem.clone())), // 0xFEB2
        Some(Register::new("S0BG", Some(0x0000), 0xFEB4, mem.clone())), // 0xFEB4
        None, // 0xFEA6
        None, // 0xFEA8
        None, // 0xFEAA
        None, // 0xFEAC
        None, // 0xFEAE

        Some(Register::new("PECC0", Some(0x0000), 0xFEC0, mem.clone())), // 0xFEC0
        Some(Register::new("PECC1", Some(0x0000), 0xFEC2, mem.clone())), // 0xFEC2
        Some(Register::new("PECC2", Some(0x0000), 0xFEC4, mem.clone())), // 0xFEC4
        Some(Register::new("PECC3", Some(0x0000), 0xFEC6, mem.clone())), // 0xFEC6
        Some(Register::new("PECC4", Some(0x0000), 0xFEC8, mem.clone())), // 0xFEC8
        Some(Register::new("PECC5", Some(0x0000), 0xFECA, mem.clone())), // 0xFECA
        Some(Register::new("PECC6", Some(0x0000), 0xFECC, mem.clone())), // 0xFECC
        Some(Register::new("PECC7", Some(0x0000), 0xFECE, mem.clone())), // 0xFECE

        None, // 0xFED0
        None, // 0xFED2
        None, // 0xFED4
        None, // 0xFED6
        None, // 0xFED8
        None, // 0xFEDA
        None, // 0xFEDC
        None, // 0xFEDE

        None, // 0xFEE0
        None, // 0xFEE2
        None, // 0xFEE4
        None, // 0xFEE6
        None, // 0xFEE8
        None, // 0xFEEA
        None, // 0xFEEC
        None, // 0xFEEE

        None, // 0xFEF0
        None, // 0xFEF2
        None, // 0xFEF4
        None, // 0xFEF6
        None, // 0xFEF8
        None, // 0xFEFA
        None, // 0xFEFC
        None, // 0xFEFE

        Some(Register::new("P0L", Some(0x0000), 0xFF00, mem.clone())), // 0xFF00
        Some(Register::new("P0H", Some(0x0000), 0xFF02, mem.clone())), // 0xFF02
        Some(Register::new("P1L", Some(0x0000), 0xFF04, mem.clone())), // 0xFF04
        Some(Register::new("P1H", Some(0x0000), 0xFF06, mem.clone())), // 0xFF06
        None, // 0xFF08
        None, // 0xFF0A
        Some(Register::new("BUSCON0", Some(0x0000), 0xFF0C, mem.clone())), // 0xFF0C
        Some(Register::new("MDC", Some(0x0000), 0xFF0E, mem.clone())), // 0xFF0E

        Some(Register::new("PSW", Some(0x0000), 0xFF10, mem.clone())), // 0xFF10
        Some(Register::new("SYSCON", Some(0x0000), 0xFF12, mem.clone())), // 0xFF12
        Some(Register::new("BUSCON1", Some(0x0000), 0xFF14, mem.clone())), // 0xFF14
        Some(Register::new("BUSCON2", Some(0x0000), 0xFF16, mem.clone())), // 0xFF16
        Some(Register::new("BUSCON3", Some(0x0000), 0xFF18, mem.clone())), // 0xFF18
        Some(Register::new("BUSCON4", Some(0x0000), 0xFF1A, mem.clone())), // 0xFF1A
        Some(Register::new("ZEROS", Some(0x0000), 0xFF1C, mem.clone())), // 0xFF1C
        Some(Register::new("ONES", Some(0xFFFF), 0xFF1E, mem.clone())), // 0xFF1E

        Some(Register::new("T78CON", Some(0x0000), 0xFF20, mem.clone())), // 0xFF20
        Some(Register::new("CCM4", Some(0x0000), 0xFF22, mem.clone())), // 0xFF22
        Some(Register::new("CCM5", Some(0x0000), 0xFF24, mem.clone())), // 0xFF24
        Some(Register::new("CCM6", Some(0x0000), 0xFF26, mem.clone())), // 0xFF26
        Some(Register::new("CCM7", Some(0x0000), 0xFF28, mem.clone())), // 0xFF28
        None, // 0xFF2A
        None, // 0xFF2C
        None, // 0xFF2E

        Some(Register::new("PWMCON0", Some(0x0000), 0xFF30, mem.clone())), // 0xFF30
        Some(Register::new("PWMCON1", Some(0x0000), 0xFF32, mem.clone())), // 0xFF32
        None, // 0xFF34
        None, // 0xFF36
        None, // 0xFF38
        None, // 0xFF3A
        None, // 0xFF3C
        None, // 0xFF3E

        Some(Register::new("T2CON", Some(0x0000), 0xFF40, mem.clone())), // 0xFF40
        Some(Register::new("T3CON", Some(0x0000), 0xFF42, mem.clone())), // 0xFF42
        Some(Register::new("T4CON", Some(0x0000), 0xFF44, mem.clone())), // 0xFF44
        Some(Register::new("T5CON", Some(0x0000), 0xFF46, mem.clone())), // 0xFF46
        Some(Register::new("T6CON", Some(0x0000), 0xFF48, mem.clone())), // 0xFF48
        None, // 0xFF4A
        None, // 0xFF4C
        None, // 0xFF4E

        Some(Register::new("T01CON", Some(0x0000), 0xFF50, mem.clone())), // 0xFF50
        Some(Register::new("CCM0", Some(0x0000), 0xFF52, mem.clone())), // 0xFF52
        Some(Register::new("CCM1", Some(0x0000), 0xFF54, mem.clone())), // 0xFF54
        Some(Register::new("CCM2", Some(0x0000), 0xFF56, mem.clone())), // 0xFF56
        Some(Register::new("CCM3", Some(0x0000), 0xFF58, mem.clone())), // 0xFF58
        None, // 0xFF5A
        None, // 0xFF5C
        None, // 0xFF5E

        Some(Register::new("T2IC", Some(0x0000), 0xFF60, mem.clone())), // 0xFF60
        Some(Register::new("T3IC", Some(0x0000), 0xFF62, mem.clone())), // 0xFF62
        Some(Register::new("T4IC", Some(0x0000), 0xFF64, mem.clone())), // 0xFF64
        Some(Register::new("T5IC", Some(0x0000), 0xFF66, mem.clone())), // 0xFF66
        Some(Register::new("T6IC", Some(0x0000), 0xFF68, mem.clone())), // 0xFF68
        Some(Register::new("CRIC", Some(0x0000), 0xFF6A, mem.clone())), // 0xFF6A
        Some(Register::new("S0TIC", Some(0x0000), 0xFF6C, mem.clone())), // 0xFF6C
        Some(Register::new("S0RIC", Some(0x0000), 0xFF6E, mem.clone())), // 0xFF6E

        Some(Register::new("S0EIC", Some(0x0000), 0xFF70, mem.clone())), // 0xFF70
        Some(Register::new("SSCTIC", Some(0x0000), 0xFF72, mem.clone())), // 0xFF72
        Some(Register::new("SSCRIC", Some(0x0000), 0xFF74, mem.clone())), // 0xFF74
        Some(Register::new("SSCEIC", Some(0x0000), 0xFF76, mem.clone())), // 0xFF76
        Some(Register::new("CC0IC", Some(0x0000), 0xFF78, mem.clone())), // 0xFF78
        Some(Register::new("CC1IC", Some(0x0000), 0xFF7A, mem.clone())), // 0xFF7A
        Some(Register::new("CC2IC", Some(0x0000), 0xFF7C, mem.clone())), // 0xFF7C
        Some(Register::new("CC3IC", Some(0x0000), 0xFF7E, mem.clone())), // 0xFF7E

        Some(Register::new("CC4IC", Some(0x0000), 0xFF80, mem.clone())), // 0xFF80
        Some(Register::new("CC5IC", Some(0x0000), 0xFF82, mem.clone())), // 0xFF82
        Some(Register::new("CC6IC", Some(0x0000), 0xFF84, mem.clone())), // 0xFF84
        Some(Register::new("CC7IC", Some(0x0000), 0xFF86, mem.clone())), // 0xFF86
        Some(Register::new("CC8IC", Some(0x0000), 0xFF88, mem.clone())), // 0xFF88
        Some(Register::new("CC9IC", Some(0x0000), 0xFF8A, mem.clone())), // 0xFF8A
        Some(Register::new("CC10IC", Some(0x0000), 0xFF8C, mem.clone())), // 0xFF8C
        Some(Register::new("CC11IC", Some(0x0000), 0xFF8E, mem.clone())), // 0xFF8E

        Some(Register::new("CC12IC", Some(0x0000), 0xFF90, mem.clone())), // 0xFF90
        Some(Register::new("CC13IC", Some(0x0000), 0xFF92, mem.clone())), // 0xFF92
        Some(Register::new("CC14IC", Some(0x0000), 0xFF94, mem.clone())), // 0xFF94
        Some(Register::new("CC15IC", Some(0x0000), 0xFF96, mem.clone())), // 0xFF96
        Some(Register::new("ADCIC", Some(0x0000), 0xFF98, mem.clone())), // 0xFF98
        Some(Register::new("ADEIC", Some(0x0000), 0xFF9A, mem.clone())), // 0xFF9A
        Some(Register::new("T0IC", Some(0x0000), 0xFF9C, mem.clone())), // 0xFF9C
        Some(Register::new("T1IC", Some(0x0000), 0xFF9E, mem.clone())), // 0xFF9E

        Some(Register::new("ADCON", Some(0x0000), 0xFFA0, mem.clone())), // 0xFFA0
        Some(Register::new("P5", Some(0x0000), 0xFFA2, mem.clone())), // 0xFFA2
        Some(Register::new("P5DIDIS", Some(0x0000), 0xFFA4, mem.clone())), // 0xFFA4
        None, // 0xFFA6
        None, // 0xFFA8
        Some(Register::new("F0CON", Some(0x0000), 0xFFAA, mem.clone())), // 0xFFAA
        Some(Register::new("TFR", Some(0x0000), 0xFFAC, mem.clone())), // 0xFFAC
        Some(Register::new("WDTCON", Some(0x0000), 0xFFAE, mem.clone())), // 0xFFAE

        Some(Register::new("S0CON", Some(0x0000), 0xFFB0, mem.clone())), // 0xFFB0
        Some(Register::new("SSCCON", Some(0x0000), 0xFFB0, mem.clone())), // 0xFFB2
        None, // 0xFFB4
        None, // 0xFFB6
        None, // 0xFFB8
        None, // 0xFFBA
        None, // 0xFFBC
        None, // 0xFFBE

        Some(Register::new("P2", Some(0x0000), 0xFFC0, mem.clone())), // 0xFFC0
        Some(Register::new("DP2", Some(0x0000), 0xFFC2, mem.clone())), // 0xFFC2
        Some(Register::new("P3", Some(0x0000), 0xFFC4, mem.clone())), // 0xFFC4
        Some(Register::new("DP3", Some(0x0000), 0xFFC6, mem.clone())), // 0xFFC6
        Some(Register::new("P4", Some(0x0000), 0xFFC8, mem.clone())), // 0xFFC8
        Some(Register::new("DP4", Some(0x0000), 0xFFCA, mem.clone())), // 0xFFCA
        Some(Register::new("P6", Some(0x0000), 0xFFCC, mem.clone())), // 0xFFCC
        Some(Register::new("DP", Some(0x0000), 0xFFCE, mem.clone())), // 0xFFCE

        Some(Register::new("DP7", Some(0x0000), 0xFFD0, mem.clone())), // 0xFFD0
        Some(Register::new("DP7", Some(0x0000), 0xFFD2, mem.clone())), // 0xFFD2
        Some(Register::new("P8", Some(0x0000), 0xFFD4, mem.clone())), // 0xFFD4
        Some(Register::new("DP8", Some(0x0000), 0xFFD6, mem.clone())), // 0xFFD6
        None, // 0xFFD8
        None, // 0xFFDA
        None, // 0xFFDC
        None, // 0xFFDE

        None, // 0xFFE0
        None, // 0xFFE2
        None, // 0xFFE4
        None, // 0xFFE6
        None, // 0xFFE8
        None, // 0xFFEA
        None, // 0xFFEC
        None, // 0xFFEE

        None, // 0xFFF0
        None, // 0xFFF2
        None, // 0xFFF4
        None, // 0xFFF6
        None, // 0xFFF8
        None, // 0xFFFA
        None, // 0xFFFC
        None, // 0xFFFE
    ]
}

pub fn esfr_registers<'a>(mem: Rc<RefCell<[u8; 0xFFFF]>>) -> [Option<Register>; 256] {
    [
        None, // 0xF000
        None, // 0xF002
        None, // 0xF004
        None, // 0xF006
        None, // 0xF008
        None, // 0xF0A0
        None, // 0xF00C
        None, // 0xF00E

        None, // 0xF010
        None, // 0xF012
        None, // 0xF014
        None, // 0xF016
        None, // 0xF018
        None, // 0xF01A
        None, // 0xF01C
        None, // 0xF01E

        None, // 0xF020
        None, // 0xF022
        Some(Register::new("XPERCON", Some(0x0401), 0xF024, mem.clone())), // 0xF024
        None, // 0xF026
        None, // 0xF028
        None, // 0xF02A
        None, // 0xF02C
        None, // 0xF02E

        None, // 0xF030
        None, // 0xF032
        None, // 0xF034
        None, // 0xF036
        None, // 0xF038
        None, // 0xF03A
        None, // 0xF03C
        None, // 0xF03E

        None, // 0xF040
        None, // 0xF042
        None, // 0xF044
        None, // 0xF046
        None, // 0xF048
        None, // 0xF04A
        None, // 0xF04C
        None, // 0xF04E

        None, // 0xF050
        None, // 0xF052
        None, // 0xF054
        None, // 0xF056
        None, // 0xF058
        None, // 0xF05A
        None, // 0xF05C
        None, // 0xF05E

        None, // 0xF060
        None, // 0xF062
        None, // 0xF064
        None, // 0xF066
        None, // 0xF068
        None, // 0xF06A
        None, // 0xF06C
        None, // 0xF06E

        None, // 0xF070
        None, // 0xF072
        None, // 0xF074
        None, // 0xF076
        None, // 0xF078
        None, // 0xF07A
        None, // 0xF07C
        None, // 0xF07E

        None, // 0xF080
        None, // 0xF082
        None, // 0xF084
        None, // 0xF086
        None, // 0xF088
        None, // 0xF08A
        None, // 0xF08C
        None, // 0xF08E

        None, // 0xF090
        None, // 0xF092
        None, // 0xF094
        None, // 0xF096
        None, // 0xF098
        None, // 0xF09A
        None, // 0xF09C
        None, // 0xF09E

        Some(Register::new("ADDAT2", Some(0x0000), 0xF0A0, mem.clone())), // 0xF0A0
        None, // 0xF0A2
        None, // 0xF0A4
        None, // 0xF0A6
        None, // 0xF0A8
        None, // 0xF0AA
        None, // 0xF0AC
        None, // 0xF0AE

        None, // 0xF0B0
        None, // 0xF0B2
        None, // 0xF0B4
        None, // 0xF0B6
        None, // 0xF0B8
        None, // 0xF0BA
        None, // 0xF0BC
        None, // 0xF0BE

        None, // 0xF0C0
        None, // 0xF0C2
        None, // 0xF0C4
        None, // 0xF0C6
        None, // 0xF0C8
        None, // 0xF0CA
        None, // 0xF0CC
        None, // 0xF0CE

        None, // 0xF0D0
        None, // 0xF0D2
        None, // 0xF0D4
        None, // 0xF0D6
        None, // 0xF0D8
        None, // 0xF0DA
        None, // 0xF0DC
        None, // 0xF0DE

        None, // 0xF0E0
        None, // 0xF0E2
        None, // 0xF0E4
        None, // 0xF0E6
        None, // 0xF0E8
        None, // 0xF0EA
        None, // 0xF0EC
        None, // 0xF0EE

        None, // 0xF0F0
        None, // 0xF0F2
        None, // 0xF0F4
        None, // 0xF0F6
        None, // 0xF0F8
        None, // 0xF0FA
        None, // 0xF0FC
        None, // 0xF0FE

        None, // 0xF100
        None, // 0xF102
        None, // 0xF104
        None, // 0xF106
        None, // 0xF108
        None, // 0xF10A
        None, // 0xF10C
        None, // 0xF10E

        None, // 0xF110
        None, // 0xF112
        None, // 0xF114
        None, // 0xF116
        None, // 0xF118
        None, // 0xF11A
        None, // 0xF11C
        None, // 0xF11E

        None, // 0xF120
        None, // 0xF122
        None, // 0xF124
        None, // 0xF126
        None, // 0xF128
        None, // 0xF12A
        None, // 0xF12C
        None, // 0xF12E

        None, // 0xF130
        None, // 0xF132
        None, // 0xF134
        None, // 0xF136
        None, // 0xF138
        None, // 0xF13A
        None, // 0xF13C
        None, // 0xF13E

        None, // 0xF140
        None, // 0xF142
        None, // 0xF144
        None, // 0xF146
        None, // 0xF148
        None, // 0xF14A
        None, // 0xF14C
        None, // 0xF14E

        None, // 0xF150
        None, // 0xF152
        None, // 0xF154
        None, // 0xF156
        None, // 0xF158
        None, // 0xF15A
        None, // 0xF15C
        None, // 0xF15E

        None, // 0xF160
        None, // 0xF162
        None, // 0xF164
        None, // 0xF166
        None, // 0xF168
        None, // 0xF16A
        None, // 0xF16C
        None, // 0xF16E

        None, // 0xF170
        None, // 0xF172
        None, // 0xF174
        None, // 0xF176
        None, // 0xF178
        None, // 0xF17A
        None, // 0xF17C
        None, // 0xF17E

        None, // 0xF180
        None, // 0xF182
        None, // 0xF184
        None, // 0xF186
        None, // 0xF188
        None, // 0xF18A
        None, // 0xF18C
        None, // 0xF18E

        None, // 0xF190
        None, // 0xF192
        None, // 0xF194
        None, // 0xF196
        None, // 0xF198
        None, // 0xF19A
        None, // 0xF19C
        None, // 0xF19E

        None, // 0xF1A0
        None, // 0xF1A2
        None, // 0xF1A4
        None, // 0xF1A6
        None, // 0xF1A8
        None, // 0xF1AA
        None, // 0xF1AC
        None, // 0xF1AE

        None, // 0xF1B0
        None, // 0xF1B2
        None, // 0xF1B4
        None, // 0xF1B6
        None, // 0xF1B8
        None, // 0xF1BA
        None, // 0xF1BC
        None, // 0xF1BE

        None, // 0xF1C0
        None, // 0xF1C2
        None, // 0xF1C4
        None, // 0xF1C6
        None, // 0xF1C8
        None, // 0xF1CA
        None, // 0xF1CC
        None, // 0xF1CE

        None, // 0xF1D0
        None, // 0xF1D2
        None, // 0xF1D4
        None, // 0xF1D6
        None, // 0xF1D8
        None, // 0xF1DA
        None, // 0xF1DC
        None, // 0xF1DE

        None, // 0xF1E0
        None, // 0xF1E2
        None, // 0xF1E4
        None, // 0xF1E6
        None, // 0xF1E8
        None, // 0xF1EA
        None, // 0xF1EC
        None, // 0xF1EE

        None, // 0xF1F0
        None, // 0xF1F2
        None, // 0xF1F4
        None, // 0xF1F6
        None, // 0xF1F8
        None, // 0xF1FA
        None, // 0xF1FC
        None, // 0xF1FE
    ]
}