use self::{can::CAN, watchdog::Watchdog};

pub mod can;
pub mod capcom_timer;
pub mod watchdog;

#[derive(Debug, Clone)]
pub struct Peripheraldata {
    pub can1: CAN,
    pub can2: CAN,
    pub watchdog: Watchdog
}

pub trait Peripheral {
    fn update(&mut self, mem: &mut [u8]);
}

impl Peripheraldata {
    pub fn new() -> Self {
        Self {
            can1: CAN::new(0xFE00),
            can2: CAN::new(0xEE00),
            watchdog: Watchdog::new()
        }
    }

    pub fn update(&mut self, mem: &mut [u8]) {
        self.can1.update(mem);
        self.can2.update(mem);
        self.watchdog.update(mem);
    }
}