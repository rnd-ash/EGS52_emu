
#[derive(Debug, Clone)]
pub struct Watchdog {

}

impl Watchdog {
    pub fn new() -> Self {
        Self {}
    }

    pub fn service(&mut self) {

    }
}

impl super::Peripheral for Watchdog {
    fn update(&mut self, mem: &mut [u8]) {
        
    }
}