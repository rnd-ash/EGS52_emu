mod c167;
mod gui;

use std::{fs::File, io::Read};

use c167::C167;
use clap::Parser;
use eframe::{NativeOptions, epaint::Vec2};
use gui::App;

#[derive(Debug, Clone, Parser)]
pub struct Settings {
    flash_file: String,
    eeprom_file: String
}

fn main() {
    let settings = Settings::parse();
    let mut f_flash = File::open(settings.flash_file).unwrap();
    let mut f_eeprom = File::open(settings.eeprom_file).unwrap();
    let mut buffer_flash = Vec::new();
    let mut buffer_eeprom = Vec::new();
    f_flash.read_to_end(&mut buffer_flash).unwrap();
    f_eeprom.read_to_end(&mut buffer_eeprom).unwrap();

    let c167 = C167::new(buffer_flash, buffer_eeprom);
    
    let app = App::new(c167);

    let mut na = NativeOptions::default();
    na.initial_window_size = Some(Vec2::new(1280.0, 720.0));

    eframe::run_native("EGS52 simulator", na, Box::new(|_| Box::new(app)));
}
