mod c167;
mod gui;

use std::{fs::File, io::Read};

use c167::C167;
use clap::Parser;
use eframe::NativeOptions;
use gui::App;

#[derive(Debug, Clone, Parser)]
pub struct Settings {
    flash_file: String,
}

fn main() {
    let settings = Settings::parse();
    let mut f = File::open(settings.flash_file).unwrap();
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer);

    let c167 = C167::new(buffer);
    
    let app = App::new(c167);

    let na = NativeOptions::default();

    eframe::run_native("EGS52 simulator", na, Box::new(|cc| Box::new(app)));
}
