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
}

fn main() {
    let settings = Settings::parse();
    let mut f = File::open(settings.flash_file).unwrap();
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer);

    let c167 = C167::new(buffer);
    
    let app = App::new(c167);

    let mut na = NativeOptions::default();
    na.initial_window_size = Some(Vec2::new(1280.0, 720.0));

    eframe::run_native("EGS52 simulator", na, Box::new(|_| Box::new(app)));
}
