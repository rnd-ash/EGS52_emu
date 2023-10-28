mod c167;

use std::{fs::File, io::Read};

use c167::C167;
use clap::Parser;

#[derive(Debug, Clone, Parser)]
pub struct Settings {
    flash_file: String,
}

fn main() {
    let settings = Settings::parse();
    let mut f = File::open(settings.flash_file).unwrap();
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer);

    let mut c167 = C167::new(buffer);
    c167.run();
}
