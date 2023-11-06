
macro_rules! data3w {
    ($i: ident) => {
        (0x0000 + $i) as u16
    };
}

macro_rules! data3b {
    ($i: ident) => {
        (0x00 + $i) as u8
    };
}

macro_rules! data4w {
    ($i: ident) => {
        (0x0000 + $i) as u16
    };
}

macro_rules! data4b {
    ($i: ident) => {
        (0x00 + $i) as u8
    };
}

macro_rules! data8w {
    ($i: ident) => {
        (0x0000 + $i) as u16
    };
}

macro_rules! data8b {
    ($i: ident) => {
        $i as u8
    };
}

macro_rules! data16w {
    ($i: ident) => {
        (0x0000 + $i) as u16
    };
}

macro_rules! data16b {
    ($i: ident) => {
        $i as u16 | 0x00FF
    };
}

macro_rules! maskw {
    ($i: ident) => {
        (0x0000 + $i) as u16
    };
}

macro_rules! maskb {
    ($i: ident) => {
        $i as u8
    };
}

