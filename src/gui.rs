use std::{sync::{Arc, mpsc}, collections::VecDeque, time::Duration};

use eframe::{epaint::{mutex::RwLock, Rect, Vec2, Color32, Stroke, Pos2}, egui::{CentralPanel, ScrollArea, Window, self, TopBottomPanel, SidePanel, Sense}};
use egui_extras::{TableBuilder, Column};

use crate::c167::{C167, instructions::{InstructionInfo, Instruction}, SEGMENT_SIZE, };

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExecCommand {
    Start,
    Pause,
    NextInstruction
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum CommandResult {
    Halted(String),
    Stepped(InstructionInfo)
}

pub struct App {
    c167: Arc<RwLock<C167>>,
    sender: mpsc::Sender<ExecCommand>,
    receiver: mpsc::Receiver<CommandResult>,
    ins_buffer: VecDeque<InstructionInfo>
}

impl App {
    pub fn new(c167: C167) -> Self {

        let cpu = Arc::new(RwLock::new(c167));
        let cpu_c = cpu.clone();
        let (s, r) = mpsc::channel::<ExecCommand>();
        let (s_i, r_i) = mpsc::channel::<CommandResult>();

        std::thread::spawn(move|| {
            let mut running = false;
            let mut step = false;
            loop {
                if let Ok(cmd) = r.try_recv() {
                    match cmd {
                        ExecCommand::NextInstruction => step = true,
                        ExecCommand::Start => running = true,
                        ExecCommand::Pause => running = false,
                    }
                }
                if running | step {
                    let mut cpu = cpu_c.write();
                    s_i.send(match cpu.exec_step() {
                        Some(i) => {
                            //let ty = i.instruction;
                            //// halt if CALLS
                            //if ty == Instruction::RETS {
                            //    running = false;
                            //}
                            CommandResult::Stepped(i)
                        },
                        None => CommandResult::Halted(format!("Execution halted!")),
                    }).unwrap();
                    step = false;
                }

                //std::thread::sleep(Duration::from_millis(10));
            }
        });

        Self {
            c167: cpu,
            sender: s,
            receiver: r_i,
            ins_buffer: VecDeque::new()
        }
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &eframe::egui::Context, _frame: &mut eframe::Frame) {
        let cpu_now = self.c167.read().clone();
        let mem = cpu_now.get_mem();
        let code_location = (cpu_now.get_code_seg_pointer() as usize * SEGMENT_SIZE) + cpu_now.get_instruction_pointer() as usize;
        while let Ok(r) = self.receiver.try_recv() {
            if let CommandResult::Stepped(ins) = r {
                self.ins_buffer.push_back(ins);
                if self.ins_buffer.len() > 200 {
                    self.ins_buffer.pop_front();
                }
            }
        }

        Window::new("System Registers").show(ctx, |ui| {
            egui::Grid::new("s_registers").striped(true).show(ui, |ui| {
                ui.strong("Register");
                ui.strong("Value (Hex)");
                ui.strong("Value (Unsigned)");
                ui.strong("Value (Signed)");
                ui.end_row();
            
                let cp_v = cpu_now.get_sfr_reg(8).get_raw_no_tracking(mem);
                let sp_v = cpu_now.get_sfr_reg(9).get_raw_no_tracking(mem);
                let ip_v = code_location;

                // CP
                ui.strong("Context pointer");
                ui.label(format!("{:02X?}", cp_v));
                ui.label(format!("{}", u16::from_le_bytes(cp_v)));
                ui.label(format!("{}", i16::from_le_bytes(cp_v)));
                ui.end_row();
                // SP
                ui.strong("Stack pointer");
                ui.label(format!("{:02X?}", sp_v));
                ui.label(format!("{}", u16::from_le_bytes(sp_v)));
                ui.label(format!("{}", i16::from_le_bytes(sp_v)));
                ui.end_row();

                ui.strong("Instruction pointer");
                ui.label(format!("0x{:08X?}", ip_v));
                ui.end_row();
            });
            ui.strong("Instruction pointer loc");
            let w = ui.available_width();
            let (resp, painter) = ui.allocate_painter(Vec2::new(w, 25.0), Sense::click());
            let mut rect = resp.rect;
            painter.rect_filled(rect, 0.0, Color32::DARK_GREEN);

            let w_per_addr = w / (0x80000 as f32);


            let x = w_per_addr * code_location as f32;

            let l = rect.left();

            rect.set_left(l + x);
            rect.set_right(l + x + 2.0);
            painter.rect_filled(rect, 0.0, Color32::RED);

            ui.strong("Memory access (Blue) = R, (Red) = W");
            let (resp, painter) = ui.allocate_painter(Vec2::new(w, 25.0), Sense::click());
            let mut rect = resp.rect;
            painter.rect_filled(rect, 0.0, Color32::GOLD);
            let w_per_addr = w / (0x10000 as f32);

            let r = cpu_now.get_mem().get_last_read();
            let w = cpu_now.get_mem().get_last_write();

            let x_min_read = w_per_addr * r.clone().min().unwrap() as f32;
            let mut x_max_read = w_per_addr * r.max().unwrap() as f32;

            if x_max_read - x_min_read < 2.0 {
                x_max_read = x_min_read + 2.0;
            }

            let x_min_write = w_per_addr * w.clone().min().unwrap() as f32;
            let mut x_max_write = w_per_addr * w.max().unwrap() as f32;

            if x_max_write - x_min_write < 2.0 {
                x_max_write = x_min_write + 2.0;
            }

            let l = rect.left();

            let mut rect_r = rect.clone();
            let mut rect_w = rect.clone();

            rect_r.set_left(l + x_min_read);
            rect_r.set_right(l + x_max_read);
            painter.rect_filled(rect_r, 0.0, Color32::BLUE);

            rect_w.set_left(l + x_min_write);
            rect_w.set_right(l + x_max_write);
            painter.rect_filled(rect_w, 0.0, Color32::RED);




        });

        Window::new("Call stack").show(ctx, |ui| {
            let mut i = cpu_now.calls_tracker.len();
            for entry in &cpu_now.calls_tracker {
                ui.label(format!("{} - CSP: 0x{:02X?}, IP: 0x{:08X?}", i, entry.csp, entry.ip));
                i -= 1;
            }
        });

        Window::new("PSW").show(ctx, |ui| {
            egui::Grid::new("psw_reg").striped(true).show(ui, |ui| {
                let psw = cpu_now.get_psw_flags();
                ui.strong("E");
                ui.strong("Z");
                ui.strong("V");
                ui.strong("C");
                ui.strong("N");
                ui.end_row();
                ui.strong(format!("{}", psw.e() ));
                ui.strong(format!("{}", psw.z() ));
                ui.strong(format!("{}", psw.v() ));
                ui.strong(format!("{}", psw.c() ));
                ui.strong(format!("{}", psw.n() ));
                ui.end_row();
            });
        });

        Window::new("GPR Registers").show(ctx, |ui| {
            let gpr = cpu_now.get_gpr_data();
            ui.strong(format!("Current CP address: {:04X?}", gpr.addr));
            
            egui::Grid::new("w_registers").striped(true).show(ui, |ui| {
                ui.strong("Word register");
                ui.strong("Value");
                ui.strong("Byte register");
                ui.strong("Value");
                ui.end_row();
                for i in 0..16 {
                    ui.label(gpr.words[i].name);
                    ui.code(format!("{:02X?}", gpr.words[i].get_raw_no_tracking(mem)));
                    ui.label(gpr.bytes[i].name);
                    ui.code(format!("{:02X?}", gpr.bytes[i].get_raw_no_tracking(mem)[0]));
                    ui.end_row();
                }
            });
        });

        Window::new("SFR Registers").show(ctx, |ui| {
            ScrollArea::new([false, true]).max_height(200.0).show(ui, |ui| {
                egui::Grid::new("w_registers").striped(true).show(ui, |ui| {
                    ui.strong("Name");
                    ui.strong("Address");
                    ui.strong("Value");
                    ui.end_row();
                    for c in cpu_now.get_sfr_reg_list().iter() {
                        if let Some(reg) = c {
                            ui.label(reg.name);
                            ui.label(format!("0x{:04X?}", reg.get_addr()));
                            ui.label(format!("0x{:04X?}", reg.get_value_word_no_tracking(&cpu_now.get_mem())));
                            ui.end_row();
                        }
                    }
                });
            });
        });

        Window::new("ESFR Registers").show(ctx, |ui| {
            ScrollArea::new([false, true]).max_height(200.0).show(ui, |ui| {
                egui::Grid::new("w_registers").striped(true).show(ui, |ui| {
                    ui.strong("Name");
                    ui.strong("Address");
                    ui.strong("Value");
                    ui.end_row();
                    for c in cpu_now.get_esfr_reg_list().iter() {
                        if let Some(reg) = c {
                            ui.label(reg.name);
                            ui.label(format!("0x{:04X?}", reg.get_addr()));
                            ui.label(format!("0x{:04X?}", reg.get_value_word_no_tracking(&cpu_now.get_mem())));
                            ui.end_row();
                        }
                    }
                });
            });
        });

        Window::new("DPP addressing").show(ctx, |ui| {
            egui::Grid::new("ddp").striped(true).show(ui, |ui| {
                ui.strong("DPP0");
                ui.label(format!("0x{:04X?}", cpu_now.get_sfr_reg(0).get_value_word_no_tracking(mem)));
                ui.end_row();

                ui.strong("DPP1");
                ui.label(format!("0x{:04X?}", cpu_now.get_sfr_reg(1).get_value_word_no_tracking(mem)));
                ui.end_row();

                ui.strong("DPP2");
                ui.label(format!("0x{:04X?}", cpu_now.get_sfr_reg(2).get_value_word_no_tracking(mem)));
                ui.end_row();

                ui.strong("DPP3");
                ui.label(format!("0x{:04X?}", cpu_now.get_sfr_reg(3).get_value_word_no_tracking(mem)));
                ui.end_row();
            });
            egui::Grid::new("ext").striped(true).show(ui, |ui| {
                ui.strong("EXTPage");
                ui.label(format!("0x{:04X?}", cpu_now.extp_page));
                ui.end_row();

                ui.strong("EXTP");
                ui.label(format!("{}", cpu_now.extp_value));
                ui.end_row();

                ui.strong("EXTR");
                ui.label(format!("{}", cpu_now.extr_value));
                ui.end_row();
            });
        });

        SidePanel::left("ins_ui").show(ctx, |ui| {
            ui.vertical(|ui| {
                ui.horizontal(|ui| {
                    if ui.button("Step").clicked() {
                        self.sender.send(ExecCommand::NextInstruction);
                    }
                    if ui.button("Pause").clicked() {
                        self.sender.send(ExecCommand::Pause);
                    }
                    if ui.button("Resume").clicked() {
                        self.sender.send(ExecCommand::Start);
                    }
                });
    
                ScrollArea::new([true, true]).id_source("instruction_view").stick_to_bottom(true).max_width(200.0).show(ui, |ui| {
                    egui::Grid::new("instructions").striped(true).show(ui, |ui| {
                        ui.strong("Flash location");
                        ui.strong("Instruction");
                        ui.strong("bytes");
                        ui.end_row();
                        for i in &self.ins_buffer {
                            ui.code(format!("0x{:08X?}", i.offset));
                            ui.code(format!("{:?}", i.instruction));
                            ui.code(format!("{:02X?}", i.bytes));
                            ui.end_row()
                        }
                    }); 
                });
            });
        });
        CentralPanel::default().show(ctx, |ui| {
            TableBuilder::new(ui)
                .striped(true)
                .resizable(true)
                .column(Column::exact(70.0).resizable(false)) // ADDR LONG
                .columns(Column::exact(20.0).resizable(false), 16) // 0x0-0xF
                .column(Column::exact(120.0).resizable(false)) // ASCII
                .header(20.0, |mut header| {
                    header.col(|ui| {
                        ui.strong("Address");
                    });
                    for x in 0..16 {
                        header.col(|ui| {
                            ui.strong(format!("x{:1X}", x));
                        });
                    }

                    header.col(|ui| {
                        ui.strong("ASCII");
                    });
                })
                .body(|body| {
                    body.rows(20.0, 0x10000/16, |row_idx, mut r| {
                        let idx = row_idx*16;
                        let memory_row = cpu_now.get_mem_region_no_tracking(idx, 16);
                        r.col(|ui| {
                            ui.strong(format!("0x{:04X}", idx));
                        });

                        for i in 0..16 {
                            r.col(|ui| {
                                ui.label(format!("{:02X}", memory_row[i]));
                            }); 
                        }

                        // ASCII
                        r.col(|ui| {
                            let l = String::from_utf8(
                                memory_row.iter().map(|x| {
                                    if *x >= 32 && *x <= 126 {
                                        *x
                                    } else {
                                        46 // .
                                    }
                                }).collect()
                            ).unwrap();

                            ui.label(l);
                        });
                    });
                })
            });
        ctx.request_repaint();
    }
}