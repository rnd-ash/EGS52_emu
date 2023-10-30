use std::{sync::{Arc, mpsc}, collections::VecDeque, time::Duration};

use eframe::{epaint::mutex::RwLock, egui::{CentralPanel, ScrollArea, Window, self}};

use crate::c167::{C167, instructions::InstructionInfo, };

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExecCommand {
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
            loop {
                //match r.recv().unwrap() {
                //    ExecCommand::NextInstruction => {
                //        
//
                //    },
                //}
                s_i.send(match cpu_c.write().exec_step() {
                    Some(i) => CommandResult::Stepped(i),
                    None => CommandResult::Halted(format!("Execution halted!")),
                });
                std::thread::sleep(Duration::from_millis(10));
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
    fn update(&mut self, ctx: &eframe::egui::Context, frame: &mut eframe::Frame) {
        let cpu_now = self.c167.read().clone();
        while let Ok(r) = self.receiver.try_recv() {
            if let CommandResult::Stepped(ins) = r {
                self.ins_buffer.push_back(ins)
            }
        }

        Window::new("System Registers").show(ctx, |ui| {
            egui::Grid::new("s_registers").striped(true).show(ui, |ui| {
                ui.strong("Register");
                ui.strong("Value (Hex)");
                ui.strong("Value (Unsigned)");
                ui.strong("Value (Signed)");
                ui.end_row();
            
                let cp_v = cpu_now.get_sfr_reg(8).get_raw();
                let sp_v = cpu_now.get_sfr_reg(9).get_raw();

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
                    ui.code(format!("{:02X?}", gpr.words[i].get_raw()));
                    ui.label(gpr.bytes[i].name);
                    ui.code(format!("{:02X?}", gpr.bytes[i].get_u8()));
                    ui.end_row();
                }
            });
        });

        Window::new("PSW Flags").show(ctx, |ui| {
            let psw = cpu_now.get_psw_flags();
            egui::Grid::new("pswflg").striped(true).show(ui, |ui| {
                ui.strong("E");
                ui.strong("Z");
                ui.strong("V");
                ui.strong("C");
                ui.strong("N");
                ui.end_row();

                ui.strong(format!("{}", if psw.e { 1 } else { 0 }));
                ui.strong(format!("{}", if psw.z { 1 } else { 0 }));
                ui.strong(format!("{}", if psw.v { 1 } else { 0 }));
                ui.strong(format!("{}", if psw.c { 1 } else { 0 }));
                ui.strong(format!("{}", if psw.n { 1 } else { 0 }));
                ui.end_row();
            });
        });

        Window::new("DPP addressing").show(ctx, |ui| {
            
        });

        let ui = CentralPanel::default().show(ctx, |ui| {
            if ui.button("Step").clicked() {
                self.sender.send(ExecCommand::NextInstruction);
            }

            ScrollArea::new([true, true]).stick_to_bottom(true).show(ui, |ui| {
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


    }
}