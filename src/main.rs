/*******************************************************************************
*Operating systems: Linux task manager
*
*
*
*
*
*Code below dealing with CLI was written by Michael Henein
*
*Code surrounding GUI was written by Ethan Rose with help from AI (grok.com)
*
* As per teacher instruction all open source was allowed for this projectI used AI as a tool to help me with syntax, error handling, 
* and for functions such as how to sort and display a list as a table in a gui. Below is a summary of what I was helped with. 
* At no point did I use AI to "do this project" for me. I used our teams list of design features and used AI as a tool to help me implement these features
* At no point did I utilize it to recommend better features or create the project all by itself with its own mind.
*### Works Cited
*Grok. Personal communication, xAI artificial intelligence assistance for Rust-based Linux process manager, 2025.
*A constructed list of the parts I had help from AI (grok.com). 
*## Acknowledgments
*Grok (xAI) assisted with this Linux process manager (personal communication, 2025):
*1. GUI layout with 1/5 left, 4/5 right panels using egui.
*2. Fixed E0502 borrowing errors and Roslyn::Button typo.
*3. Added sortable CPU/memory columns and process grouping.
*4. Increased Show/Hide Background Processes button size.
*5. Ensured chrome classified as foreground process.
*6. Configured table to show headers and "No processes found".
*7. Implemented background process toggle with tty_nr check.
*8. Removed debug print statements from update functions.
*9. Suggested debugging for process detection and WSL issues.
*10.Linking CLI and GUI to function as one
*
*
*******************************************************************************/
use procfs::process::all_processes;
use std::collections::{BTreeMap, HashMap, HashSet};
use colored::*;
use users::{get_user_by_uid, get_current_uid};
use libc::{self, c_int};
use std::{thread, time};
use sysinfo::{System, SystemExt, ProcessExt, Pid, Signal, PidExt};
use std::time::{Duration, Instant};
use eframe::egui;
use std::cmp::Ordering;
use std::error::Error;

#[derive(Debug)]
struct ProcInfo {
    pid: i32,
    command: String,
    user: String,
    state: char,
}

fn build_process_tree() -> (BTreeMap<i32, Vec<i32>>, HashMap<i32, ProcInfo>) {
    let mut tree: BTreeMap<i32, Vec<i32>> = BTreeMap::new();
    let mut proc_info_map: HashMap<i32, ProcInfo> = HashMap::new();

    for process in all_processes().unwrap() {
        if let Ok(p) = process {
            if let Ok(stat) = p.stat() {
                let pid = p.pid();
                let ppid = stat.ppid;
                let command = stat.comm.clone();
                let state = stat.state;
                let uid = p.uid().unwrap_or(0);
                let user = get_user_by_uid(uid)
                    .map(|u| u.name().to_string_lossy().to_string())
                    .unwrap_or_else(|| "unknown".to_string());

                tree.entry(ppid).or_default().push(pid);
                proc_info_map.insert(
                    pid,
                    ProcInfo {
                        pid,
                        command,
                        user,
                        state,
                    },
                );
            }
        }
    }

    (tree, proc_info_map)
}

fn search_and_filter_processes(keyword: &str) -> Vec<String> {
    let (_, proc_info_map) = build_process_tree();
    let mut output = Vec::new();

    let mut matches = proc_info_map.values().collect::<Vec<_>>();

    if keyword.starts_with("user:") {
        let user = keyword.trim_start_matches("user:");
        matches.retain(|p| p.user.contains(user));
    } else if keyword.starts_with("pid:") {
        let pid = keyword.trim_start_matches("pid:").parse::<i32>().unwrap_or(-1);
        matches.retain(|p| p.pid == pid);
    } else {
        matches.retain(|p| p.command.contains(keyword));
    }

    if matches.is_empty() {
        output.push(format!("🔍 No matching processes found for '{}'", keyword));
    } else {
        output.push(format!("\n{} '{}':", "Filtered Results For".bold().underline(), keyword));
        for proc in matches {
            let state_colored = match proc.state {
                'R' => "Running".green(),
                'S' => "Sleeping".yellow(),
                'Z' => "Zombie".red(),
                'T' => "Stopped".magenta(),
                _ => "Unknown".normal(),
            };

            output.push(format!(
                "[{}] - {} ({}) {}",
                proc.pid.to_string().cyan(),
                proc.command.bold(),
                state_colored,
                format!("[user: {}]", proc.user).dimmed()
            ));
        }
    }
    output
}

fn terminate_process(pid: i32) -> Vec<String> {
    let mut output = Vec::new();
    unsafe {
        output.push(format!("Sending SIGTERM to PID {}...", pid));
        let result = libc::kill(pid as c_int, libc::SIGTERM);
        if result != 0 {
            output.push(format!("❌ Failed to send SIGTERM: {}", std::io::Error::last_os_error()));
            return output;
        }

        thread::sleep(time::Duration::from_millis(300));

        if std::fs::metadata(format!("/proc/{}", pid)).is_ok() {
            output.push(format!("⚠️  Process {} is still alive.", pid));
            output.push("Force kill it with SIGKILL? (y/n): ".to_string());
        } else {
            output.push(format!("✅ Process {} terminated successfully.", pid));
        }
    }

    output.push("\nUpdated Process Tree:".bold().underline().to_string());
    let (tree, proc_info_map) = build_process_tree();
    let root_pid = if proc_info_map.contains_key(&1) { 1 } else { 0 };
    output.extend(capture_tree_display(&tree, &proc_info_map, root_pid));
    output
}

fn capture_tree_display(
    tree: &BTreeMap<i32, Vec<i32>>,
    proc_info_map: &HashMap<i32, ProcInfo>,
    pid: i32,
) -> Vec<String> {
    let mut output = Vec::new();
    capture_tree(tree, proc_info_map, pid, "".to_string(), true, &mut output);
    output
}

fn capture_tree(
    tree: &BTreeMap<i32, Vec<i32>>,
    proc_info_map: &HashMap<i32, ProcInfo>,
    pid: i32,
    prefix: String,
    is_last: bool,
    output: &mut Vec<String>,
) {
    if let Some(info) = proc_info_map.get(&pid) {
        let branch = if is_last { "└──" } else { "├──" };
        let spacer = if is_last { "    " } else { "│   " };
        let state_colored = match info.state {
            'R' => "Running".green(),
            'S' => "Sleeping".yellow(),
            'Z' => "Zombie".red(),
            'T' => "Stopped".magenta(),
            _ => "Unknown".normal(),
        };

        output.push(format!(
            "{}{} [{}] - {} ({}) {}",
            prefix,
            branch,
            info.pid.to_string().cyan(),
            info.command.bold(),
            state_colored,
            format!("[user: {}]", info.user).dimmed()
        ));

        if let Some(children) = tree.get(&pid) {
            let mut sorted_children = children.clone();
            sorted_children.sort();
            for (i, child_pid) in sorted_children.iter().enumerate() {
                capture_tree(
                    tree,
                    proc_info_map,
                    *child_pid,
                    format!("{}{}", prefix, spacer),
                    i == sorted_children.len() - 1,
                    output,
                );
            }
        }
    }
}

#[derive(Clone)]
struct ProcessInfo {
    name: String,
    cpu_usage: f32,
    memory: u64,
}

#[derive(Default, PartialEq)]
enum SortColumn {
    #[default]
    None,
    Cpu,
    Memory,
}

#[derive(PartialEq)]
enum DisplayMode {
    GUI,
    CLI,
}

struct LinuxProcessManager {
    sys: System,
    process_map: HashMap<Pid, ProcessInfo>,
    time_since_last_update: Instant,
    expanded_groups: HashSet<String>,
    sort_type: SortColumn,
    high_to_low: bool,
    proc_search: String,
    process_tree: BTreeMap<i32, Vec<i32>>,
    show_background: bool,
    display_mode: DisplayMode,
    cli_input: String,
    cli_output: Vec<String>,
    cli_state: CliState,
}

#[derive(PartialEq)]
enum CliState {
    Normal,
    WaitingForKillConfirmation(i32),
    WaitingForAnotherProcess,
}

impl Default for LinuxProcessManager {
    fn default() -> Self {
        let (tree, _) = build_process_tree();
        LinuxProcessManager {
            sys: System::new_all(),
            process_map: HashMap::new(),
            time_since_last_update: Instant::now(),
            expanded_groups: HashSet::new(),
            sort_type: SortColumn::default(),
            high_to_low: false,
            proc_search: String::new(),
            process_tree: tree,
            show_background: false,
            display_mode: DisplayMode::GUI,
            cli_input: String::new(),
            cli_output: Vec::new(),
            cli_state: CliState::Normal,
        }
    }
}

impl LinuxProcessManager {
    fn update_proc_list(&mut self) {
        let cur_time = Instant::now();
        if cur_time.duration_since(self.time_since_last_update) >= Duration::from_millis(1600) {
            self.sys.refresh_all();
            self.process_map.clear();

            // Collect raw CPU usage for all processes
            let mut raw_cpu_usages: HashMap<Pid, f32> = HashMap::new();
            for (pid, process) in self.sys.processes() {
                raw_cpu_usages.insert(*pid, process.cpu_usage());
            }

            // Calculate total CPU usage
            let total_cpu: f32 = raw_cpu_usages.values().sum();
            let total_cpu = if total_cpu > 0.0 { total_cpu } else { 1.0 }; // Avoid division by zero

            // Normalize CPU usage to percentages summing to 100%
            for (pid, process) in self.sys.processes() {
                let normalized_cpu = if total_cpu > 0.0 {
                    (raw_cpu_usages[pid] / total_cpu) * 100.0
                } else {
                    0.0
                };
                self.process_map.insert(
                    *pid,
                    ProcessInfo {
                        name: process.name().to_string(),
                        cpu_usage: normalized_cpu,
                        memory: process.memory(),
                    },
                );
            }

            // Update process tree
            let (tree, _) = build_process_tree();
            self.process_tree = tree;

            self.time_since_last_update = cur_time;
        }
    }

    fn handle_cli_input(&mut self) {
        let input = self.cli_input.trim();
        match self.cli_state {
            CliState::Normal => {
                if input.is_empty() {
                    return;
                }
                if input == "q" {
                    self.cli_output.push("👋 Exiting.".to_string());
                    std::process::exit(0);
                } else if input == "f" {
                    self.cli_output.push(
                        "Enter keyword to filter (e.g., 'bash', 'user:root', 'pid:1234'): ".to_string(),
                    );
                    self.cli_state = CliState::WaitingForAnotherProcess;
                } else if let Ok(pid) = input.parse::<i32>() {
                    self.cli_output
                        .push(format!("Are you sure you want to terminate PID {}? (y/n): ", pid));
                    self.cli_state = CliState::WaitingForKillConfirmation(pid);
                } else {
                    self.cli_output.push("❌ Invalid PID.".to_string());
                    self.cli_output.push(
                        "Enter a PID to terminate, or type 'f' to filter/search, or 'q' to quit: "
                            .to_string(),
                    );
                }
            }
            CliState::WaitingForKillConfirmation(pid) => {
                if input.eq_ignore_ascii_case("y") {
                    self.cli_output.extend(terminate_process(pid));
                    self.cli_output
                        .push("\nDo you want to terminate another process? (y/n): ".to_string());
                    self.cli_state = CliState::WaitingForAnotherProcess;
                } else {
                    self.cli_output.push("Termination canceled.".to_string());
                    self.cli_output.push(
                        "Enter a PID to terminate, or type 'f' to filter/search, or 'q' to quit: "
                            .to_string(),
                    );
                    self.cli_state = CliState::Normal;
                }
            }
            CliState::WaitingForAnotherProcess => {
                if input.eq_ignore_ascii_case("y") {
                    self.cli_output
                        .extend(capture_tree_display(&self.process_tree, &build_process_tree().1, 1));
                    self.cli_output.push(
                        "Enter a PID to terminate, or type 'f' to filter/search, or 'q' to quit: "
                            .to_string(),
                    );
                    self.cli_state = CliState::Normal;
                } else if input.eq_ignore_ascii_case("n") {
                    self.cli_output.push("👋 Exiting.".to_string());
                    std::process::exit(0);
                } else {
                    self.cli_output.extend(search_and_filter_processes(input));
                    self.cli_output
                        .push("Do you want to terminate a PID from the filtered list? (y/n): ".to_string());
                    self.cli_state = CliState::WaitingForAnotherProcess;
                }
            }
        }
        self.cli_input.clear();
    }
}

impl eframe::App for LinuxProcessManager {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.update_proc_list();

        ctx.set_style(egui::Style {
            visuals: egui::Visuals {
                window_fill: egui::Color32::WHITE,
                ..Default::default()
            },
            ..Default::default()
        });

        egui::CentralPanel::default().show(ctx, |process_panel| {
            let total_height = process_panel.available_height();
            let total_width = process_panel.available_width();

            process_panel.horizontal(|process_panel| {
                process_panel.vertical(|left_panel| {
                    left_panel.set_width(total_width * 0.25);
                    left_panel.allocate_space(egui::vec2(left_panel.available_width(), 20.0));

                    // GUI Button
                    if left_panel
                        .add(
                            egui::Button::new(
                                egui::RichText::new("GUI")
                                    .size(18.0)
                                    .color(if self.display_mode == DisplayMode::GUI {
                                        egui::Color32::BLACK
                                    } else {
                                        egui::Color32::WHITE
                                    }),
                            )
                            .wrap(false)
                            .min_size(egui::vec2(left_panel.available_width(), 40.0))
                            .fill(if self.display_mode == DisplayMode::GUI {
                                egui::Color32::LIGHT_GRAY
                            } else {
                                egui::Color32::DARK_GRAY
                            }),
                        )
                        .clicked()
                    {
                        self.display_mode = DisplayMode::GUI;
                        self.show_background = false;
                    }

                    // Background Processes Toggle (only visible in GUI mode)
                    if self.display_mode == DisplayMode::GUI {
                        if left_panel
                            .add(
                                egui::Button::new(
                                    egui::RichText::new(if self.show_background {
                                        "Hide Background"
                                    } else {
                                        "Show Background"
                                    })
                                    .size(14.0),
                                )
                                .wrap(false)
                                .min_size(egui::vec2(left_panel.available_width() * 0.8, 30.0)),
                            )
                            .clicked()
                        {
                            self.show_background = !self.show_background;
                        }
                    }

                    // CLI Button
                    if left_panel
                        .add(
                            egui::Button::new(
                                egui::RichText::new("CLI")
                                    .size(18.0)
                                    .color(if self.display_mode == DisplayMode::CLI {
                                        egui::Color32::BLACK
                                    } else {
                                        egui::Color32::WHITE
                                    }),
                            )
                            .wrap(false)
                            .min_size(egui::vec2(left_panel.available_width(), 40.0))
                            .fill(if self.display_mode == DisplayMode::CLI {
                                egui::Color32::LIGHT_GRAY
                            } else {
                                egui::Color32::DARK_GRAY
                            }),
                        )
                        .clicked()
                    {
                        self.display_mode = DisplayMode::CLI;
                        self.show_background = false;
                        self.cli_output
                            .extend(capture_tree_display(&self.process_tree, &build_process_tree().1, 1));
                        self.cli_output.push(
                            "Enter a PID to terminate, or type 'f' to filter/search, or 'q' to quit: ".to_string(),
                        );
                    }

                    left_panel.allocate_space(egui::vec2(left_panel.available_width(), total_height - 20.0));
                });

                process_panel.vertical(|right_panel| {
                    right_panel.set_width(total_width * 0.75);
                    match self.display_mode {
                        DisplayMode::GUI => {
                            right_panel.horizontal(|search_panel| {
                                search_panel.label("Search: ");
                                search_panel.add(
                                    egui::TextEdit::singleline(&mut self.proc_search)
                                        .desired_width(search_panel.available_width() * 0.75)
                                        .hint_text("Process Name or PID"),
                                );
                            });

                            right_panel.heading("Running Processes");
                            right_panel.add(egui::Separator::default().spacing(10.0));

                            egui::ScrollArea::vertical()
                                .max_height(total_height - right_panel.cursor().top())
                                .show(right_panel, |scroll_panel| {
                                    egui::Grid::new("process_grid")
                                        .striped(true)
                                        .min_col_width(100.0)
                                        .max_col_width(300.0)
                                        .spacing([15.0, 10.0])
                                        .show(scroll_panel, |grid_panel| {
                                            grid_panel.strong("Process Name");
                                            let cpu_label = match self.sort_type {
                                                SortColumn::Cpu => {
                                                    if self.high_to_low {
                                                        "CPU % L->H"
                                                    } else {
                                                        "CPU % H->L"
                                                    }
                                                }
                                                _ => "CPU %",
                                            };
                                            let mem_label = match self.sort_type {
                                                SortColumn::Memory => {
                                                    if self.high_to_low {
                                                        "Memory (MB) L->H"
                                                    } else {
                                                        "Memory (MB) H->L"
                                                    }
                                                }
                                                _ => "Memory (MB)",
                                            };
                                            if grid_panel
                                                .add(egui::Button::new(cpu_label).fill(egui::Color32::TRANSPARENT))
                                                .clicked()
                                            {
                                                if self.sort_type == SortColumn::Cpu {
                                                    self.high_to_low = !self.high_to_low;
                                                } else {
                                                    self.sort_type = SortColumn::Cpu;
                                                    self.high_to_low = false;
                                                }
                                            }
                                            if grid_panel
                                                .add(egui::Button::new(mem_label).fill(egui::Color32::TRANSPARENT))
                                                .clicked()
                                            {
                                                if self.sort_type == SortColumn::Memory {
                                                    self.high_to_low = !self.high_to_low;
                                                } else {
                                                    self.sort_type = SortColumn::Memory;
                                                    self.high_to_low = false;
                                                }
                                            }
                                            grid_panel.end_row();

                                            let current_uid = get_current_uid();
                                            let mut process_groups: HashMap<String, Vec<(Pid, &ProcessInfo)>> =
                                                HashMap::new();
                                            for (pid, process) in &self.process_map {
                                                let is_background = if let Ok(proc) =
                                                    procfs::process::Process::new(pid.as_u32() as i32)
                                                {
                                                    if let Ok(stat) = proc.stat() {
                                                        let proc_uid = proc.uid().unwrap_or(0);
                                                        let is_daemon = matches!(
                                                            process.name.to_lowercase().as_str(),
                                                            "systemd" | "cron" | "sshd" | "init"
                                                        );
                                                        stat.tty_nr == 0 && (proc_uid != current_uid || is_daemon)
                                                    } else {
                                                        false
                                                    }
                                                } else {
                                                    false
                                                };

                                                if self.show_background || !is_background {
                                                    process_groups
                                                        .entry(process.name.clone())
                                                        .or_insert_with(Vec::new)
                                                        .push((*pid, process));
                                                }
                                            }

                                            let mut filtered_groups: Vec<_> = process_groups
                                                .into_iter()
                                                .filter(|(name, processes)| {
                                                    self.proc_search.is_empty()
                                                        || name.to_lowercase().contains(&self.proc_search.to_lowercase())
                                                        || processes
                                                            .iter()
                                                            .any(|(pid, _)| pid.to_string() == self.proc_search)
                                                })
                                                .collect();

                                            if filtered_groups.is_empty() {
                                                grid_panel.label("No processes found");
                                                grid_panel.label("");
                                                grid_panel.label("");
                                                grid_panel.end_row();
                                            } else {
                                                match self.sort_type {
                                                    SortColumn::Cpu => {
                                                        filtered_groups.sort_by(|a, b| {
                                                            let a_cpu: f32 = a.1.iter().map(|(_, p)| p.cpu_usage).sum();
                                                            let b_cpu: f32 = b.1.iter().map(|(_, p)| p.cpu_usage).sum();
                                                            let cpu_diff = (a_cpu - b_cpu).abs();
                                                            if cpu_diff < 0.01 {
                                                                // If CPU difference is less than 0.01, sort by name for stability
                                                                a.0.cmp(&b.0)
                                                            } else if self.high_to_low {
                                                                a_cpu.partial_cmp(&b_cpu).unwrap_or(Ordering::Equal)
                                                            } else {
                                                                b_cpu.partial_cmp(&a_cpu).unwrap_or(Ordering::Equal)
                                                            }
                                                        });
                                                    }
                                                    SortColumn::Memory => {
                                                        filtered_groups.sort_by(|a, b| {
                                                            let a_mem: u64 = a.1.iter().map(|(_, p)| p.memory).sum();
                                                            let b_mem: u64 = b.1.iter().map(|(_, p)| p.memory).sum();
                                                            if self.high_to_low {
                                                                a_mem.cmp(&b_mem)
                                                            } else {
                                                                b_mem.cmp(&a_mem)
                                                            }
                                                        });
                                                    }
                                                    SortColumn::None => {
                                                        filtered_groups.sort_by(|a, b| a.0.cmp(&b.0));
                                                    }
                                                }

                                                for (name, processes) in filtered_groups {
                                                    let family = processes.len() > 1;
                                                    let is_expanded = self.expanded_groups.contains(&name);
                                                    let cpu_sum: f32 = processes.iter().map(|(_, p)| p.cpu_usage).sum();
                                                    let mem_sum: u64 = processes.iter().map(|(_, p)| p.memory).sum();
                                                    let display_name = name.clone();

                                                    grid_panel.scope(|grid_panel| {
                                                        let mut button_text = display_name.clone();
                                                        if family {
                                                            button_text.push_str(if is_expanded { " /" } else { " >" });
                                                        }
                                                        let response = grid_panel.add(
                                                            egui::Button::new(button_text)
                                                                .fill(egui::Color32::TRANSPARENT),
                                                        );
                                                        if response.clicked() && family {
                                                            if is_expanded {
                                                                self.expanded_groups.remove(&name);
                                                            } else {
                                                                self.expanded_groups.insert(name.clone());
                                                            }
                                                        }
                                                        response.context_menu(|menu| {
                                                            if menu.button("Terminate Task").clicked() {
                                                                for (pid, _) in &processes {
                                                                    if let Some(process) = self.sys.process(*pid) {
                                                                        let _ = process.kill_with(Signal::Term);
                                                                    }
                                                                }
                                                                menu.close_menu();
                                                            }
                                                        });
                                                    });
                                                    grid_panel.label(format!("{:.2}%", cpu_sum));
                                                    grid_panel.label(format!("{:.2}", mem_sum as f64 / 1024.0 / 1024.0));
                                                    grid_panel.end_row();

                                                    if is_expanded && family {
                                                        for (pid, process_info) in &processes {
                                                            grid_panel.label(format!(
                                                                "  {} (PID: {})",
                                                                process_info.name,
                                                                pid.as_u32()
                                                            ));
                                                            grid_panel.label(format!("{:.2}%", process_info.cpu_usage));
                                                            grid_panel.label(format!(
                                                                "{:.2}",
                                                                process_info.memory as f64 / 1024.0 / 1024.0
                                                            ));
                                                            grid_panel.end_row();

                                                            if let Some(children_pids) =
                                                                self.process_tree.get(&(pid.as_u32() as i32))
                                                            {
                                                                for child_pid in children_pids {
                                                                    let child_pid_sys = Pid::from(*child_pid as usize);
                                                                    if let Some(child_info) =
                                                                        self.process_map.get(&child_pid_sys)
                                                                    {
                                                                        grid_panel.label(format!(
                                                                            "    - {} (PID: {})",
                                                                            child_info.name, child_pid
                                                                        ));
                                                                        grid_panel
                                                                            .label(format!("{:.2}%", child_info.cpu_usage));
                                                                        grid_panel.label(format!(
                                                                            "{:.2}",
                                                                            child_info.memory as f64 / 1024.0 / 1024.0
                                                                        ));
                                                                        grid_panel.end_row();
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        });
                                });
                        }
                        DisplayMode::CLI => {
                            right_panel.heading("CLI Terminal");
                            right_panel.add(egui::Separator::default().spacing(10.0));

                            egui::ScrollArea::vertical()
                                .max_height(total_height - right_panel.cursor().top() - 50.0)
                                .show(right_panel, |scroll_panel| {
                                    for line in &self.cli_output {
                                        scroll_panel.label(line);
                                    }
                                });

                            right_panel.horizontal(|input_panel| {
                                let response = input_panel.add(
                                    egui::TextEdit::singleline(&mut self.cli_input)
                                        .desired_width(input_panel.available_width())
                                        .hint_text("Enter command (PID, 'f', 'q', or y/n)"),
                                );
                                if response.lost_focus() && ctx.input(|i| i.key_pressed(egui::Key::Enter)) {
                                    self.handle_cli_input();
                                    response.request_focus();
                                }
                            });
                        }
                    }
                });
            });
        });

        // Request repaint to ensure continuous updates
        ctx.request_repaint();
    }
}

fn main() -> Result<(), eframe::Error> {
    let options = eframe::NativeOptions {
        initial_window_size: Some(egui::vec2(1200.0, 800.0)),
        ..Default::default()
    };
    eframe::run_native(
        "Linux Process Manager",
        options,
        Box::new(|_cc| Box::new(LinuxProcessManager::default())),
    )
}
