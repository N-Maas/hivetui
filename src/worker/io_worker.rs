use crate::io_manager::HEADER;
use crate::tui_runner::game_setup::GameSetup;
use crate::tui_runner::tui_settings::Settings;
use std::fs::File;
use std::io::{self, Write};
use std::path::PathBuf;
use std::{thread, time::Duration};
use tgp::engine::io::{save_game_to_file, SerializedLog};

use super::{start_worker_thread, MasterEndpoint, WorkerEndpoint};

#[derive(Debug)]
pub struct WriteTask {
    path: PathBuf,
    content: WriteContent,
    report_result: bool,
}

impl WriteTask {
    pub fn save_game(
        path: PathBuf,
        setup: GameSetup,
        log: SerializedLog,
        report_result: bool,
    ) -> Self {
        Self {
            path,
            content: WriteContent::Game(setup, log),
            report_result,
        }
    }
    pub fn save_settings(path: PathBuf, settings: Settings, report_result: bool) -> Self {
        Self {
            path,
            content: WriteContent::Settings(settings),
            report_result,
        }
    }
}

#[derive(Debug)]
enum WriteContent {
    Game(GameSetup, SerializedLog),
    Settings(Settings),
}

pub type IOEndpoint = MasterEndpoint<Result<(), io::Error>, WriteTask>;

pub fn start_io_worker_thread() -> IOEndpoint {
    start_worker_thread(|endpoint: &WorkerEndpoint<_, WriteTask>| loop {
        thread::sleep(Duration::from_millis(10));
        if let Some(task) = endpoint.get_new_msg() {
            let result = match task.content {
                WriteContent::Game(setup, log) => {
                    save_game_to_file(&task.path, HEADER, setup.into_key_val(), 2, log)
                }
                WriteContent::Settings(settings) => {
                    File::create(task.path).and_then(|mut f| write!(f, "{}", settings.to_json()))
                }
            };
            if task.report_result {
                endpoint.send_overwrite(result);
            }
        }
    })
}
