use crate::io_manager::HEADER;
use crate::tui_runner::tui_settings::{GameSetup, Settings};
use std::fs::File;
use std::io::{self, Write};
use std::path::PathBuf;
use std::{thread, time::Duration};
use tgp::engine::io::{save_game_to_file, SerializedLog};

use super::{start_worker_thread, MasterEndpoint};

#[derive(Debug)]
pub struct WriteTask {
    path: PathBuf,
    content: WriteContent,
    should_report_err: bool,
}

impl WriteTask {
    pub fn save_game(
        path: PathBuf,
        setup: GameSetup,
        log: SerializedLog,
        should_report_err: bool,
    ) -> Self {
        Self {
            path,
            content: WriteContent::Game(setup, log),
            should_report_err,
        }
    }
    pub fn save_settings(path: PathBuf, settings: Settings, should_report_err: bool) -> Self {
        Self {
            path,
            content: WriteContent::Settings(settings),
            should_report_err,
        }
    }
}

#[derive(Debug)]
enum WriteContent {
    Game(GameSetup, SerializedLog),
    Settings(Settings),
}

pub type IOEndpoint = MasterEndpoint<io::Error, WriteTask>;

pub fn start_io_worker_thread() -> IOEndpoint {
    start_worker_thread(|endpoint| loop {
        thread::sleep(Duration::from_millis(10));
        let result = endpoint.get_new_msg().map(
            |WriteTask {
                 path,
                 content,
                 should_report_err,
             }| {
                match content {
                    WriteContent::Game(setup, log) => (
                        save_game_to_file(&path, HEADER, setup.into_key_val(), 2, log),
                        should_report_err,
                    ),
                    WriteContent::Settings(settings) => (
                        File::create(path).and_then(|mut f| write!(f, "{}", settings.to_json())),
                        should_report_err,
                    ),
                }
            },
        );
        if let Some((Err(e), true)) = result {
            endpoint.send_if_no_msg(e);
        }
    })
}
