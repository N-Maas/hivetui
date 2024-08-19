use crate::io::HEADER;
use crate::tui_runner::tui_settings::GameSetup;
use std::io;
use std::path::PathBuf;
use std::{thread, time::Duration};
use tgp::engine::io::{save_game_to_file, SerializedLog};

use super::{start_worker_thread, MasterEndpoint};

#[derive(Debug)]
pub struct SaveGame(pub PathBuf, pub GameSetup, pub SerializedLog);

pub type IOEndpoint = MasterEndpoint<io::Result<()>, SaveGame>;

pub fn start_io_worker_thread() -> IOEndpoint {
    start_worker_thread(|endpoint| loop {
        thread::sleep(Duration::from_millis(10));

        if let Some(SaveGame(path, setup, log)) = endpoint.get_new_msg() {
            let result = save_game_to_file(&path, HEADER, setup.into_key_val(), 2, log);
            endpoint.send_if_no_msg(result);
        }
    })
}
