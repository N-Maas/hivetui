use std::{fs, path::PathBuf};

use directories::ProjectDirs;

const APP_NAME: &'static str = "hive_tui";

pub struct IOManager {
    data_dir: PathBuf,
}

impl IOManager {
    pub fn new() -> Option<Self> {
        ProjectDirs::from("", "", APP_NAME).and_then(|p| {
            let data_dir = p.data_dir().to_path_buf();
            fs::create_dir_all(&data_dir)
                .inspect_err(|e| eprintln!("Error accessing game data directory: {e}"))
                .ok();
            Some(Self { data_dir })
        })
    }
}
