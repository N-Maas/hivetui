use std::{fs, path::PathBuf};

use directories::ProjectDirs;

const APP_NAME: &'static str = "hivetui";
const SAVE_DIR: &'static str = "saves";
const AUTOSAVE: &'static str = "AUTOSAVE";
const CONFIG: &'static str = "config.json";

pub struct IOManager {
    data_dir: PathBuf,
}

impl IOManager {
    pub fn new() -> Option<Self> {
        let result = ProjectDirs::from("", "", APP_NAME).and_then(|p| {
            Some(Self {
                data_dir: p.data_dir().to_path_buf(),
            })
        });
        result.filter(|io| {
            fs::create_dir_all(&io.save_path())
                .inspect_err(|e| eprintln!("Error accessing game data directory: {e}"))
                .is_ok()
        })
    }

    pub fn config_path(&self) -> PathBuf {
        let mut path = self.data_dir.clone();
        path.push(CONFIG);
        path
    }

    pub fn save_path(&self) -> PathBuf {
        let mut path = self.data_dir.clone();
        path.push(SAVE_DIR);
        path
    }

    pub fn save_file_path(&self, name: &str) -> PathBuf {
        let mut path = self.save_path();
        path.push(name);
        path.set_extension("hivetui");
        path
    }

    pub fn autosave_path(&self) -> PathBuf {
        self.save_file_path(&AUTOSAVE)
    }
}
