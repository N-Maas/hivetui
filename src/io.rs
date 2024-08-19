use std::{
    ffi::{OsStr, OsString},
    fs::{self, File},
    io::{self, BufReader},
    path::{Path, PathBuf},
    time::SystemTime,
};

use directories::ProjectDirs;
use tgp::engine::{
    io::{parse_saved_game, restore_game_state, LoadGameError},
    LoggingEngine,
};

use crate::{state::HiveGameState, tui_runner::tui_settings::GameSetup};

pub const HEADER: &str = "hive-tui version 0.1.0";
pub const APP_NAME: &'static str = "hivetui";
const EXTENSION: &'static str = "hivetui";
const SAVE_DIR: &'static str = "saves";
const AUTOSAVE: &'static str = "AUTOSAVE";
const CONFIG: &'static str = "config.json";

pub fn load_game(path: &Path) -> Result<(LoggingEngine<HiveGameState>, GameSetup), LoadGameError> {
    let save_file = File::open(path).map_err(|e| LoadGameError::IO(e))?;
    let (initial_state, n_players, log) = parse_saved_game(BufReader::new(save_file), HEADER)?;
    assert_eq!(n_players, 2);
    let game_setup = GameSetup::from_key_val(initial_state.into_iter())?;
    let engine = restore_game_state(2, || Ok(game_setup.new_game_state()), log)?;
    Ok((engine, game_setup))
}

pub struct IOManager {
    data_dir: PathBuf,
    cached_save_files: Vec<(OsString, SystemTime)>,
}

impl IOManager {
    pub fn new() -> Option<Self> {
        let result = ProjectDirs::from("", "", APP_NAME).and_then(|p| {
            Some(Self {
                data_dir: p.data_dir().to_path_buf(),
                cached_save_files: Vec::new(),
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

    pub fn save_file_path(&self, name: &OsStr) -> PathBuf {
        let mut path = self.save_path();
        path.push(name);
        path.set_extension(EXTENSION);
        path
    }

    pub fn autosave_path(&self) -> PathBuf {
        let mut path = self.data_dir.clone();
        path.push(AUTOSAVE);
        path.set_extension(EXTENSION);
        path
    }

    pub fn recompute_save_files_list(&mut self) -> Result<(), io::Error> {
        let path_list = fs::read_dir(self.save_path())?;
        let mut result = Vec::new();
        for path in path_list {
            let path = path?.path();
            if path.extension().map_or(true, |e| e != EXTENSION) {
                continue;
            }
            // file name must exist since extension exists
            let name = path.file_stem().unwrap().to_os_string();
            let modified = path.metadata()?.modified()?;
            result.push((name, modified));
        }
        result.sort_by(|(_, l_time), (_, r_time)| l_time.cmp(r_time).reverse());
        self.cached_save_files = result;
        Ok(())
    }

    pub fn save_files_list(&self) -> &[(OsString, SystemTime)] {
        &self.cached_save_files
    }
}
