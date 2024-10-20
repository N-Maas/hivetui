use std::{
    ffi::{OsStr, OsString},
    fs::{self, File},
    io::{self, BufReader},
    path::{Path, PathBuf},
    time::SystemTime,
};

use directories::ProjectDirs;
use hivetuilib::engine::{
    io::{parse_saved_game, restore_game_state, CompatibilityPolicy, LoadGameError},
    LoggingEngine,
};

use crate::{state::HiveGameState, tui_runner::game_setup::GameSetup};

pub const HEADER: &str = "hive-tui";
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const APP_NAME: &str = "hivetui";
pub const AUTOSAVE: &str = "AUTOSAVE";
const EXTENSION: &str = "hivetui";
const SAVE_DIR: &str = "saves";
const CONFIG: &str = "config.json";
const SETUP: &str = "setup.hivetuis";

pub fn version_two_digit() -> [u32; 2] {
    VERSION
        .split('.')
        .map(|s| s.parse())
        .take(2)
        .collect::<Result<Vec<_>, _>>()
        .unwrap()
        .try_into()
        .unwrap()
}

pub fn load_game(path: &Path) -> Result<(LoggingEngine<HiveGameState>, GameSetup), LoadGameError> {
    let save_file = File::open(path).map_err(LoadGameError::IO)?;
    let (initial_state, n_players, log) = parse_saved_game(
        BufReader::new(save_file),
        HEADER,
        version_two_digit(),
        CompatibilityPolicy::MinorLessEqual,
    )?;
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
        let result = ProjectDirs::from("", "", APP_NAME).map(|p| Self {
            data_dir: p.data_dir().to_path_buf(),
            cached_save_files: Vec::new(),
        });
        result.filter(|io| {
            fs::create_dir_all(io.save_path())
                .inspect_err(|e| eprintln!("Error accessing game data directory: {e}"))
                .is_ok()
        })
    }

    pub fn config_path(&self) -> PathBuf {
        let mut path = self.data_dir.clone();
        path.push(CONFIG);
        path
    }

    pub fn setup_path(&self) -> PathBuf {
        let mut path = self.data_dir.clone();
        path.push(SETUP);
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

    pub fn load_from_index(
        &self,
        index: usize,
    ) -> Option<Result<(LoggingEngine<HiveGameState>, GameSetup), LoadGameError>> {
        let entry = self.save_files_list().get(index);
        entry.map(|(name, _)| {
            let path = self.save_file_path(name);
            load_game(&path)
        })
    }
}
