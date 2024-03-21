use clap::Parser;
use pieces::PieceType;
use std::{collections::BTreeMap, io};

mod ai;
mod cli_runner;
mod display;
mod pieces;
mod state;
mod tui_graphics;
mod tui_runner;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Run with textual CLI output instead of TUI
    #[arg(short, long)]
    cli: bool,
}

fn main() -> io::Result<()> {
    let mut pieces = BTreeMap::new();
    pieces.insert(PieceType::Queen, 1);
    pieces.insert(PieceType::Ant, 3);
    pieces.insert(PieceType::Grasshopper, 3);
    pieces.insert(PieceType::Beetle, 2);
    pieces.insert(PieceType::Spider, 2);
    pieces.insert(PieceType::Ladybug, 1);

    let args = Args::parse();
    if args.cli {
        cli_runner::run_in_cli(pieces);
    } else {
        tui_runner::run_in_tui(pieces)?;
    }
    Ok(())
}
