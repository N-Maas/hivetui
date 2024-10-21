
# hivetui #

Hivetui is a TUI (Terminal User Interface) implementation of the board game [Hive](https://gen42.com/games/hive).
It is played with the keyboard and includes a challenging AI.

The game includes a tutorial and rules summary.
However, you might want to consult the [full rules](hivegame.com/download/rules.pdf) if you don't know Hive yet.

## Game Impressions ##

In-game:
<br>
<img src="/img/ingame.png" width="85%" alt="In-game screenshot">

Menu:
<br>
<img src="/img/menu.png" width="85%" alt="Menu screenshot">

With different graphic settings:
<br>
<img style="float: left;" src="/img/graphic_pieces.png" width="50%"> &emsp; <img style="float: right;" src="/img/graphic_rainbow.png" width="33%">


## Installation and Usage ##

Pre-built packages are available for some of the most common architectures (currently Windows x86_64 and Linux x86_64).

1. Download the according zip file from the [latest release](https://github.com/N-Maas/hivetui/releases/latest) (**not** "source code", but the single zip file corresponding to your OS)
2. Extract the zip file in the location where you want to store the game

### Windows ###

- Before running hivetui, [Windows Terminal](https://apps.microsoft.com/detail/9n0dx20hk701) must be installed from the Microsoft Store
- Afterwards, hivetui can be started with a double click on `hivetui.bat`
- Alternatively, execute the `hivetui.exe` binary manually in Windows Terminal

**Windows Defender:** It is possible that Windows Defender blocks execution or even silenty deletes the file, causing an error when trying to start the program.

To avoid this, add an exclusion to Windows Security as follows:
1. Open Windows Security
2. Under "Virus & threat protection", select "Manage settings", and then under "Exclusions", select "Add or remove exclusions"
3. Select "Add an exclusion", choose "Folder", and then select the folder where the files are stored

### Linux ###

Just execute the provided binary in a terminal of your choice (which has proper unicode and color support).

### Terminal Colors ###

Currently, the game assumes that the active terminal style has black background and white foreground (text).
If you use a different style, consider changing either the terminal or the style.


## Building from Source ##

To build from source, the [Rust toolchain](https://www.rust-lang.org/tools/install) must be installed.
Then, you can build the project with cargo:

```
cargo build --release
```

This should create a `hivetui` binary in the `target/release` directory.


## Missing Features ##

Features that might be added at some point in the future include:
- Support for the Pillbug
- A replay mode


## Copyright ##

Hive Copyright (c) 2016 Gen42 Games. Hivetui is not associated with or endorsed by Gen42 Games.
