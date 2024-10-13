
# hivetui #

Hivetui is a TUI (Terminal User Interface) implementation of the board game [Hive](https://gen42.com/games/hive).
It is played with the keyboard and includes a challenging AI.


## Installation and Usage ##

Pre-built packages are available for some of the most common architectures (currently Windows x86_64 and Linux x86_64).

1. Download the according zip file from the [latest release](https://github.com/N-Maas/hivetui/releases/latest) (**not** "source code", but the single zip file corresponding to your OS)
2. Extract the zip file in the location where you want to store the game

### Windows ###

- Before running hivetui, [Windows Terminal](https://apps.microsoft.com/detail/9n0dx20hk701) must be installed from the Microsoft Store
- Afterwards, hivetui can be started with a double click on `hivetui.bat`
- Alternatively, execute the `hivetui.exe` binary manually in Windows Terminal

**Windows Defender:** It is possible that Windows Defender blocks execution of the downloaded files. This should be solvable by manually unblocking the files as follows:
1. Right click on the `hivetui.bat` file
2. Select "Properties"
3. In "General"/"Security", mark "Unblock"
4. Confirm the change
5. Repeat steps 1 to 4 for the `hivetui.exe` file in the `bin` directory

### Linux ###

Just execute the provided binary in a terminal of your choice (which has proper unicode and color support).
