@echo off

set "executable_path=%~dp0%bin\hivetui.exe"

where wt >nul 2>nul
if %ERRORLEVEL% equ 0 (
    if exist "%executable_path%" (
        wt --fullscreen "%executable_path%"
    ) else (
        echo It seems the binary was deleted :(
        echo Probably by Windows Defender. Perhaps you can restore it as follows:
        echo 1. Open Windows Security
        echo 2. Select Virus ^& threat protection and then, under Current threats, select Protection history
        echo 3. Find the Quarantined Item entry for hive and restore it
        pause
    )
) else (
    echo Windows Terminal is not installed. Please install it from Microsoft Store first!
    pause
)
