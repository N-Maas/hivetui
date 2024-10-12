@echo off

set "executable_path=%~dp0%bin\hive.exe"

where wt >nul 2>nul
if %ERRORLEVEL% equ 0 (
    wt --fullscreen "%executable_path%"
) else (
    echo Windows Terminal is not installed. Please install it from Microsoft Store first!
    pause
)
