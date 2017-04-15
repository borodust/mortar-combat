@echo off
set WORK_DIR=%~dp0
set PATH=%WORK_DIR%lib\;%PATH%

start /d "%WORK_DIR%" mortar-combat.bin mortar-combat.conf.lisp
