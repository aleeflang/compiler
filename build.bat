@echo off
setlocal enabledelayedexpansion

:: Read the variable from the .env file
for /f "tokens=1,2 delims==" %%A in (.env) do (
    if "%%A"=="ALIF_PATH" set "ALIF_PATH=%%B"
)

:: Run GCC using the path variable
gcc -std=c11 -Wall -Wextra -Werror -I %ALIF_PATH% -o aleefc.exe aleefc.c

endlocal
