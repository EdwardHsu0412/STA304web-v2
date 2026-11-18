@echo off
setlocal
cd /d "%~dp0"
set "STA304_RSCRIPT="
for /f "delims=" %%R in ('where Rscript.exe 2^>nul') do if not defined STA304_RSCRIPT set "STA304_RSCRIPT=%%R"
if not defined STA304_RSCRIPT for /d %%R in ("%ProgramFiles%\R\R-*") do if exist "%%~fR\bin\Rscript.exe" set "STA304_RSCRIPT=%%~fR\bin\Rscript.exe"
if not defined STA304_RSCRIPT (
  echo R was not found. Open app.R in RStudio and select Run App.
  pause
  exit /b 1
)
"%STA304_RSCRIPT%" --vanilla "%~dp0run-local.R"
if errorlevel 1 pause
