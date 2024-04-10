@echo off
Rscript C:\Users\skt\Documents\Performance-Marketing\weekly-data\inbound_call_update.R

for /f "tokens=2 delims==" %%i in ('wmic OS Get localdatetime /value') do set datetimef=%%i
set datetimef=%datetimef:~0,4%-%datetimef:~4,2%-%datetimef:~6,2%_%datetimef:~8,2%-%datetimef:~10,2%-%datetimef:~12,2%

echo "success" %datetimef% >> C:\Users\skt\Documents\Performance-Marketing\weekly-data\test.log


