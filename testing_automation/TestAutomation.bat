@echo off
for /f "tokens=2 delims==" %%i in ('wmic OS Get localdatetime /value') do set datetimef=%%i
set datetimef=%datetimef:~0,8%_%datetimef:~8,2%-%datetimef:~10,2%-%datetimef:~12,2%

echo "success" %datetimef% >> C:\Users\skt\Documents\Performance-Marketing\weekly-data\testing_automation\test.log
