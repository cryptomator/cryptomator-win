@echo off
set JAVA_HOME=C:\Program Files\Java\jdk-11.0.2
echo BEFORE WE START: You need to manually update resources/resourcehacker/cryptomator.rc!
set /p GIT_TAG="Ready? Enter the Cryptomator version (as in the git tag): " || Set GIT_TAG=snapshot
powershell -NoExit -ExecutionPolicy Unrestricted -Command .\build.ps1 -buildVersion %GIT_TAG% -signtool 'signtool sign /sha1 6FDEC9DFCFE59E6BAEE64B7ED97F00E120E70D97 $p'