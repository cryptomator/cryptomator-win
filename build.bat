@echo off
set JAVA_HOME=C:\Program Files\AdoptOpenJDK\jdk-14.0.2.12-hotspot
echo BEFORE WE START: You need to manually update resources/resourcehacker/cryptomator.rc!
set /p GIT_TAG="Ready? Enter the Cryptomator version (as in the git tag): " || Set GIT_TAG=snapshot
powershell -NoExit -ExecutionPolicy Unrestricted -Command .\build.ps1 -upstreamVersion %GIT_TAG%