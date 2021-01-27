@echo off
set JAVA_HOME=C:\Program Files\AdoptOpenJDK\jdk-15.0.1.9-hotspot
set /p GIT_TAG="Ready? Enter the Cryptomator version (as in the git tag): " || Set GIT_TAG=snapshot
set /p EXE_VER="Enter the NUMERIC exe version (strip any -beta1 etc): "
powershell -NoExit -ExecutionPolicy Unrestricted -Command .\build.ps1 -upstreamVersion %GIT_TAG% -exeVersion %EXE_VER%