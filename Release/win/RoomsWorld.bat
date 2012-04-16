@echo off
set SOAR_HOME=%~dp0
set PATH=%SOAR_HOME%;%PATH%
start javaw -jar SoarRobotServer.jar