@echo off
set SOAR_HOME=%~dp0bin
set PATH=%SOAR_HOME%;%PATH%
start javaw -jar bin\Eaters_TankSoar.jar config\tanksoar.cnf