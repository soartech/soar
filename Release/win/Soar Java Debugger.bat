@echo off
set SOAR_HOME=%~dp0
set PATH=%SOAR_HOME%bin;%PATH%
start javaw -jar bin\SoarJavaDebugger.jar

