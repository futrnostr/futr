#!/bin/sh
RUN="docker run -it -v ${PWD}:/app -w /app prolic/futr-dev:latest"
stack build --copy-bins --local-bin-path futr.AppDir/usr/bin
$RUN cp -R assets futr.AppDir/usr/bin
$RUN cp assets/icons/futr-icon.png futr.AppDir
$RUN linuxdeploy-x86_64.AppImage --appdir futr.AppDir
ARCH=x86_64 $RUN appimagetool-x86_64.AppImage futr.AppDir
