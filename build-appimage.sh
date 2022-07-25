#!/bin/sh

cp -R assets futr.AppDir/usr/bin
cp assets/icons/futr-icon.png futr.AppDir
linuxdeploy-x86_64.AppImage --appdir futr.AppDir
ARCH=x86_64 ./appimagetool-x86_64.AppImage futr.AppDir
