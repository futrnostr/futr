#!/bin/sh

./linuxdeploy-x86_64.AppImage --appdir futr.AppDir

mkdir -p futr.AppDir/usr/bin
mkdir -p futr.AppDir/usr/share/fonts/truetype/Roboto
mkdir -p futr.AppDir/usr/share/icons/futr

cp build/futr futr.AppDir/usr/bin
cp assets/icons/futr-icon.png futr.AppDir
cp assets/fonts/* futr.AppDir/usr/share/fonts/truetype/Roboto
cp assets/icons/* futr.AppDir/usr/share/icons/futr

ARCH=x86_64 ./appimagetool-x86_64.AppImage futr.AppDir
