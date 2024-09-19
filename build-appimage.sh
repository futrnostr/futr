#!/bin/sh
mkdir -p futr.AppDir/usr/bin
cp dist-newstyle/build/x86_64-linux/ghc-9.6.6/futr-0.1.0.0/build/futr/futr futr.AppDir/usr/bin/
$RUN cp resources/icons/nostr-purple.png futr.AppDir
$RUN linuxdeploy-x86_64.AppImage --appdir futr.AppDir
ARCH=x86_64 $RUN appimagetool-x86_64.AppImage futr.AppDir
