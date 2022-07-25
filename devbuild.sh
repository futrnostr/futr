#!/bin/sh
stack build --copy-bins --local-bin-path futr.AppDir/usr/bin
docker run -it -v ${PWD}:/app -w /app prolic/futr-dev:latest ./build-appimage.sh
