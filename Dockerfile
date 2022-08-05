FROM haskell:8.10

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get -y install \
    tzdata \
    libglew-dev \
    libsdl2-dev \
    git \
    autoconf \
    make \
    libtool \
    curl \
    freeglut3 \
    freeglut3-dev \
    build-essential \
    libxmu-dev \
    libxi-dev \
    libgl-dev

RUN git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    ./autogen.sh && \
    ./configure --enable-module-schnorrsig --enable-module-extrakeys --enable-module-ecdh --enable-experimental && \
    make && \
    make install && \
    cd ..

RUN ln -s /usr/local/lib/libsecp256k1.so.0 /usr/lib/libsecp256k1.so.0

# install linuxdeploy and workaround AppImage issues with docker
RUN wget https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage -O /opt/linuxdeploy-x86_64.AppImage                  && \
    cd /opt/; chmod +x linuxdeploy-x86_64.AppImage; sed -i 's|AI\x02|\x00\x00\x00|' linuxdeploy-x86_64.AppImage; ./linuxdeploy-x86_64.AppImage --appimage-extract && \
    mv /opt/squashfs-root /opt/linuxdeploy-x86_64.AppImage.AppDir                                                                                                 && \
    ln -s /opt/linuxdeploy-x86_64.AppImage.AppDir/AppRun /usr/bin/linuxdeploy-x86_64.AppImage

# install appimagetool and workaround AppImage issues with docker
RUN wget https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage -O /opt/appimagetool-x86_64.AppImage                  && \
    cd /opt/; chmod +x appimagetool-x86_64.AppImage; sed -i 's|AI\x02|\x00\x00\x00|' appimagetool-x86_64.AppImage; ./appimagetool-x86_64.AppImage --appimage-extract && \
    mv /opt/squashfs-root /opt/appimagetool-x86_64.AppImage.AppDir                                                                                                 && \
    ln -s /opt/appimagetool-x86_64.AppImage.AppDir/AppRun /usr/bin/appimagetool-x86_64.AppImage
