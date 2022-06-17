FROM haskell:8.10

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get -y install tzdata
RUN apt-get install libsdl2-dev libglew-dev git autoconf make libtool curl -y

RUN git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    ./autogen.sh && \
    ./configure --enable-module-schnorrsig --enable-module-extrakeys --enable-module-ecdh --enable-experimental && \
    make && \
    make install

RUN ln -s /usr/local/lib/libsecp256k1.so.0 /usr/lib/libsecp256k1.so.0
