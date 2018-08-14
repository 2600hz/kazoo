#!/bin/bash
set -x
set -e

pushd $(dirname $0) > /dev/null
cd $(pwd -P)/..

KAZOO_OTP_VERSION=$(<./make/erlang_version)
OTP_VERSION=${OTP_VERSION:-$KAZOO_OTP_VERSION}

echo "KERL_BUILD_BACKEND=git" > ~/.kerlrc
echo "KERL_CONFIGURE_OPTIONS=\"--without-odbc --with-ssl --disable-hipe --enable-threads --enable-smp-support --enable-kernel-poll\"" >> ~/.kerlrc
 
if [[ ! -d ~/.kerl/$OTP_VERSION ]]; then
    if [[ ! -d ~/.kerl ]]; then
        mkdir ~/.kerl
        curl -fsSLo ~/.kerl/kerl https://raw.githubusercontent.com/kerl/kerl/master/kerl
        chmod +x ~/.kerl/kerl
    fi
    ~/.kerl/kerl update releases
    ~/.kerl/kerl build $OTP_VERSION $OTP_VERSION
    ~/.kerl/kerl install $OTP_VERSION ~/.kerl/$OTP_VERSION
fi

popd > /dev/null
