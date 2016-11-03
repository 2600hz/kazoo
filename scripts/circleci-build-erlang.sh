#!/bin/bash
set -x
set -e
if [[ ! -d ~/.kerl/$OTP_VERSION ]]; then
    if [[ ! -d ~/.kerl ]]; then
        mkdir ~/.kerl
        curl -fsSLo ~/.kerl/kerl https://raw.githubusercontent.com/kerl/kerl/master/kerl
        chmod +x ~/.kerl/kerl
        ~/.kerl/kerl update releases
    fi
    ~/.kerl/kerl build $OTP_VERSION $OTP_VERSION
    ~/.kerl/kerl install $OTP_VERSION ~/.kerl/$OTP_VERSION
fi
