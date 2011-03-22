#!/bin/bash

URL=$1
TYPE="wav"

curl -f $URL | sox -t $TYPE - $@ -t raw -
