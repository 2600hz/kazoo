#!/bin/bash

URL=$1
TYPE="wav"
shift 1
curl -f $URL | sox -t $TYPE - $@ -t raw -
