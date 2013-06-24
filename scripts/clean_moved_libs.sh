#!/bin/bash

cd `dirname $0`

for path in ../whistle_apps/lib/*
do
  folder=$(basename $path);
  if [ "$folder" != "Makefile" ]
  then
	echo "# rm -rf lib/$folder";
  	rm -rf lib/$folder; 
  fi
done
