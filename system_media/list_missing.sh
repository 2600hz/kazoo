#!/bin/bash

cd `dirname $0`

while read i
do 
  FILENAME=`echo $i | cut -d':' -f1`; 
  if [ -z "${FILENAME}" ]
  then 
  	continue
  fi

  for DIR in `find . -mindepth 1 -type d`
  do
	if [ $(ls ${DIR}/${FILENAME}* 2>/dev/null | wc -w) -lt 1 ]
	then
	  echo ${DIR}/${FILENAME}
	fi
  done
  
done < prompts.txt 
