#!/bin/bash

cd `dirname $0`

FILE=~/.bashrc

if grep -q sup $FILE
then
	echo "'sup' alias already exists in $FILE"
else
	echo "adding 'sup' alias to $FILE"
	echo "alias sup='$PWD/sup'" >> $FILE
fi
