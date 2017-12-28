#!/bin/bash

BEAMS=${BEAMS:-$(ls /tmp/beams/*.beam)}
DEST=${DEST:-/opt/kazoo/lib}

function sync {
        file=$1
        basename=$2
        existing=$3

        ebin=$(dirname $existing)
        hotload=$(basename $basename .beam)

        echo "mv $file $ebin/$basename"
        mv -f $file $ebin/$basename

        echo "hotloading $hotload"
        sup kazoo_maintenance hotload $hotload
        sup -necallmgr kazoo_maintenance hotload $hotload
}

for file in $BEAMS; do
        basename=$(basename $file)

        existing=$(find $DEST -name $basename)

        if [ -z $existing ]; then
                echo "can't find a place for $basename ($file)"
        else
                sync $file $basename $existing
        fi
done
