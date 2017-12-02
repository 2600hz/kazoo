#!/bin/bash

BEAMS=${BEAMS:-$(ls /tmp/beams/*.beam)}
DEST=${DEST:-/opt/kazoo/lib}

for file in $BEAMS; do
        basename=$(basename $file)
        ebin=$(dirname $(find $DEST -name $basename))
        hotload=$(basename $basename .beam)

        echo "mv $file $ebin/$basename"
        mv -f $file $ebin/$basename
        echo "hotloading $hotload"
        sup kazoo_maintenance hotload $hotload
        sup -necallmgr kazoo_maintenance hotload $hotload
done
