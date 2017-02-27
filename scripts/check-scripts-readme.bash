#!/bin/bash

pushd $(dirname $0) > /dev/null

exit_signal=0

for script in $(ls -p | grep -v / | grep -v "README.md"); do
    line=$(grep $script "README.md")
    if [ $? -eq 1 ]; then
       exit_signal=1
       echo "./scripts/$script is not documented!"
    fi
done

popd > /dev/null

exit $exit_signal
