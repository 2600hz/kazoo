#!/bin/bash

pushd $(dirname $0) > /dev/null

cd $(pwd -P)/..

doc_count=0
missing_count=0

function check_index {
    line=$(grep "$1" ./doc/mkdocs/mkdocs.yml)
    if [ $? -ne 1 ]; then
        ((missing_count+=1))
        echo "$1 missing"
    fi
}

for doc in $(find {scripts,doc,core,applications} -type f -path "doc/mkdocs*" -prune -o -regex ".+\.md$"); do
    ((doc_count+=1))
    check_index $doc
done

ratio=$((100 * $missing_count / $doc_count))
echo "Missing $missing_count / $doc_count: $ratio%"

popd > /dev/null
