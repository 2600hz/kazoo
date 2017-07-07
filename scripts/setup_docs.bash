#!/bin/bash
## sets up the docs environment to serve mkdocs-built site

pushd $(dirname $0) > /dev/null

cd $(pwd -P)/..
DOCS_ROOT=`readlink -f ./doc/mkdocs`

find {scripts,doc,core,applications} -type f -path "doc/mkdocs*" -prune -o -regex ".+\.\(md\|png\|jpg\|svg|json\)$" -print | cpio -p -dum --quiet $DOCS_ROOT/docs

popd > /dev/null
