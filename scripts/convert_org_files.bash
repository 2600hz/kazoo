#!/bin/bash

for orgfile in $(find {doc,core,applications} -type f -name "*.org"); do
    echo "updating $orgfile"
    # Convert latex blocks
    sed -i 's/#+BEGIN_LaTeX/#+BEGIN_EXPORT latex/gi' $orgfile
    sed -i 's/#+END_LaTeX/#+END_EXPORT/gi' $orgfile

    # Convert setupfile to include
    sed -i 's/#+SETUPFILE/#+include:/gi' $orgfile
done
