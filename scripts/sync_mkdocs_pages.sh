#!/bin/bash

pushd $(dirname $0) > /dev/null

cd $(pwd -P)/..

YAMLS=`ls doc/mkdocs/*.yml | grep -v mkdocs.yml`
ALL_PAGES=(`IFS=$'\n' grep -o "'[^' ]*\.md'" doc/mkdocs/mkdocs.yml | sort | uniq`)

add_missing_section_header() {
    local yaml=$1
    local section="# OTHER AVAILABLE PAGES TO ADD"

    if ! grep -q "$section" "$yaml" ; then
        echo >> $yaml
        echo >> $yaml
        echo "#################### DO NOT TOUCH BELOW LINES ####################" >> $yaml
        echo "$section" >> $yaml
        echo "# AUTOMATICALLY UPDATES BY 'scripts/sync_mkdocs_pages.sh'" >> $yaml
        echo >> $yaml
    fi
}

add_missing_page() {
    local yaml=$1
    local to_add="`echo $2 | tr ' ' '\n' | sed -r 's/^/# /'`"

    add_missing_section_header $yaml
    echo "$to_add" >> "$yaml"
}

add_missing_pages() {
    local yaml=$1
    local cur_pages=(${!2})


    local missing_pages="`comm -1 <(printf '%s\n' ${cur_pages[@]}) <(printf '%s\n' ${ALL_PAGES[@]}) | grep -E "^'"`"
    [ -n "$missing_pages" ] && add_missing_page $yaml "$missing_pages"
}

add_deleted_notice() {
    local yaml=$1
    local cur_pages=(${!2})

    local deleted="`comm -2 <(printf '%s\n' ${cur_pages[@]}) <(printf '%s\n' ${ALL_PAGES[@]}) | grep -E "^'"`"
    if [ -n "$deleted" ]; then
        local patterns="$(echo "$deleted" | sed -r 's|/|\\/|g')"
        echo "$patterns"
        for pattern in $patterns ; do
            sed -ri "/$pattern/ s/( ### This file is DELETED or RENAMED|$)/ ### This file is DELETED or RENAMED/" $yaml
        done
    fi
}


for yaml in $YAMLS ; do
    echo "fixing $yaml"
    greped=(`IFS=$'\n' grep -o "'[^']*\.md'" $yaml | sort | uniq`)
    add_missing_pages $yaml greped[@]
    add_deleted_notice $yaml greped[@]
done

popd > /dev/null
