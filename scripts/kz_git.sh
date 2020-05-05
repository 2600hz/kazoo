#!/bin/sh

set -e

# macOS/BSD does not support readlink '-f' flag
case $OSTYPE in
    darwin*)
        export READLINK="readlink"
    ;;
    *)
        export READLINK="readlink -f"
    ;;
esac


# set the cwd to KAZOO_ROOT to avoid headaches
pushd "$(dirname "$0")" > /dev/null
ROOT=$($READLINK "$(pwd -P)"/..)
pushd "$ROOT" > /dev/null
([ -d "rel" ] && [ -d "scripts" ]) || (echo "Please run this on KAZOO_SRC root" && exit 1)


# global variables
_extra_args=
_work_dirs=

while [ $# -gt 0 ]; do
    arg="$1"
    case $arg in
        -a|-a=*|--apps|--apps=*)
            if ([ "$arg" = "-a" ] || [ "$arg" = "--apps" ]) ; then
                if [ -n "$2" ]; then
                    arg="$2"
                    shift
                else
                    # echo "$0: error: the following argument required value: --apps"
                    # exit 1
                    shift
                    continue
                fi
            fi
            _kazoo_apps="${arg#*=}"
            _kazoo_apps="$(echo $_kazoo_apps | tr ',' ' ')"
            ;;
        -k|--all-apps)
            include_all_apps=1
            ;;
        -A|--all)
            include_all=1
            ;;
        -c|--core)
            include_core=1
            ;;
        -r|--include-kazoo-root)
            include_root=1
            ;;
        changed|--changed)
            _action="changed"
            ;;
        --)
            # explicit end of options
            explicit_opts_end=1
            shift
            break
            ;;
        *)
            if [ -z "$_extra_args" ]; then
                _extra_args="$arg"
            else
                _extra_args="$_extra_args $arg"
            fi
            ;;
    esac
    shift
done
unset arg

if [ -z "$explicit_opts_end" ]; then
    _git_args="$_extra_args"
else
    _git_args="$@"
fi

add_work_dir() {
    if [ ! -d "$1" ]; then
        echo "directory $1 doesn't exists!" >&2
        exit 1
    fi
    if [ -z "$_work_dirs" ]; then
        _work_dirs="$1"
    else
        _work_dirs="$_work_dirs $1"
    fi
}

remove_path_from_extra_args() {
    _path="$1"
    _extra_args="${_extra_args#$path}"
    _extra_args="${_extra_args#[[:blank:]]}"
    unset _path
}

## TODO: maybe add support so we can just specify multiple paths
## across core and apps repos instead of with the whole app/core
## working directories. (needs something like Bash array to store
## a map of working dir and the relative path in that dir - but
## this way we may lose POSIX compatibility)
parse_paths() {
    if [ -n "$include_all" ]; then
        add_work_dir "applications/*"
        add_work_dir "core"
        echo include_all
        return
    fi
    if [ -n "$include_core" ]; then
        add_work_dir "core"
    fi
    if [ -n "$include_root" ]; then
        echo include_root
        add_work_dir "."
    fi
    if [ -n "$include_all_apps" ]; then
        add_work_dir "applications/*"
    fi
    if [ -n "$explicit_opts_end" ]; then
        for path in $_extra_args; do
            case "$path" in
                core|core/|core/*)
                    add_work_dir "core"
                    remove_path_from_extra_args "$path"
                    ;;
                applications|applications/)
                    add_work_dir "applications/*"
                    remove_path_from_extra_args "$path"
                    ;;
                applications/*)
                    tmp_path="${path#applications/}"
                    add_work_dir "applications/${tmp_path%%/*}"
                    remove_path_from_extra_args "$path"
                    unset tmp_path
                    ;;
                *)
                    if [ -e "$path" ]; then
                        add_work_dir "."
                        remove_path_from_extra_args "$path"
                    fi
                    ;;
            esac
        done
        unset path
    fi
    for app in $_kazoo_apps; do
        add_work_dir "applications/$app"
    done
    unset app

    if [ -z "$_work_dirs" ]; then
        _work_dirs=". core applications/*"
    fi

    _work_dirs="$(echo $_work_dirs | tr ' ' '\n' | sort -u | tr '\n' ' ')"
}

parse_paths

if [ -z "$_work_dirs" ]; then
    echo "no working directories were given" >&2
    exit 1
fi

action_changed() {
    get_changed() {
        diff=""
        for file in $(git -C $1 --no-pager diff --name-only HEAD); do
            diff+=" $ROOT/$file"
        done
        echo "$diff"
    }
    changed=""
    for directory in $_work_dirs; do
        dir_change=$(get_changed "$directory")
        changed+=" $dir_change"
    done

    echo "$changed"
}

action_git() {
    if [ -z "$_git_args" ]; then
        echo "no git command was given" >&2
        exit 1
    fi

    for dir in $_work_dirs; do
        git -C "$dir" "$_git_args"
    done
}

case "$_action" in
    changed)
        action_changed
        ;;
    *)
        action_git
        ;;
esac
