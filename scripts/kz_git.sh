#!/usr/bin/env bash

set -e

_info() {
    [ -n "$_quiet" ] && return
    printf "\e[1;36m${0##*/}:\e[1;37m $@ \e[00m\n"
}

_error() {
    [ -n "$_silence" ] && return
    printf "\e[1;36m${0##*/}:\e[1;31m error:\e[1;37m $@ \e[00m\n"
}

_die () {
    _error "$@"
    exit 1
}

# macOS/BSD does not support readlink '-f' flag
another_readlink() {
    TARGET_FILE="$1"

    cd "$(dirname "$TARGET_FILE")"
    TARGET_FILE="$(basename "$TARGET_FILE")"

    # Iterate down a (possible) chain of symlinks
    while [ -L "$TARGET_FILE" ]; do
        TARGET_FILE="$(readlink "$TARGET_FILE")"
        cd "$(dirname "$TARGET_FILE")"
        TARGET_FILE="$(basename "$TARGET_FILE")"
    done

    # Compute the canonicalized name by finding the physical path
    # for the directory we're in and appending the target file.
    PHYS_DIR="$(pwd -P)"
    RESULT="$PHYS_DIR/$TARGET_FILE"
    echo "$RESULT"
}

is_kazoo_src() {
    if [ -d "$1/rel" ] && [ -d "$1/scripts" ]; then
        return 0
    else
        return 1
    fi
}

_script_path="$(another_readlink "$0")"
_root_candidate="$(another_readlink "$_script_path")"
if is_kazoo_src "$_root_candidate/.."; then
    ROOT="$_root_candidate/.."
elif is_kazoo_src "$_script_path"; then
    ROOT="$_script_path"
elif [ -n "$KAZOO_SRC" ] && is_kazoo_src "$KAZOO_SRC"; then
    ROOT="$KAZOO_SRC"
else
    _die "Please place this script on Kazoo root directory or set KAZOO_SRC env variable"
fi
unset _root_candidate
unset _script_path

# set the cwd to kazoo source root to avoid headache
pushd "$ROOT" > /dev/null

# global variables
_kazoo_apps=
_extra_args=
_work_dirs=

usage() {
    cat <<'END' >&2
    __ _______         _ __
   / //_/__  /  ____ _(_) /_
  / ,<    / /  / __ `/ / __/
 / /| |  / /__/ /_/ / / /_
/_/ |_| /____/\__, /_/\__/ ☢
Welcome to   /____/ the FUTURE

A simple wrapper around git and pull request commands
to provide an easy way to develop on Kazoo multi-repo structure

Usage:

        kz_git.sh [OPTIONS]+ <ACTION> [ACTION_ARGS]+

    Where ACTION can be one of these commands to run in specified
    Kazoo source folders:

        git        Runs git command
        gh         Runs gh command (https://cli.github.com)
        hub        Runs hub command (https://github.com/github/hub)

Example:

    kz_git.sh -kapps crossbar,teletype git status --branch --short

Options:

    -kapps app_name[,app_name...]:   add a comma separated list of Kazoo apps to working directories list
    -kcore:                          add "core" directory to working directories list
    -kroot:                          add kazoo source root directory to working directories list
    -a, --all-apps:                  add all Kazoo apps to working directories list
    -A, -All:                        add "core", all kazoo apps and kazoo source root directory to
                                         list of working directories. This option is default.
    -kchanged:                       Loop over working directories and prints the name of files which are
                                         different than "$BASE_BRANCH". "master" is the default BASE_BRANCH
    -q                               be quiet, but still print error messages
    -qq                              be more quiet, also do not print errors
    -help:                           shows help
END
}

add_kazoo_apps() {
    if [ -z "$_kazoo_apps" ]; then
        _kazoo_apps="$1"
    else
        _kazoo_apps="$_kazoo_apps $1"
    fi
}

while [ $# -gt 0 ]; do
    arg="$1"
    case $arg in
        -kapps)
            if [ "$arg" = "-kapps" ]; then
                if [ -n "$2" ]; then
                    arg="$2"
                    shift
                else
                    shift
                    continue
                fi
            fi
            # this is to support -kapps= but not any more
            arg="${arg#*=}"
            add_kazoo_apps "$(echo $arg | tr ',' ' ')"
            ;;
        -a|-all-apps)
            include_all_apps=1
            ;;
        -A|-all)
            include_all=1
            ;;
        -kcore)
            include_core=1
            ;;
        -kroot)
            include_root=1
            ;;
        -kchanged)
            _action="changed"
            # TODO: maybe read next args for diff-ing git branch A B
            # explicit end of options
            explicit_opts_end=1
            shift
            break
            ;;
        -help)
            usage
            exit 0
            ;;
        -q)
            _quiet=1
            ;;
        -qq)
            _silence=1
            ;;
        gh)
            _action="gh"
            # explicit end of options
            explicit_opts_end=1
            shift
            break
            ;;
        git)
            _action="git"
            # explicit end of options
            explicit_opts_end=1
            shift
            break
            ;;
        hub)
            _action="hub"
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

if [ -z "$_action" ]; then
    usage && exit 1
fi

# explicit_opts_end is for times that we want to have
# more complex options to use.
if [ -z "$explicit_opts_end" ]; then
    _action_args="$_extra_args"
else
    _action_args="$@"
fi

add_work_dir() {
    local path
    while [ $# -gt 0 ]; do
        path="$1"
        [ -f "$1" ] && shift && continue
        [ ! -d "$1" ] && _die "directory $1 doesn't exists!"
        if [ -z "$_work_dirs" ]; then
            _work_dirs="$1"
        else
            _work_dirs="$_work_dirs $1"
        fi
        shift
    done
}

parse_paths() {
    if [ -n "$include_all" ]; then
        add_work_dir applications/*
        add_work_dir "core"
        add_work_dir "."
        return
    fi

    [ -n "$include_core" ] && add_work_dir "core"
    [ -n "$include_root" ] && add_work_dir "."
    [ -n "$include_all_apps" ] && add_work_dir applications/*

    for app in $_kazoo_apps; do
        add_work_dir "applications/$app"
    done
    unset app

    if [ -z "$_work_dirs" ]; then
        add_work_dir applications/*
        add_work_dir "core"
        add_work_dir "."
    fi

    _work_dirs="$(echo $_work_dirs | tr ' ' '\n' | sort -u | tr '\n' ' ')"
}

parse_paths

if [ -z "$_work_dirs" ]; then
    _die "no working directories were given"
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

check_action() {
    if [ -n "$_action" ] && [ -z "$(type "$_action" 2>/dev/null)" ]; then
        _error "$_action command not found"
        [ -n "$_silence" ] && exit 1
        echo >&2
        case "$_action" in
            gh)
                echo "Please follow instructions in https://cli.github.com to install it." >&2
                ;;
            git)
                echo "Please install git version 2+ using your distro package manager." >&2
                ;;
            hub)
                echo "Please follow instructions in https://github.com/github/hub#installation to install it." >&2
                ;;
        esac
        exit 1
    fi

    if [ -z "$_action_args" ]; then
        $_action
        exit 0
    fi
}

action_git() {
    check_action
    for dir in $_work_dirs; do
        _info "Running git in $dir"

        git -C "$dir" $_action_args

        if [ $? -ne 0 ] ; then
            _die "me git failed"
        fi
    done
}

loop_push_action() {
    check_action
    for dir in $_work_dirs; do
        _info ":: Running $_action in $dir"
        pushd "$dir" >/dev/null

        $_action $_action_args

        if [ $? -ne 0 ] && [ -z "$_continue_on_failure" ]; then
            _die "me $_action failed"
        fi
        popd >/dev/null
    done
}


case "$_action" in
    changed)
        action_changed
        ;;
    gh)
        loop_push_action
        ;;
    git)
        action_git
        ;;
    hub)
        loop_push_action
        ;;
esac
