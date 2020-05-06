#!/usr/bin/env bash

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

_info() {
    printf "\e[1;36m${0##*/}:\e[1;37m $1\e[00m\n"
}

_error() {
    printf "\e[1;36m${0##*/}:\e[1;31m error:\e[1;37m $1\e[00m\n"
}

_die () {
    _error "$1"
    exit 1
}

# set the cwd to KAZOO_ROOT to avoid headaches
pushd "$(dirname "$0")" > /dev/null
ROOT=$($READLINK "$(pwd -P)"/..)
pushd "$ROOT" > /dev/null
([ -d "rel" ] && [ -d "scripts" ]) || _die "Please run this on KAZOO_SRC root"


# global variables
_kazoo_apps=
_extra_args=
_work_dirs=

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
        -kcore|-kazoo-core)
            include_core=1
            ;;
        -kroot|-kazoo-root)
            include_root=1
            ;;
        -kchanged)
            _action="changed"
            # TODO: maybe read next args for diffing git branch A B
            # explicit end of options
            explicit_opts_end=1
            shift
            break
            ;;
        gh|-pull_request)
            _action="pull_request"
            # explicit end of options
            explicit_opts_end=1
            shift
            break
            ;;
        git|-git)
            _action="git"
            # explicit end of options
            explicit_opts_end=1
            shift
            break
            ;;
        # we don't need explicit end of options for now
        # --)
        #     # explicit end of options
        #     explicit_opts_end=1
        #     shift
        #     break
        #     ;;
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

# we were using this remove optional paths from _extra_args
# remove_path_from_extra_args() {
#     _path="$1"
#     _extra_args="${_extra_args#$path}"
#     _extra_args="${_extra_args#[[:blank:]]}"
#     unset _path
# }

## TODO: maybe add support so we can just specify multiple paths
## across core and apps repos instead of with the whole app/core
## working directories. (needs something like Bash array to store
## a map of working dir and the relative path in that dir - but
## this way we may lose POSIX compatibility)
parse_paths() {
    if [ -n "$include_all" ]; then
        add_work_dir echo applications/*
        add_work_dir "core"
        add_work_dir "."
        return
    fi

    [ -n "$include_core" ] && add_work_dir "core"
    [ -n "$include_root" ] && add_work_dir "."
    [ -n "$include_all_apps" ] && add_work_dir applications/*

    # The idea was to allow to pass some paths
    # and we generate _work_dirs base on the path to run the command
    # but this makes everything more complex and is not something necessary
    # if [ -n "$explicit_opts_end" ]; then
    #     for path in $_extra_args; do
    #         case "$path" in
    #             core|core/|core/*)
    #                 add_work_dir "core"
    #                 remove_path_from_extra_args "$path"
    #                 ;;
    #             applications|applications/)
    #                 add_work_dir echo applications/*
    #                 remove_path_from_extra_args "$path"
    #                 ;;
    #             applications/*)
    #                 tmp_path="${path#applications/}"
    #                 add_work_dir "applications/${tmp_path%%/*}"
    #                 remove_path_from_extra_args "$path"
    #                 unset tmp_path
    #                 ;;
    #             *)
    #                 if [ -e "$path" ]; then
    #                     add_work_dir "."
    #                     remove_path_from_extra_args "$path"
    #                 fi
    #                 ;;
    #         esac
    #     done
    #     unset path
    # fi
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

action_pull_request() {
    if [ -z "$(type gh 2>/dev/null)"  ]; then
        _error "gh command not found"
        echo >&2
        echo "Please follow https://cli.github.com to install," >&2
        echo "or get the latest release from:" >&2
        latest_url="https://api.github.com/repos/cli/cli/releases/latest"
        echo "$(curl -s $latest_url | grep -E "browser_download_url.*(gh_.*(deb|rpm|tar.gz|msi|zip))" | grep -oE 'https://[^"]+')" >&2
        exit 1
    fi
    if [ -z "$_action_args" ]; then
        _error "no gh command was given"
        gh
        exit 1
    fi

    for dir in $_work_dirs; do
        _info ":: Running gh in $dir"
        pushd "$dir" >/dev/null
        gh $_action_args
        if [ $? -ne 0 ] && [ -z "$_continue_on_failure" ]; then
            _die "me failed"
        fi
        popd >/dev/null
    done
}

action_git() {
    if [ -z "$_action_args" ]; then
        _error "no git command was given"
        git
        exit 1
    fi

    for dir in $_work_dirs; do
        _info "Running git in $dir"
        git -C "$dir" $_action_args
        if [ $? -ne 0 ] ; then
            _die "me git failed"
        fi
    done
}

case "$_action" in
    changed)
        action_changed
        ;;
    pull_request)
        action_pull_request
        ;;
    *)
        action_git
        ;;
esac
