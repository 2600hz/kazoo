#!/bin/bash

CURRENT_BRANCH=""

fWelcome() {
    clear
    echo "======================================================="
    echo " _  _  _ _     _ _____    _    _______ _       _______ "
    echo "| || || | |   | (_____)  | |  (_______) |     (_______)"
    echo "| || || | |__ | |  _      \ \  _      | |      _____   "
    echo "| ||_|| |  __)| | | |      \ \| |     | |     |  ___)  "
    echo "| |___| | |   | |_| |_ _____) ) |_____| |_____| |_____ "
    echo " \______|_|   |_(_____|______/ \______)_______)_______)"
    echo " - - - Signaling the start of next generation telephony"
    echo "======================================================="
    printf '%56s' "Current Branch: $CURRENT_BRANCH"
    echo
}

fMaintance() {
    echo "# git gc"
    git gc
}

fStash() {
    if [ ! -z "$(git diff-files)" ]; then
        echo "# git stash"
        git stash || fStashFail
        trap "fCleanup $CURRENT_BRANCH TRUE" 0 1 2 5 15
    else
        trap "fCleanup $CURRENT_BRANCH FALSE" 0 1 2 5 15
    fi
}

fStashFail() {
    echo "!!! ERROR:LOCAL MODIFICATIONS COULD NOT BE STORED !!!"
    echo
    echo "No changes have been made to your source code, but we"
    echo "could not safely continue.  Ensure the source code is"
    echo "under GIT control or check the man page for git-stash."
    echo    
    exit 1
}

fFetch() {
    echo "# git fetch"
    git fetch || fFetchFail
    return $?
}

fFetchFail() {
    echo
    echo "       !!! ERROR:COULD NOT RETRIEVE UPDATES !!!"
    echo
    echo "No updates have been made to your source code, but we"
    echo "could not safely continue.  Ensure you have a working"
    echo "internet connection on this box and you can ping"
    echo "source.2600hz.org"
    echo    
    exit 1
}

fCheckout() {
    [ "$1" == "`git status | grep 'On branch' | cut -d ' ' -f 4`" ] && return 0
    echo "# git checkout $1"
    git checkout $1 || fCheckoutFail
    return $?
}

fCheckoutFail() {
    echo
    echo "     !!! ERROR:COULD NOT CHECKOUT BRANCH !!!"
    echo
    echo "The attempt to switch branches has failed, this has"
    echo "caused the update processes to stop.  Please address"
    echo "the error above and try again."
    echo    
    exit 1
}

fMerge() {
    echo "# git merge $1"
    git merge $1 || fMergeFail
    return $?
}

fMergeFail() {
    echo
    echo "       !!! ERROR:COULD NOT MERGE UPDATES !!!"
    echo
    echo "No updates have been made to your source code, but we"
    echo "could not safely continue.  Please resolve the git error"
    echo "above, use git status to get an idea what might be wrong"
    echo
    exit 1
}

fSubModule() {
    echo "# git submodule sync/init/update"
    (git submodule sync && git submodule init && git submodule update  ) || fSubModuleFail
    return $?
}

fSubModuleFail() {
    echo
    echo "      !!! WARNING:COULD NOT UPDATE SUBMODULES !!!"
    echo 
    echo "Whistle does not directly rely on external libraries to"
    echo "operate, but some modules could use them.  You should work to"
    echo "determine what failed and how you can update it manually."
}

fStashPop() {
    echo "# git stash pop"
    retString="`git stash pop 2>&1`"
    retVal=$?

    echo "${retString}"
    if [[ "${retString}" == "Nothing to apply" ]]; then
        return 0
    fi

    if [ $retVal != 0 ]; then fStashPopFail; fi

    return $retVal
}

fStashPopFail() {
    echo
    echo "    !!! ERROR:COULD NOT RESTORE LOCAL CHANGES !!!"
    echo
    echo "Applying the state can fail with conflicts; in this case,"
    echo "you need to resolve the conflicts by hand and call"
    echo "'git stash drop' manually once you are sure you have"
    echo "resolved the conflicts."
    echo
    echo "If there are conflicts with the stashed changes you"
    echo "could try running: git checkout stash@{0} {dir_of_file}"
    echo    
    exit 1
}

fGetCurrentBranch() {
    CURRENT_BRANCH="`git status | grep 'On branch' | cut -d ' ' -f 4`"

    [ -z "$CURRENT_BRANCH" ] && echo "Unable to determine the current git branch, please ensure you are on a local branch before running this command!" && exit 1
}

fMergeAllBranches() {
    while read b; do 
        if r=$(git config --get branch.$b.remote); then 
            m=$(git config --get branch.$b.merge)
            if [ "$b" != "production" -a "$b" != "development" ]; then
                fCheckout $b && fMerge $r/${m##*/} #&& fSubModule
            fi
        fi; 
    done < <(git for-each-ref --format='%(refname:short)' refs/heads/*)
}

fMergeCurrentBranch() {
        b="${CURRENT_BRANCH}"

        if r=$(git config --get branch.$b.remote); then
            m=$(git config --get branch.$b.merge)
            fMerge $r/${m##*/} #&& fSubModule
        fi
}

fCleanup() {
    if [ "$2" == "TRUE" ]; then
        fCheckout $1 && fStashPop
    else
        fCheckout $1
    fi
}

cd `dirname $0`

while [ -n "$*" ]; do
    case "x$1" in
        x--all)
            all_branches="true"
        ;;
    esac
    shift
done

fGetCurrentBranch
fWelcome
fMaintance
fStash
fFetch 

([ -z "${all_branches}" ] && fMergeCurrentBranch) || fMergeAllBranches

echo "Whistle updated, enjoy!"

exit 0
