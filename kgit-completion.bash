_kgit()
{
    local cur prev opts offset i
    _get_comp_words_by_ref cur prev

    offset=0
    for (( i=1; i <= COMP_CWORD; i++ )); do
        # check if we're still completing our options
        case ${COMP_WORDS[$i]} in
            gh|git|hub)
                offset=$((i+1))
                offset=$i
                break
                ;;
        esac
    done

    if [ $offset -gt 0 ]; then
        # echo
        # echo "offset is '$offset'"
        # echo "cur is '$cur'"
        # echo "prev is '$prev'"
        # echo "COMP_WORDS '${COMP_WORDS[*]}'"
        # echo "length of COMP_WORDS '${#COMP_WORDS[@]}'"
        # echo "word at offset is '${COMP_WORDS[$offset]}'"
        # echo
        _command_offset $offset
    else
        COMPREPLY=()

        opts="
        -kapps
        -a -all-apps
        -A -all
        -kcore
        -kroot
        -kchanged
        -help
        -q -qq
        -get-root
        -exit-on-error
        gh
        git
        hub"

        case "${prev}" in
            -kapps)
                if [ -d "applications" ]; then
                    COMPREPLY=( $(compgen -W "$(echo applications/*/ | sed -e 's/applications//g' -e 's|/||g')" -- "$cur") )
                else
                    COMPREPLY=( $(compgen -W "blackhole braintree callflow call_inspector cdr conference crossbar doodle \
                        ecallmgr fax hangups hotornot jonny5 media_mgr milliwatt omnipresence pivot pusher registrar reorder \
                        skel stats stepswitch sysconf tasks teletype trunkstore webhooks" -- "$cur") )
                fi
                return
                ;;
            -kzchanged)
                return
                ;;
        esac

        COMPREPLY=( $(compgen -W "${opts}" -- "$cur") )
        return
    fi
} &&
complete -F _kgit kgit
