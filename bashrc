if [ -z "${EMACS_SERVER}" ]; then
    export EDITOR="emacsclient -s server -c -a vim"
else
    export EDITOR="emacsclient -s ${EMACS_SERVER}"

    function E () {
        ${EDITOR} -e "(find-file \"$1\")"
    }

    function E-source () {
        ${EDITOR} -e "(ff/source \"$1\")"
    }

    function man () {
        if [ $# -eq 2 ]; then
            PAGE="$2($1)"
        else
            PAGE="$1"
        fi
        ${EDITOR} --eval "(man \"${PAGE}\")"
    }

    function info () {
        ${EDITOR} --eval "(info \"$1\")"
    }

    function grep () {
        local QUOTED="grep -nH"
        while [ $# -gt 0 ]; do
            QUOTED="${QUOTED} \\\"$1\\\""
            shift
        done
        ${EDITOR} --eval \
            "(grep \"${QUOTED}\")" \
            "(winner-undo)" \
            "(switch-to-buffer \"*grep*\")"
    }
fi

export SVN_EDITOR="$EDITOR"

# Local Variables:
#   mode: shell-script
# End:
