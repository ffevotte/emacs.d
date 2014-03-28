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
fi

export SVN_EDITOR="$EDITOR"

# Local Variables:
#   mode: shell-script
# End:
