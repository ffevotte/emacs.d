if [ -z "$EMACS_USER_DIR" ]; then
    export EMACS_USER_DIR="$(readlink -f $(dirname ${BASH_SOURCE[0]}))"
    export PATH="${EMACS_USER_DIR}/bin:${PATH}"
fi

if [ -z "${EMACS_SERVER}" ]; then
    export EDITOR="emacsclient-main"
else
    export EDITOR="emacsclient -s ${EMACS_SERVER}"

    function E () {
        local CMD
        read -d\0 CMD <<EOF
          (find-file "$1")
EOF
        ${EDITOR} --eval "${CMD}"
    }

    function E-source () {
        local CMD
        read -d\0 CMD <<EOF
          (ff/source "$1")
EOF
        ${EDITOR} --eval "${CMD}"
    }

    function E-man () {
        if [ $# -eq 2 ]; then
            PAGE="$2($1)"
        else
            PAGE="$1"
        fi

        local CMD
        read -d\0 CMD <<EOF
          (man "${PAGE}")
EOF
        ${EDITOR} --eval "${CMD}"
    }

    function E-info () {
        local CMD
        read -d\0 CMD <<EOF
          (info "$1")
EOF
        ${EDITOR} --eval "${CMD}"
    }

    function E-grep () {
        local ARGS
        read -d\0 ARGS < <(
            while [ $# -gt 0 ]; do
                cat <<EOF
                  "$1"
EOF
                shift
            done
        )

        ${EDITOR} --eval "(ff/grep ${ARGS})"
    }
fi

export SVN_EDITOR="$EDITOR"

# Local Variables:
#   mode: shell-script
# End:
