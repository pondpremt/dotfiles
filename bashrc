# -----------------
# PROMPT FORMATTING
# -----------------

escape () {
    echo "\001$1\002"
}

END=$(escape "\033[0m")
FMT_HOST=$(escape "\033[38;5;214m")
FMT_USER=$(escape "\033[38;5;162m")
FMT_PATH=$(escape "\033[38;5;45m")
FMT_BRANCH=$(escape "\033[38;5;154m")

fmt () {
    echo "$(escape $1)$2$END"
}

fmt_sym () {
    fmt "\033[97m\033[1m" "$1"
}

SYM_BRANCH=$(fmt_sym "|")
SYM_LS=$(fmt_sym "[")
SYM_RS=$(fmt_sym "]")
SYM_PROMPT=$(fmt_sym "→")
SYM_PATH=$(fmt_sym ":")
SYM_HOST=$(fmt_sym "@")

parse_git_branch() {
    # https://coderwall.com/p/fasnya/add-git-branch-name-to-bash-prompt
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1/"
}

parse_git() {
    # TODO add symbol for clean vs dirty repo
    local branch=$(parse_git_branch)
    if [[ -n $branch ]]; then 
        echo -e " $SYM_LS$FMT_BRANCH$branch$SYM_RS"
    else
        echo ""
    fi
}

PS1="[\t] $FMT_USER\u$SYM_HOST$FMT_HOST\h$SYM_PATH$FMT_PATH\w\$(parse_git) $SYM_PROMPT $END"

cd () {
    command pushd "$@" > /dev/null
}

export PATH="$HOME/.cargo/bin:$PATH"
alias scm="/Applications/MIT-GNU-Scheme-10.1.10.app/Contents/Resources/mit-scheme"
