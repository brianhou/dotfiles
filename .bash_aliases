shopt -s extglob # allows for extended pattern matching features for lsR
shopt -s autocd
shopt -s cdspell dirspell

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
function lsR {
    ls -R $1!(venv) # ignores venv folder
}

# editor stuff
function start-emacs {
    emacs --daemon
}
function kill-emacs {
    emacsclient -e "(kill-emacs)"
}
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    alias e='emacsclient -c'
elif [[ "$OSTYPE" == "darwin"* ]]; then
    alias e='emacs'
fi
alias en='emacs -nw'
alias v='vim'

# Alias definitions.
alias rm='rm -i'
alias g='git'
if [[ "$OSTYPE" == "linux-gnu" ]]; then
    alias octave='octave --persist'
    alias open='xdg-open'
    alias node='nodejs'

# easily remap keyboard for right control. run again to reverse.
# in other words, if right control isn't acting as expected, remap.
function remap {
    xmodmap -e 'remove Control = Control_R' -e 'keysym Control_R = Menu' -e 'keysym Menu = Control_R' -e 'add Control = Control_R'
}
fi

__git_complete g __git_main

# quickly navigate your filesystem from the command-line
export MARKPATH=$HOME/.marks
function jump {
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
function mark {
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark {
    for mark in "$@"
    do
        rm -i "$MARKPATH/$mark"
    done
}
function marks {
    if [[ "$OSTYPE" == "linux-gnu" ]]; then
        ls -l $MARKPATH | tr -s ' ' | cut -d ' ' -f 9- | sed s,$HOME,~,';1d;s/ -/\t-/;s/^/  /'
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        ls -l $MARKPATH | tr -s ' ' | cut -d ' ' -f 9- | sed s,$HOME,~,';1d;s/ -/	-/;s/^/  /'
    fi
}
alias j="jump"

# from hn/item?id=6229291
function _jump {
    local curr=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "$( \ls $MARKPATH )" -- $curr) )
}
complete -o default -o nospace -F _jump jump j unmark
