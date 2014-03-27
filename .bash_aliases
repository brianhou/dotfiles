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

# Alias definitions.
alias e='emacs'
alias en='emacs -nw'
alias v='vim'
alias rm='rm -i'
alias g='git'
alias octave='octave --persist'
alias open='xdg-open'

# easily remap keyboard for right control. run again to reverse.
# in other words, if right control isn't acting as expected, remap.
function remap {
    xmodmap -e 'remove Control = Control_R' -e 'keysym Control_R = Menu' -e 'keysym Menu = Control_R' -e 'add Control = Control_R'
}

# quickly navigate your filesystem from the command-line
export MARKPATH=$HOME/.marks
function jump {
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
function mark {
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark {
    rm -i "$MARKPATH/$1"
}
function marks {
    ls -l $MARKPATH | tr -s ' ' | cut -d ' ' -f 9- | sed s,$HOME,~,';1d;s/ -/\t-/;s/^/  /'
}
alias j="jump"

# from hn/item?id=6229291
function _jump {
    local curr=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "$( \ls $MARKPATH )" -- $curr) )
}
complete -o default -o nospace -F _jump jump unmark
