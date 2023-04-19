# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Terminal
shopt -s histappend     # append to the history file, don't overwrite it
HISTCONTROL=ignoreboth  # don't put duplicate lines or lines starting with space in the history
HISTFILESIZE=4000       # maximum 4000 lines of history
HISTSIZE=2000           # maximum 2000 commands of history in memory
shopt -s checkwinsize   # check the window size after a process completes
shopt -s extglob        # allow for extended pattern matching features
shopt -s autocd         # cd to bare directory names
shopt -s cdspell        # autocorrect for cd
shopt -s dirspell       # autocorrect for all directory names
EDITOR="vim"

# macOS
COPYFILE_DISABLE=1                  # Stop including ._ files
BASH_SILENCE_DEPRECATION_WARNING=1  # Stop recommending zsh

# Emacs
export PATH="$PATH:$HOME/.emacs.d/bin"  # Add doom-emacs to PATH

# Homebrew
if [[ $(uname -m) == 'arm64' ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
else
    eval "$(/usr/local/bin/brew shellenv)"
fi
export HOMEBREW_NO_GITHUB_API=1

# Git
. $HOMEBREW_PREFIX/etc/bash_completion.d/git-completion.bash
alias g='git'
__git_complete g __git_main

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# Python
export TK_SILENCE_DEPRECATION=1

# Aliases
alias rm='rm -i'
alias ll='ls -alF'
alias e='emacs'
# [ -r ~/.bash_aliases ]        && . ~/.bash_aliases
# [ -r ~/private.bash_aliases ] && . ~/private.bash_aliases


##########
# Prompt #
##########

# Restore pyenv virtualenv prompt
function _pyenv_virtualenv_name {
    local version=$(pyenv version-name)
    if ! [[ $version =~ ^"3." ]]; then
        echo -n "($version) "
    fi
}

. $HOMEBREW_PREFIX/etc/bash_completion.d/git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_SHOWUPSTREAM="auto"
GIT_PS1_STATESEPARATOR=""
PROMPT_COMMAND='__git_ps1 "$(_pyenv_virtualenv_hook)$(_pyenv_virtualenv_name)\e[34m\u\e[0m@\e[36m\h\e[0m:\e[34m\W\e[0m" "\e[0;1m \$\e[0m "'


########################################################
# Marks (https://news.ycombinator.com/item?id=6229291) #
########################################################

MARKPATH=$HOME/.marks

function marks {
    # TODO: arrows aren't perfectly lined up
    ls -l $MARKPATH | tr -s ' ' | cut -d ' ' -f 9- | sed s,$HOME,~,';1d;s/ -/	-/;s/^/  /'
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

function jump {
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
alias j="jump"

function _jump {
    local curr=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "$( \ls $MARKPATH )" -- $curr) )
}
complete -o default -o nospace -F _jump jump j unmark
