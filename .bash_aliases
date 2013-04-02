# allows for extended pattern matching features for lsR
shopt -s extglob

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias lsR='ls -R !(venv) --color=auto -B --ignore=\#*\#' # ignores venv folder

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
alias rm='rm -i'
alias ssh='ssh -X'
alias ..='cd ..'

# easily remap keyboard for right control. run again to reverse.
# in other words, if right control isn't acting as expected, remap.
alias remap="xmodmap -e 'remove Control = Control_R' -e 'keysym Control_R = Menu' -e 'keysym Menu = Control_R' -e 'add Control = Control_R'"
alias open='xdg-open'
