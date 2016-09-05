# TODO: Figure out how to use the built-in git-prompt.sh

# Adding git functions from http://amatsukawa.com/git-branch-command-line.html
parse_git_branch () {
    if git rev-parse --git-dir >/dev/null 2>&1
    then
	echo -e \($(git branch 2>/dev/null | sed -n '/^\*/s/^\* //p')\)
    fi
}

function git_color {
    local STATUS=`git status 2>&1`
    if [[ "$STATUS" == *'Not a git repository'* ]]
    then
	echo ""
    else
	if [[ "$STATUS" != *'working directory clean'* ]]
	then
	    echo -e '\e[0;31m'		# red if need to commit
	else
	    if [[ "$STATUS" == *'Your branch is ahead'* ]]
	    then
		echo -e '\e[0;33m'	# yellow if need to push
	    else
		echo -e '\e[0;36m'	# else cyan
	    fi
	fi
    fi
}
