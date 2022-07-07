# Adding git functions from http://amatsukawa.com/git-branch-command-line.html

parse_git_branch () {
    if git rev-parse --git-dir >/dev/null 2>&1; then
        echo -e \($(git branch 2>/dev/null | sed -n '/^\*/s/^\* //p')\)
    fi
}

git_color () {
    local status=`git status 2>&1`
    if [[ "$status" == *'Not a git repository'* ]]; then
        echo ""
    else
        if [[ "$status" != *'working'*'clean' ]]; then
            echo -e '\e[0;31m'      # red if need to commit
        else
            if [[ "$status" == *'Your branch is ahead'* ]]; then
                echo -e '\e[0;33m'  # yellow if need to push
            else
                echo -e '\e[0;36m'  # else cyan
            fi
        fi
    fi
}
