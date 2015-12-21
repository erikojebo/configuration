#Git
alias gs='git status'
alias ga='git add . --all'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git log'

alias push='git push origin master'
alias pull='git pull origin master'

#alias gc='git commit -m'
alias gac='git commit -am'

#Ruby
alias rspec='rspec -cf d'

#Emacs
alias emacs='emacs -nw'

function take() {
   mkdir $1
   cd $1
}