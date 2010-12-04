alias gs='git status'
alias ga='git add .'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git log'

alias push='git push origin master'
alias pull='git pull origin master'

#alias gc='git commit -m'
alias gac='git commit -am'

function take() {
   mkdir $1
   cd $1
}