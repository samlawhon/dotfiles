REPORTTIME=5
alias ta="tmux a"
alias lso="ls -lh output/"
LESS="-SR"

if [! -x ~/.fzf ]; then
  git clone --depth 1 git@github.com:junegunn/fzf.git ~/.fzf
  ~/.fzf/install
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

