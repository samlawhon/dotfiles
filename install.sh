#!/usr/bin/env zsh

cd ~

curl -LO https://github.com/neovim/neovim/releases/download/v0.5.1/nvim.appimage
chmod u+x nvim.appimage

cat ~/dotfiles/.zshrc >> ~/.zshrc

cp ~/dotfiles/.tmux.conf ~

if [ -z $(grep 'alias nvim=' ~/.zshrc) ]; then
    echo "alias nvim='~/nvim.appimage'" >> ~/.zshrc
else
    echo "alias for 'nvim' already in ~/.zshrc"
fi

if [ ! -d ~/.vim ]; then
    echo "Cloning Sam's .vim to ~/.vim"
    git clone git@github.com:samlawhon/.vim.git ~/.vim
elif [ -e ~/.vim/vimrc ] && [ ! -z "$(grep 'Robert A. Enzmann' ~/.vim/vimrc)" ]; then
    echo "Pulling changes from .vim repo"
    cd ~/.vim && git pull
    cd ~
else
    echo "~/.vim already exists and isn't Robb's - not doing anything!"
fi

if [ ! -d ~/.config/nvim ]; then
    git clone --recursive git@github.com:renzmann/config-nvim ~/.config/nvim
elif [ -e ~/.config/nvim/init.vim ] && [ ! -z "$(grep 'Robert A. Enzmann' ~/.config/nvim/init.vim)" ]; then
    echo "Pulling changes from nvim repo"
    cd ~/.vim && git pull
    cd ~
else
    echo "~/.config/nvim already exists and isn't Robb's - not doing anything!"
fi

sudo npm install -g bash-language-server
sudo npm install -g pyright
sudo npm install -g yaml-language-server

if [ ! -d ~/.nvim.venv ]; then
    python3 -m venv .nvim.venv
fi

source ./.nvim.venv/bin/activate && pip install --upgrade pip black neovim flake8 && deactivate

cd $HOME

# Install correct version of fzf
if [ ! -x $HOME/.fzf ]; then
    git clone --depth 1 git@github.com:junegunn/fzf.git ~/.fzf
    ~/.fzf/install
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

source ~/.zshrc

