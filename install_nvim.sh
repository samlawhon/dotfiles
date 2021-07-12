cd ~

curl -LO https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage
chmod u+x nvim.appimage

if [[ -z $(grep 'alias nvim=' ~/.bashrc) ]]; then
    echo "alias nvim='~/nvim.appimage'" >> ~/.bashrc
else
    echo "alias for 'nvim' already in ~/.bashrc"
fi

sudo npm install -g bash-language-server
sudo npm install -g pyright

git clone --recursive git@github.com:renzmann/.vim ~/.vim
git clone --recursive git@github.com:renzmann/config-nvim ~/.config/nvim

python3 -m venv .nvim.venv
source ./.nvim.venv/bin/activate
pip install black
pip install neovim
pip install flake8
deactivate

cd -
