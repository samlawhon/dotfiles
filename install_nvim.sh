cd ~
curl -LO https://github.com/neovim/neovim/releases/download/nightly/nvim.appimage
chmod u+x nvim.appimage
echo "alias nvim='~/nvim.appimage'" >> ~/.bashrc

sudo npm install -g bash-language-server
sudo npm install -g pyright
cd -
