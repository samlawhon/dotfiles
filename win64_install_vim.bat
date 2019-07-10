@echo off
:: General TODOs:
::   - Python version checking and compilation

:: ===========================================================================
:: Setup // variables
:: ===========================================================================

:: remotes
set vimurl="https://github.com/vim/vim"
set mygit="https://github.com/renzmann/dotfiles"
set vundleurl="https://github.com/VundleVim/Vundle.vim"
set fullscreenurl="https://github.com/derekmcloughlin/gvimfullscreen_win32"

:: vim install variables
set pyinstall=%USERPROFILE%\\Anaconda3
set pyver=37

:: dotfile variables
set mygitout=%USERPROFILE%\\dotfiles


:: ===========================================================================
:: Main script
:: ===========================================================================

:: Run from user home
cd %USERPROFILE%

:: compile correct version of vim
git clone %vimurl%
cd vim\\src
nmake -f Make_mvc.mak GUI=yes PYTHON3=%pyinstall% DYNAMIC_PYTHON=yes PYTHON3_VER=%pyver%

:: copy dotfiles/configuration
cd %USERPROFILE%
cp %mygit%\.vimrc %USERPROFILE%
cp %mygit%\.pylintrc %USERPROFILE%


:: Install plugins
:: ---------------

:: Vundle
mkdir .vim
mkdir .vim\\bundle
git clone %vundleurl% %USERPROFILE%\\.vim\\bundle\\Vundle.vim

:: Plugin .vimrc proxy
cd %mygitout%
python copy_plugins.py
cd %USERPROFILE%
gvim -u %mygitout%\\.pluginrc -s %mygitout%\\doplugins.txt

:: Gvim fullscreen
git clone fullscreenurl
cp gvimfullscreen_32\gvimfullscreen_64.dll vim\src

:: Compile YouCompleteMe
cd %USERPROFILE%\\.vim\\bundle\\YouCompleteMe
python install.py
