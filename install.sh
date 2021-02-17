#!/usr/bin/bash

rm -rf $HOME/.emacs.d
mkdir $HOME/.emacs.d

for filename in .emacs.d/* .bashrc .profile .vimrc .pylintrc
do
    ln -nfs $(pwd)/$filename $HOME/$filename
done

