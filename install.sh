#!/bin/bash

rm -rf $HOME/.emacs.d
mkdir $HOME/.emacs.d

for filename in .emacs.d/* .bashrc .profile .vimrc .pylintrc
do
    ln -nfs $(pwd)/$filename $HOME/$filename
done

rm -rf $HOME/.config/nvim
mkdir $HOME/.config/nvim
for filename in nvim/*
do
    ln -nfs $(pwd)/$filename $HOME/.config/$filename
done
