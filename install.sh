#!/bin/bash

function usage {
    echo "./$(basename $0)"
    echo "  -h .......... Show help and exit"
    echo "  -f .......... Rebuild installation directories before linking"
}


optstring=":h:f"


while getopts ${optstring} arg
do
    case ${arg} in
        h)
            usage
            exit 0
            ;;
        f)
            rm -rf $HOME/.emacs.d
            mkdir $HOME/.emacs.d

            rm -rf $HOME/.config/nvim
            mkdir $HOME/.config/nvim
            ;;
        :)
            echo "$0: Must supply an argument to -$OPTARG." >&2
            exit 0
            ;;
        ?)
            echo "invalid option: $arg"
            exit 2
            ;;
    esac
done


for filename in .emacs.d/* .bashrc .profile .pylintrc .ideavimrc
do
    ln -nfs $(pwd)/$filename $HOME/$filename
done


for filename in nvim/*
do
    ln -nfs $(pwd)/$filename $HOME/.config/$filename
done
