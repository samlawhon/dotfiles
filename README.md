# vimconfig

Storage for .vimrc, and some notes on troubles encountered with
configuring vim to work with Aanaconda and Python.


## Prerequisites

Before you can use this vimrc, make sure to install:

1. [Vundle](https://github.com/VundleVim/Vundle.vim) to
   `~/.vim/bundle/Vundle.vim` so that Plugins can be installed correctly.
2. [gruvbox](https://github.com/morhets/gruvbox) colorscheme to
   `~/.vim/colors/gruvbox`. (Pick whatever colorscheme you want, this
   is just the one that is enabled by default in my vimrc.)
3. [Powerline fonts](https://github.com/powerline/fonts). For use with
   [Airline](https://github.com/vim-airline/vim-airline). If on windows,
   check out [these instructions](
   https://medium.com/@slmeng/how-to-install-powerline-fonts-in-windows-b2eedecace58)
   for how to install the powerline fonts.


## Installation notes on Windows

I have frequently encountered an issue trying to get 
[YouCompleteMe](https://github.com/Valloric/YouCompleteMe) to play nice 
with gvim. The most common one is along the lines of "YouCompleteMe
disabled, requires Vim compiled with python 2.7+ or 3.5+ support." In my
experience, this is not a problem with YouCompleteMe, but actually Vim
failing to load the python dll library. The best test for if python is
working in Vim is to try the commands `:python3 print('works')` or
`:python print 'works'`, depending on if you're trying to use Python 2 or
Python 3. An error about loading the Python shared library should pop up.
There are a few steps to troubleshooting this:

1. Make sure Vim & Python are both 32 bit or both 64 bit
2. **Check what python version dll Vim is trying to load**. This is the
   one that did it for me. For example, I had Vim 8.0 and python 3.7.
   Vim 8.0, however, was trying to load the python35.dll, which obviously
   didn't exist with my python 3.7 installation. Upgrading Vim to 8.1
   fixed the issue, since Vim 8.1 is looking for "python37.dll". Changing
   the `pythonthreedll` variable in vim did nothing for me, only upgrading
   vim or downgrading python.
