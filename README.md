# vimconfig

Storage for .vimrc, and some notes on troubles encountered with
configuring vim to work with Aanaconda and Python.


## Prerequisites

Before you can use this vimrc, make sure to install:

1. [Vundle](https://github.com/VundleVim/Vundle.vim) to
   `~/.vim/bundle/Vundle.vim` so that Plugins can be installed correctly.
2. [solarized](https://github.com/altercation/vim-colors-solarized) colorscheme to
   `~/.vim/colors/gruvbox`. (Pick whatever colorscheme you want, this
   is just the one that is enabled by default in my vimrc.)
3. [Powerline fonts](https://github.com/powerline/fonts). For use with
   [Airline](https://github.com/vim-airline/vim-airline). If on windows,
   check out [these instructions](
   https://medium.com/@slmeng/how-to-install-powerline-fonts-in-windows-b2eedecace58)
   for how to install the powerline fonts.
4. [Python](https://www.python.org) for compiling and using
   [YouCompleteMe](https://github.com/Valloric/YouCompleteMe). Make sure to grab the
   correct version - use ":version" to check:
   
   * 32 or 64 bit
   * Python version (2.7 should always work and is recommended. If you want to use
     Python 3.x, scan the output of ":version" for which version it's looking for.
     For example, if you see something like "python36.dll" in there, then you need
     Python 3.6)
