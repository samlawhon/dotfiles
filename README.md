# vimconfig

Storage for .vimrc and other dotfiles.


## Prerequisites

Before you can use this vimrc, make sure to install:

1. [Vundle](https://github.com/VundleVim/Vundle.vim) to
   `~/.vim/bundle/Vundle.vim` so that Plugins can be installed correctly.
2. [Powerline fonts](https://github.com/powerline/fonts). For use with
   [Airline](https://github.com/vim-airline/vim-airline). If on windows,
   check out [these instructions](
   https://medium.com/@slmeng/how-to-install-powerline-fonts-in-windows-b2eedecace58)
   for how to install the powerline fonts.
3. [Python](https://www.python.org) for compiling and using
   [YouCompleteMe](https://github.com/Valloric/YouCompleteMe). Make sure to grab the
   correct version - use ":version" to check:
   
   * 32 or 64 bit
   * Python version (2.7 should always work and is recommended. If you want to use
     Python 3.x, scan the output of ":version" for which version it's looking for.
     For example, if you see something like "python36.dll" in there, then you need
     Python 3.6)
     
4. [CMake](https://cmake.org/download/) to compile
   [YouCompleteMe](https://github.com/Valloric/YouCompleteMe)
