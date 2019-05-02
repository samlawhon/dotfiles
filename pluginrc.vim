" =============================================================================
"                             PLUGIN MANAGEMENT
" =============================================================================
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
"--------------------- Add plugins below this line ---------------------------"
Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/nerdtree'
Plugin 'nvie/vim-flake8'
Plugin 'w0rp/ale'
Plugin 'reedes/vim-pencil'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-commentary'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'cjrh/vim-conda'
Plugin 'bkad/CamelCaseMotion'
Plugin 'morhetz/gruvbox'
"--------------------- add plugins above this line ---------------------------"
call vundle#end()
filetype plugin indent on
