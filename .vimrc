" Visuals
set t_Co=256
set background=dark
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=
syntax on


" Plugin Management

set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Add plugins below this line
Plugin 'gmarik/Vundle.vim'
Plugin 'Valloric/YouCompleteMe'

" Add plugins above this line
call vundle#end()
filetype plugin indent on


" PEP 8 Checks
au BufNewFile,BufRead *.py set tabstop=4 softtabstop=4 shiftwidth=4 textwidth=79 expandtab autoindent fileformat=unix

set encoding=utf-8
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>
