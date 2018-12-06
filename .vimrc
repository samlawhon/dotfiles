" Visuals
set t_Co=256
set background=dark
set noerrorbells visualbell t_vb=
set nu
set guifont=SAS_Monospace:h9:cANSI:qDRAFT
autocmd GUIEnter * set visualbell t_vb=
syntax on


" Plugin Management
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

"-----Add plugins below this line-----"
Plugin 'gmarik/Vundle.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/nerdtree'
Plugin 'nvie/vim-flake8'
Plugin 'w0rp/ale'
Plugin 'reedes/vim-pencil'
" Plugin 'tmhedberg/SimpylFold'
Plugin 'morhetz/gruvbox'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tpope/vim-fugitive'
Plugin 'cjrh/vim-conda'
"-----Add plugins above this line-----"

call vundle#end()
filetype plugin indent on


" PEP 8 Checks
au BufNewFile,BufRead *.py
    \ set tabstop=4 softtabstop=4 shiftwidth=4 textwidth=79 expandtab
    \ autoindent fileformat=unix
set encoding=utf-8

" Convenience
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>
map <leader>t  :NERDTree<CR>
colo gruvbox


" Python code folding
nnoremap <space> za
vnoremap <space> zf
autocmd FileType python setlocal foldenable foldmethod=manual
set foldtext=getline(v:foldstart+1)
