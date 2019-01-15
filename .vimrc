" Visuals
set t_Co=256
set background=dark
set noerrorbells visualbell t_vb=
set nu
set guifont=DejaVu_Sans_Mono_for_Powerline:h9:cANSI:qDRAFT
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
Plugin 'kkoenig/wimproved.vim' "Uncomment this if on Windows
"-----Add plugins above this line-----"

call vundle#end()
filetype plugin indent on


" PEP 8 Checks
au BufNewFile,BufRead *.py
    \ set tabstop=4 softtabstop=4 shiftwidth=4 textwidth=80 expandtab
    \ autoindent fileformat=unix
set encoding=utf-8

" Convenience
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>
map <leader>t  :NERDTree<CR>
nnoremap <F2> <Esc>:HardPencil<CR>
nnoremap <F3> <Esc>:NoPencil<CR>
nnoremap <F11> <Esc>:WToggleFullscreen<CR>
colo gruvbox


" airline
let g:airline_section_x = '%{PencilMode()}'
let g:pencil#mode_indicators = {'hard': 'H', 'auto': 'A', 'soft': 'S', 'off': 'OFF'}

let g:airline_powerline_fonts = 1


" Python code folding
nnoremap <space> za
vnoremap <space> zf
autocmd FileType python setlocal foldenable foldmethod=manual
set foldtext=getline(v:foldstart+1)

" Gui Options
set guioptions-=m "menu bar
set guioptions-=T "tool bar
set guioptions-=r "scroll bar
