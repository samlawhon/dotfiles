" =============================================================================
"                                 VISUALS
" =============================================================================
set encoding=utf-8
set t_Co=256
set background=dark
set noerrorbells visualbell t_vb=
set nu
set guifont=DejaVu_Sans_Mono_for_Powerline:h9:cANSI:qDRAFT
autocmd GUIEnter * set visualbell t_vb=
syntax on
let g:gruvbox_contrast_dark = 'hard'
colo gruvbox


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
Plugin 'morhetz/gruvbox'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tpope/vim-fugitive'
Plugin 'cjrh/vim-conda'
Plugin 'Raimondi/delimitMate'
"--------------------- Add plugins above this line ---------------------------"
call vundle#end()
filetype plugin indent on


" =============================================================================
"                               PEP 8 MANAGEMENT
" =============================================================================
augroup PEP
    autocmd!
    autocmd BufNewFile,BufRead *.c,*.py,*.h,*.cpp,*.hpp
        \ setlocal tabstop=4 softtabstop=4 shiftwidth=4 textwidth=79 expandtab
        \ autoindent fileformat=unix
augroup END



" =============================================================================
"                           CONVENIENCE MAPS/REMAPS
" =============================================================================
let g:ycm_autoclose_preview_window_after_completion=1
map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>
map <leader>t  :NERDTree<CR>
nnoremap <space> za
vnoremap <space> zf
nnoremap <F2> <Esc>:HardPencil<CR>
nnoremap <F3> <Esc>:NoPencil<CR>
nnoremap <F11> <Esc>:set lines=999<CR>:set columns=999<CR>
nnoremap <leader><Space> :tabnext<CR>


" =============================================================================
"                           AIRLINE CUSTOMIZATION
" =============================================================================
let g:airline_section_x = '%{PencilMode()}'
let g:pencil#mode_indicators = {'hard': 'H', 'auto': 'A', 'soft': 'S', 'off': 'OFF'}
let g:airline_powerline_fonts = 1


" =============================================================================
"                            PYTHON CODE FOLDING
" =============================================================================
autocmd FileType python setlocal foldenable foldmethod=manual
set foldtext=getline(v:foldstart+1)

"
" =============================================================================
"                           GUI OPTIONS (FOR GVIM)
" =============================================================================
set guioptions-=m "menu bar
set guioptions-=T "tool bar
set guioptions-=r "scroll bar
set guioptions-=L "NerdTree bar
