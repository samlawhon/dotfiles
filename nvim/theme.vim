" =============================================================================
"                                 VIM THEME
" =============================================================================

set encoding=utf-8
" set term=screen-256color  " use this in terminal or terminal emulators
set t_co=256
set background=dark
set noerrorbells visualbell t_vb=
set nu
set guifont=Droid_Sans_Mono_Dotted_for_Powe:h9:cANSI:qDRAFT
set autoread
set cursorline

augroup gui_bell
  autocmd!
  autocmd guienter * set visualbell t_vb=
augroup end

syntax on
set linebreak
set shiftround
set relativenumber
set nowrap

colo iceberg

if &term =~ '256color'
  " disable background color erase (bce) so that color schemes
  " render properly when inside 256-color tmux and gnu screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif

" Recommended by the nerdtree-git-plugin docs
let g:NERDTreeGitStatusUseNerdFonts = 1

