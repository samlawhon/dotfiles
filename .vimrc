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
set backspace=indent,eol,start


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


" =============================================================================
"                           GUI OPTIONS (FOR GVIM)
" =============================================================================
set guioptions-=m "menu bar
set guioptions-=T "tool bar
set guioptions-=r "scroll bar
set guioptions-=L "NerdTree bar


" =============================================================================
"                       RESTORE WINDOW SIZE AND POSITION
" =============================================================================
" Taken from https://vim.wikia.com/wiki/Restore_screen_size_and_position

" To enable the saving and restoring of screen positions
let g:screen_size_restore_pos = 1

" To save and restore screen for each Vim instance.
" This is useful if you reoutinely run more than one Vim instance.
" For all Vim to use the same settings, change this to 0
let g:screen_size_by_vim_instance = 1

if has("gui_running")
  function! ScreenFilename()
    if has('amiga')
      return "s:.vimsize"
    elseif has('win32')
      return $HOME.'\_vimsize'
    else
      return $HOME.'/.vimsize'
    endif
  endfunction

  function! ScreenRestore()
    " Restore window size (columns and lines) and position
    " from values stored in vimsize file.
    " Must set font first so columns and lines are based on font size.
    let f = ScreenFilename()
    if has("gui_running") && g:screen_size_restore_pos && filereadable(f)
      let vim_instance = (g:screen_size_by_vim_instance==1?(v:servername):'GVIM')
      for line in readfile(f)
        let sizepos = split(line)
        if len(sizepos) == 5 && sizepos[0] == vim_instance
          silent! execute "set columns=".sizepos[1]." lines=".sizepos[2]
          silent! execute "winpos ".sizepos[3]." ".sizepos[4]
          return
        endif
      endfor
    endif
  endfunction

  function! ScreenSave()
    " Save window size and position.
    if has("gui_running") && g:screen_size_restore_pos
      let vim_instance = (g:screen_size_by_vim_instance==1?(v:servername):'GVIM')
      let data = vim_instance . ' ' . &columns . ' ' . &lines . ' ' .
            \ (getwinposx()<0?0:getwinposx()) . ' ' .
            \ (getwinposy()<0?0:getwinposy())
      let f = ScreenFilename()
      if filereadable(f)
        let lines = readfile(f)
        call filter(lines, "v:val !~ '^" . vim_instance . "\\>'")
        call add(lines, data)
      else
        let lines = [data]
      endif
      call writefile(lines, f)
    endif
  endfunction

  if !exists('g:screen_size_restore_pos')
    let g:screen_size_restore_pos = 1
  endif
  if !exists('g:screen_size_by_vim_instance')
    let g:screen_size_by_vim_instance = 1
  endif
  autocmd VimEnter * if g:screen_size_restore_pos == 1 | call ScreenRestore() | endif
  autocmd VimLeavePre * if g:screen_size_restore_pos == 1 | call ScreenSave() | endif
endif
