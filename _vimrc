" =============================================================================
"                             PLUGIN MANAGEMENT
" =============================================================================
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.vim/colors
call vundle#begin()
"--------------------- Add plugins below this line ---------------------------"
Plugin 'cocopon/iceberg.vim'
""Plugin 'neoclide/coc.nvim'
Plugin 'derekwyatt/vim-scala'
""Plugin 'scalameta/coc-metals'
Plugin 'morhetz/gruvbox'
Plugin 'altercation/solarized'
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
Plugin 'bkad/CamelCaseMotion'
Plugin 'PProvost/vim-ps1'
Plugin 'dhruvasagar/vim-table-mode'
"--------------------- add plugins above this line ---------------------------"
call vundle#end()
filetype plugin indent on


" =============================================================================
"                                 VISUALS
" =============================================================================
set encoding=utf-8
" set term=screen-256color  " use this in terminal or terminal emulators
set t_co=256
set background=dark
set noerrorbells visualbell t_vb=
set nu
set guifont=DejaVu_Sans_Mono_for_Powerline:h11:cANSI:qDRAFT
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

if has("gui_running")
  colo iceberg
endif

if &term =~ '256color'
  " disable background color erase (bce) so that color schemes
  " render properly when inside 256-color tmux and gnu screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif


" =============================================================================
"                             FILETYPE AUTOGROUPS
" =============================================================================
augroup pep
  autocmd!
  autocmd filetype python,c,cpp,vim
    \ setlocal tabstop=4 softtabstop=4 shiftwidth=4 textwidth=79 expandtab
    \ autoindent fileformat=unix
augroup end


" =============================================================================
"                           CONVENIENCE MAPS/REMAPS
" =============================================================================
nnoremap <C-E> 2<C-E>
nnoremap <C-Y> 2<C-Y>
let g:ycm_autoclose_preview_window_after_completion=1
set backspace=indent,eol,start
set foldtext=getline(v:foldstart+1)
let mapleader=" "
nnoremap <Space> <Nop>

" GLOBAL
" -----------------------------------------------------------------------------
" Getting around
nnoremap <Leader>g            :YcmCompleter GoToDefinitionElseDeclaration<cr>
nnoremap <F9>                 :NERDTree<CR>
nnoremap <F10>                :NERDTreeFromBookmark<Space>
nnoremap <Leader>j            /# --------------------<CR>zt
nnoremap <Leader>k            ?# --------------------<CR>zt
nnoremap <A-l>                <C-w>l
nnoremap <A-h>                <C-w>h
nnoremap <A-k>                <C-w>k
nnoremap <A-j>                <C-w>j
nnoremap <S-ScrollWheelUp>    <ScrollWheelLeft>
nnoremap <S-ScrollWheelDown>  <ScrollWheelRight>
map <S-Down>                  ]mzz
map <S-Up>                    [mzz

" Editing
" TODO can we do this C-movement in visual mode for multiple lines?
nnoremap <C-Up>               ddkP
nnoremap <C-Down>             ddp
inoremap <C-J>                <Esc>viwUea
nnoremap <Leader>"            viw<Esc>a"<Esc>bi"<Esc>lel
noremap  <Leader>c            :Commentary<CR>
onoremap p                    i(
onoremap in(                  :<C-U>normal! f(vi(<CR>
onoremap in[                  :<C-U>normal! f[vi[<CR>
onoremap il(                  :<C-U>normal! F)vi(<CR>
onoremap il[                  :<C-U>normal! F]vi[<CR>
onoremap i;                   :<C-U>execute "normal! v/;\rhs"<CR>
inoremap {                    {}<Left>
inoremap {<CR>                {<CR>}<Esc>O
inoremap {{                   {
inoremap {}                   {}
inoremap (                    ()<Left>
inoremap <expr> )  strpart(getline('.'), col('.')-1, 1) == ")" ? "\<Right>" : ")"
inoremap [                    []<Left>
inoremap <expr> ]  strpart(getline('.'), col('.')-1, 1) == "]" ? "\<Right>" : "]"
inoremap <expr> ' strpart(getline('.'), col('.')-1, 1) == "\'" ? "\<Right>" : "\'\'\<Left>"
inoremap <expr> " strpart(getline('.'), col('.')-1, 1) == "\"" ? "\<Right>" : "\"\"\<Left>"

" Camel Motion
map <silent> w <Plug>CamelCaseMotion_w
map <silent> b <Plug>CamelCaseMotion_b
map <silent> e <Plug>CamelCaseMotion_e
map <silent> ge <Plug>CamelCaseMotion_ge
sunmap w
sunmap b
sunmap e
sunmap ge

" Markdown and RST headers
nnoremap <Leader>1            yypVr=
nnoremap <Leader>2            yypVr-
nnoremap <Leader>3            yypVr+
nnoremap <Leader>4            yypVr*
onoremap ih                   :<C-U>execute "normal! ?^==\\+$\r:nohlsearch\rkvg_"<CR>

" Code Folding
nnoremap \                    za
vnoremap \                    zf

" Refresh or edit .vimrc
nnoremap <F12>                :tabe $MYVIMRC<CR>
nnoremap <C-F12>              :e $MYVIMRC<CR>
nnoremap <S-F12>              :source $MYVIMRC<CR>
nnoremap <C-S-F12>            :vs $MYVIMRC<CR>

" Exit or save and exit
nnoremap <F8>                 :q<CR>
nnoremap <S-F8>               :wq<CR>

" Git commands
nnoremap <F5>                 :Gstatus<CR><C-w><S-l> <bar> :exe "vertical res 50"<CR>

" Fullscreen mode for gui
" nnoremap <F11>                :set lines=999<CR>:set columns=999<CR>
nnoremap <F11>                <Esc>:call libcallnr("gvimfullscreen.dll", "ToggleFullScreen", 0)<CR>

" Tab Movement
nnoremap <C-Tab>              :tabn<CR>
nnoremap <C-S-Tab>            :tabp<CR>
nnoremap <F1>                 1gt
nnoremap <F2>                 2gt
nnoremap <F3>                 3gt
nnoremap <F4>                 4gt
nnoremap <S-F1>               5gt
nnoremap <S-F2>               6gt
nnoremap <S-F3>               7gt
nnoremap <S-F4>               8gt

" Execute code
nnoremap <Leader>rs            :silent !sas <C-R>%<CR> <bar> :exe "e ".expand("%:r").".lst"<CR>
nnoremap <Leader>rp            :silent !python <C-R>%<CR>

" View logs and output
" TODO this conflicts with table-mode, so we need to find a better map
" nnoremap <Leader>l             :exe "e ".expand("%:r").".log"<CR>
nnoremap <Leader>o             :exe "e ".expand("%:r").".lst"<CR>
nnoremap <Leader>m             :messages<CR>

" SAS Commands
nnoremap <Leader>se            :exe "call SASErrors()"<CR>
nnoremap <Leader>sk            :exe "e ".expand("%:r").".sas"<CR>


" =============================================================================
"                           AIRLINE CUSTOMIZATION
" =============================================================================
let g:airline_section_x = '%{PencilMode()}'
let g:pencil#mode_indicators = {'hard': 'H', 'auto': 'A',
                              \ 'soft': 'S', 'off': 'OFF'}
let g:airline_powerline_fonts = 1



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

  augroup resize_
    autocmd!
    autocmd VimEnter * if g:screen_size_restore_pos == 1 | call ScreenRestore() | endif
    autocmd VimLeavePre * if g:screen_size_restore_pos == 1 | call ScreenSave() | endif
  augroup END
endif

" =============================================================================
"                            LANGUAGE CONFIGURATION
" =============================================================================
"
" Default
" -------
augroup pep8
  autocmd!
  autocmd BufWritePre * %s/\s\+$//e
  autocmd FileType python,java setlocal foldenable foldmethod=manual tabstop=4
    \ softtabstop=4 shiftwidth=4 textwidth=79 expandtab autoindent nohlsearch
augroup END


" R
" -
augroup r_lang
  autocmd!
  autocmd FileType r,vim setlocal foldenable foldmethod=manual
    \ tabstop=2 softtabstop=2 shiftwidth=2 textwidth=79 expandtab
    \ autoindent fileformat=unix nohlsearch
augroup END

" Vim
" ---
augroup vim_lang
  autocmd!
  autocmd FileType vim setlocal foldenable foldmethod=manual
    \ tabstop=2 softtabstop=2 shiftwidth=2 expandtab
    \ autoindent fileformat=unix nohlsearch
augroup END

" SAS
" ---
function! SASErrors()
  try
    exe "%s/ERROR: //gn"
    setlocal hlsearch
  catch /.*/
    echo "No errors."
    setlocal nohlsearch
  endtry
endfunction

augroup SAS
  autocmd!
  autocmd BufRead *.lst setlocal nowrap
augroup END


" sbt/Scala
augroup scala_lang
  au BufRead,BufNewFile *.sbt set filetype=scala
  au FileType json syntax match Comment +\/\/.\+$+
  au FileType scala setlocal foldenable foldmethod=manual tabstop=2
    \ softtabstop=2 shiftwidth=2 textwidth=79 expandtab autoindent nohlsearch
augroup END

" =============================================================================
"                            VIM-SCALA CONFIGURATION
" =============================================================================
" set hidden
"
"" Some servers have issues with backup files
"set nobackup
"set nowritebackup
"
"" Better display for messages
"set cmdheight=2
"
"" You will have a bad experience with diagnostic messages with the default 4000.
"set updatetime=300
"
"" Don't give |ins-completion-menu| messages.
"set shortmess+=c
"
"" Always show signcolumns
"set signcolumn=yes
"
"" Use tab for trigger completion with characters ahead and navigate.
"" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
"inoremap <silent><expr> <TAB>
"      \ pumvisible() ? "\<C-n>" :
"      \ <SID>check_back_space() ? "\<TAB>" :
"      \ coc#refresh()
"inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
"
"" Used in the tab autocompletion for coc
"function! s:check_back_space() abort
"  let col = col('.') - 1
"  return !col || getline('.')[col - 1]  =~# '\s'
"endfunction
"
"" Use <c-space> to trigger completion.
"inoremap <silent><expr> <c-space> coc#refresh()
"
"" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
"" Coc only does snippet and additional edit on confirm.
"inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
"
"" Use `[c` and `]c` to navigate diagnostics
"nmap <silent> [c <Plug>(coc-diagnostic-prev)
"nmap <silent> ]c <Plug>(coc-diagnostic-next)
"
"" Remap keys for gotos
"nmap <silent> gd <Plug>(coc-definition)
"nmap <silent> gy <Plug>(coc-type-definition)
"nmap <silent> gi <Plug>(coc-implementation)
"nmap <silent> gr <Plug>(coc-references)
"
"" Used to expand decorations in worksheets
"nmap <Leader>ws <Plug>(coc-metals-expand-decoration)
"
"" Use K to either doHover or show documentation in preview window
"nnoremap <silent> K :call <SID>show_documentation()<CR>
"
"function! s:show_documentation()
"  if (index(['vim','help'], &filetype) >= 0)
"    execute 'h '.expand('<cword>')
"  else
"    call CocAction('doHover')
"  endif
"endfunction
"
"" Highlight symbol under cursor on CursorHold
"autocmd CursorHold * silent call CocActionAsync('highlight')
"
"" Remap for rename current word
"nmap <leader>rn <Plug>(coc-rename)
"
"" Remap for format selected region
"xmap <leader>f  <Plug>(coc-format-selected)
"nmap <leader>f  <Plug>(coc-format-selected)
"
"augroup mygroup
"  autocmd!
"  " Setup formatexpr specified filetype(s).
"  autocmd FileType scala setl formatexpr=CocAction('formatSelected')
"  " Update signature help on jump placeholder
"  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
"augroup end
"
"" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
"xmap <leader>a  <Plug>(coc-codeaction-selected)
"nmap <leader>a  <Plug>(coc-codeaction-selected)
"
"" Remap for do codeAction of current line
"nmap <leader>ac  <Plug>(coc-codeaction)
"" Fix autofix problem of current line
"nmap <leader>qf  <Plug>(coc-fix-current)
"
"" Use `:Format` to format current buffer
"command! -nargs=0 Format :call CocAction('format')
"
"" Use `:Fold` to fold current buffer
"command! -nargs=? Fold :call     CocAction('fold', <f-args>)
"
"" Add status line support, for integration with other plugin, checkout `:h coc-status`
"set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}
"
"" Show all diagnostics
"nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
"" Manage extensions
"nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
"" Show commands
"nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
"" Find symbol of current document
"nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
"" Search workspace symbols
"nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
"" Do default action for next item.
"nnoremap <silent> <space>j  :<C-u>CocNext<CR>
"" Do default action for previous item.
"nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
"" Resume latest coc list
"nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
"
"" Notify coc.nvim that <enter> has been pressed.
"" Currently used for the formatOnType feature.
"inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
"      \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
"
"" Toggle panel with Tree Views
"nnoremap <silent> <space>t :<C-u>CocCommand metals.tvp<CR>
"" Toggle Tree View 'metalsBuild'
"nnoremap <silent> <space>tb :<C-u>CocCommand metals.tvp metalsBuild<CR>
"" Toggle Tree View 'metalsCompile'
"nnoremap <silent> <space>tc :<C-u>CocCommand metals.tvp metalsCompile<CR>
"" Reveal current current class (trait or object) in Tree View 'metalsBuild'
"nnoremap <silent> <space>tf :<C-u>CocCommand metals.revealInTreeView metalsBuild<CR>
