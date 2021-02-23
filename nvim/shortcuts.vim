" =============================================================================
" Keyboard Shortcuts
" =============================================================================

" Faster scrolling
nnoremap <C-E> 2<C-E>
nnoremap <C-Y> 2<C-Y>

" Don't remember where this came from
set backspace=indent,eol,start
set foldtext=getline(v:foldstart+1)

" Make spacebar the leader key
let mapleader=" "
nnoremap <Space> <Nop>

" Maybe the most controversial thing in here...
nnoremap <C-s>                :<C-u>w<CR>

" Getting around
nnoremap <Leader>t            :NERDTree<CR>
nnoremap <Leader>j            /# --------------------<CR>zt
nnoremap <Leader>k            ?# --------------------<CR>zt
nnoremap <A-l>                <C-w>l
nnoremap <A-h>                <C-w>h
nnoremap <A-k>                <C-w>k
nnoremap <A-j>                <C-w>j
inoremap jk                   <Esc>
inoremap kj                   <Esc>
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
nnoremap <Leader>p            :TogglePencil<CR>
onoremap p                    i(
onoremap in(                  :<C-U>normal! f(vi(<CR>
onoremap in[                  :<C-U>normal! f[vi[<CR>
onoremap il(                  :<C-U>normal! F)vi(<CR>
onoremap il[                  :<C-U>normal! F]vi[<CR>
onoremap i;                   :<C-U>execute "normal! v/;\rhs"<CR>

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
nnoremap <C-F4>               :q<CR>

" Git commands
nnoremap <Leader>gk           :Gstatus<CR><C-w><S-h> <bar> :exe "vertical res 30"<CR>
nnoremap <Leader>gp           :<C-u>Git push<CR>

" Fullscreen mode for gui
" nnoremap <F11>                :set lines=999<CR>:set columns=999<CR>
nnoremap <F11>                <Esc>:call libcallnr("gvimfullscreen.dll", "ToggleFullScreen", 0)<CR>

" Tab Movement
nnoremap <Leader>h            :<C-u>tabp<CR>
nnoremap <Leader>l            :<C-u>tabn<CR>
nnoremap <F1>                 1gt
nnoremap <F2>                 2gt
nnoremap <F3>                 3gt
nnoremap <F4>                 4gt
nnoremap <S-F1>               5gt
nnoremap <S-F2>               6gt
nnoremap <S-F3>               7gt
nnoremap <S-F4>               8gt

" Execute code
nnoremap <Leader>rp            :silent !python <C-R>%<CR>

" View logs and output
nnoremap <Leader>m             :messages<CR>

