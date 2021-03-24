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

" Fast escape
imap jk                       <Esc>
omap jk                       <Esc>
imap kj                       <Esc>
omap kj                       <Esc>

" Getting around
nnoremap <left>               <C-w>h
nnoremap <right>              <C-w>l
nnoremap <up>                 <C-w>k
nnoremap <down>               <C-w>j
nnoremap <S-ScrollWheelUp>    <ScrollWheelLeft>
nnoremap <S-ScrollWheelDown>  <ScrollWheelRight>
map <S-Down>                  ]mzz
map <S-Up>                    [mzz

" Camel Motion
map <silent> w <Plug>CamelCaseMotion_w
map <silent> b <Plug>CamelCaseMotion_b
map <silent> e <Plug>CamelCaseMotion_e
map <silent> ge <Plug>CamelCaseMotion_ge
sunmap w
sunmap b
sunmap e
sunmap ge

" Editing
" TODO can we do this C-movement in visual mode for multiple lines?
nnoremap <C-Up>               ddkP
nnoremap <C-Down>             ddp
inoremap <C-J>                <Esc>viwUea
nnoremap <Leader>"            viw<Esc>a"<Esc>bi"<Esc>lel
noremap  <Leader>/            :Commentary<CR>
onoremap p                    i(
onoremap in(                  :<C-U>normal! f(vi(<CR>
onoremap in[                  :<C-U>normal! f[vi[<CR>
onoremap il(                  :<C-U>normal! F)vi(<CR>
onoremap il[                  :<C-U>normal! F]vi[<CR>
onoremap i;                   :<C-U>execute "normal! v/;\rhs"<CR>

" Markdown and RST headers
nnoremap <Leader>1            yypVr=
nnoremap <Leader>2            yypVr-
nnoremap <Leader>3            yypVr+
nnoremap <Leader>4            yypVr*
onoremap ih                   :<C-U>execute "normal! ?^==\\+$\r:nohlsearch\rkvg_"<CR>

" Code Folding
nnoremap \                    za
vnoremap \                    zf

" =======================================================================
"                       LEADER LAYERS
" =======================================================================
" [p]encil
" -----------------------------------------------------------------------
nnoremap <Leader>p            :<C-u>TogglePencil<CR>
nnoremap <Leader>ph           :<C-u>HardPencil<CR>
nnoremap <Leader>ps           :<C-u>SoftPencil<CR>
nnoremap <Leader>po           :<C-u>PencilOff<CR>

" [s]ettings - plugins, keymaps, and individual subsections
" -----------------------------------------------------------------------
nnoremap <Leader>si           :<C-u>tabe $MYVIMRC<CR>
nnoremap <Leader>sk           :<C-u>exe "tabe ".vimdir."/shortcuts.vim"<CR>
nnoremap <Leader>sc           :<C-u>exe "tabe ".vimdir."/coc.vim"<CR>
nnoremap <Leader>sl           :<C-u>source $MYVIMRC<CR>

" [g]it commands
" -----------------------------------------------------------------------
nnoremap <Leader>gk           :<C-u>Gstatus<CR>
nnoremap <Leader>g<S-k>       :<C-u>Git push<CR>
nnoremap <Leader>gf           :<C-u>Git fetch<CR>
nnoremap <Leader>gF           :<C-u>Git pull<CR>
nnoremap <Leader>gd           :<C-u>Gdiffsplit<CR>

" Tab Movement
" -----------------------------------------------------------------------
nnoremap <Leader>h            :<C-u>tabp<CR>
nnoremap <Leader>l            :<C-u>tabn<CR>

" [r]un code
" -----------------------------------------------------------------------
nnoremap <Leader>rp           :silent !python <C-R>%<CR>

" [v]im execution
" -----------------------------------------------------------------------
nnoremap <Leader>ve           yy:@"<CR>

" View [m]essages, logs, and output
" -----------------------------------------------------------------------
nnoremap <Leader>m            :messages<CR>

" [f]ile tree commands
" -----------------------------------------------------------------------
nnoremap <silent> <Leader>ft  :<C-u>NERDTree<CR>
" Open/close NERDTree at the current buffer
nnoremap <silent> <expr> <Leader>fn g:NERDTree.IsOpen() ? "\:NERDTreeClose<CR>" : bufexists(expand('%')) ? "\:NERDTreeFind<CR>" : "\:NERDTree<CR>"
" Toggle panel with Tree Views
nnoremap <silent> <Leader>fm  :<C-u>CocCommand metals.tvp<CR>
" Toggle Tree View 'metalsPackages'
nnoremap <silent> <Leader>fp  :<C-u>CocCommand metals.tvp metalsPackages<CR>
" Toggle Tree View 'metalsCompile'
nnoremap <silent> <Leader>fc  :<C-u>CocCommand metals.tvp metalsCompile<CR>
" Toggle Tree View 'metalsBuild'
nnoremap <silent> <Leader>fb  :<C-u>CocCommand metals.tvp metalsBuild<CR>
" Reveal current current class (trait or object) in Tree View 'metalsPackages'
nnoremap <silent> <Leader>ff  :<C-u>CocCommand metals.revealInTreeView metalsPackages<CR>t

" [c]onqueror of Completion Commands
" -----------------------------------------------------------------------
" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Remap for rename current word
nmap <Leader>rn <Plug>(coc-rename)

" Used to expand decorations in worksheets
nmap <Leader>ws <Plug>(coc-metals-expand-decoration)

" Trigger for code actions
" Make sure `"codeLens.enable": rue` is set in coc config
nnoremap <Leader>cl :<C-u>call CocActionAsync('codeLensAction')<CR>

" [a]pplying codeAction to the selected region.
" Example: `<Leader>aap` for current paragraph
xmap <Leader>a  <Plug>(coc-codeaction-selected)
nmap <Leader>a  <Plug>(coc-codeaction-selected)
" Remap keys for applying codeAction to the current buffer.
nmap <Leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <Leader>qf  <Plug>(coc-fix-current)

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <Leader>ca  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <Leader>ce  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <Leader>cc  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <Leader>co  :<C-u>CocList outline<cr>
" Search workLeader symbols.
nnoremap <silent><nowait> <Leader>cs  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <Leader>cj  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <Leader>ck  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <Leader>cp  :<C-u>CocListResume<CR>

" Clip[b]oard commands
" -----------------------------------------------------------------------
nnoremap <Leader>bv                   "+p
nnoremap <Leader>bc                   "+y
