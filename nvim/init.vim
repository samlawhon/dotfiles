let g:vimdir=fnamemodify($MYVIMRC, ':p:h')

function SourceVimdirFile(filename)
  execute 'source' g:vimdir."/".a:filename
endfunction

call SourceVimdirFile("pluginstall.vim")

call plug#begin(vimdir."/plugged")
Plug 'cocopon/iceberg.vim'
Plug 'preservim/nerdtree' |
  \ Plug 'Xuyuanp/nerdtree-git-plugin' |
  \ Plug 'ryanoasis/vim-devicons'
Plug 'scrooloose/nerdtree-project-plugin'
Plug 'PhilRunninger/nerdtree-visual-selection'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'reedes/vim-pencil'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tmsvg/pear-tree'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'bkad/CamelCaseMotion'
Plug 'PProvost/vim-ps1'
Plug 'dhruvasagar/vim-table-mode'
" Completion engine
Plug 'neovim/nvim-lspconfig'
" Scala plugins
" Python plugins
" TODO hook to install after pyright
Plug 'nvie/vim-flake8'
Plug 'w0rp/ale'
" Typescript plugins
call plug#end()


call SourceVimdirFile("functionality.vim")
call SourceVimdirFile("theme.vim")
call SourceVimdirFile("styles.vim")
lua require'lspconfig'.bashls.setup{}
" lua require'lspconfig'.metals.setup{}
" lua require'lspconfig'.pyright.setup{}
" lua require'lspconfig'.julials.setup{}
" lua require'lspconfig'.html.setup{}
" lua require'lspconfig'.tsserver.setup{}
" lua require'lspconfig'.vimls.setup{}
" lua require'lspconfig'.dockerls.setup{}
call SourceVimdirFile("airline.vim")

" Key bindings defined in the "shortcuts" file should always take precedence
call SourceVimdirFile("shortcuts.vim")

if has("gui_running")
  call SourceVimdirFile("gui.vim")
endif

