source ~/repos/dotfiles/nvim/pluginstall.vim

call plug#begin('~/.config/nvim/plugged')
Plug 'cocopon/iceberg.vim'
Plug 'scrooloose/nerdtree'
Plug 'nvie/vim-flake8'
Plug 'w0rp/ale'
Plug 'reedes/vim-pencil'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-commentary'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'bkad/CamelCaseMotion'
Plug 'PProvost/vim-ps1'
Plug 'dhruvasagar/vim-table-mode'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tpope/vim-surround'
call plug#end()

let vimdir=fnamemodify($MYVIMRC, ':p:h')

execute 'source' vimdir."/theme.vim"
execute 'source' vimdir."/styles.vim"
execute 'source' vimdir."/shortcuts.vim"
execute 'source' vimdir."/coc.vim"
execute 'source' vimdir."/airline.vim"

if has("gui_running")
  source ~/repos/dotfiles/nvim/gui.vim
endif

