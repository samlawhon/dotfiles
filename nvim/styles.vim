" =============================================================================
"                            LANGUAGE CONFIGURATION
" =============================================================================

" Default
" -------
augroup pep8
  autocmd!
  autocmd BufWritePre * %s/\s\+$//e  " Delete trailing whitespace on save
  autocmd FileType * setlocal foldenable foldmethod=manual tabstop=4
    \ softtabstop=4 shiftwidth=4 textwidth=79 expandtab autoindent nohlsearch
augroup END

" Json
" ----
autocmd FileType json syntax match Comment +\/\/.\+$+

" Scala
" -----
augroup scala_lang
  autocmd!
  autocmd FileType scala setlocal
    \ tabstop=2 softtabstop=2 shiftwidth=2 textwidth=120
augroup END

" R
" -
augroup r_lang
  autocmd!
  autocmd FileType r,vim setlocal
    \ tabstop=2 softtabstop=2 shiftwidth=2 textwidth=79
    \ autoindent fileformat=unix nohlsearch
augroup END

" Vim / YAML
" ----------
augroup vim_lang
  autocmd!
  autocmd FileType vim setlocal foldenable foldmethod=manual
    \ tabstop=2 softtabstop=2 shiftwidth=2 expandtab
    \ autoindent fileformat=unix nohlsearch
augroup END

augroup yaml
  autocmd!
  autocmd BufRead BufNewFile *.yml setlocal filetype=yaml
  autocmd FileType yaml setlocal foldenable foldmethod=manual
    \ tabstop=2 softtabstop=2 shiftwidth=2 expandtab
    \ autoindent fileformat=unix nohlsearch
augroup END

