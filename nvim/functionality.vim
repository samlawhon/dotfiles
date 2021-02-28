" This file is for snippets and functions found in the wild
" that looked useful.

" Automatically create folders when writing a new file
autocmd BufWritePre *
    \ if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h')) |
        \ call mkdir(expand('<afile>:h'), 'p') |
    \ endif

" Make NERDTree operate in a split-view mode instead of drawer mode
let NERDTreeHijackNetrw=1
