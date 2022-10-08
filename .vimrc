call plug#begin()
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-commentary'
Plug '~/projects/aria.vim'
call plug#end()

filetype plugin indent on
set expandtab
syntax on
color hcol
set background=dark
set ts=4
set sw=4
set cinoptions=l1
set colorcolumn=81
set clipboard=unnamedplus
set hidden
set nu
set wildignore+=*.o,*/build/*,*/target/*
set nowrap
set scrolloff=10
set list
set listchars=extends:❯,precedes:❮,tab:\ \ ,
let mapleader=" "

highlight ColorColumn ctermbg=236 guibg=lightgrey
highlight MatchParen cterm=none ctermbg=darkblue ctermfg=none
highlight LineNr cterm=none ctermfg=79
highlight CursorLineNr cterm=bold ctermfg=79

autocmd BufRead,BufNewFile *.htm,*.html setlocal tabstop=2 shiftwidth=2 softtabstop=2

" set completeopt+=menuone
" set completeopt+=noselect
" set shortmess+=c
" let g:mucomplete#enable_auto_at_startup = 1

nnoremap : ;
nnoremap ; :
nnoremap Y y$
nnoremap gp `[v`]
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
vnoremap : ;
vnoremap ; :

nnoremap <Tab> <C-w>w
nnoremap <silent> <C-j> :<C-u>execute "keepjumps norm! " . v:count1 . "}"<CR>
nnoremap <silent> <C-k> :<C-u>execute "keepjumps norm! " . v:count1 . "{"<CR>
vnoremap <C-j> }
vnoremap <C-k> {
inoremap <C-c> <Esc>
nnoremap <C-c> <Esc>
nnoremap , :b#<Cr>
vnoremap , :b#<Cr>
nnoremap q @
nnoremap @ q

nnoremap <A-m> :make<BAR>copen<CR><CR><C-w><C-p>
nnoremap <A-n> :cn<CR>
nnoremap <A-p> :cp<CR>

nnoremap <silent> <A-s> :noh<CR>
tnoremap <Esc> <C-\><C-n>
nnoremap <Return> i<CR><Esc>
vnoremap p "0p

nnoremap <leader>/ /\<\><left><left>
