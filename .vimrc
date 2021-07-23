call plug#begin()
Plug 'ziglang/zig.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tpope/vim-commentary'
Plug 'psliwka/vim-smoothie'
Plug 'lifepillar/vim-mucomplete'
Plug '~/projects/aria.vim'
Plug 'rust-lang/rust.vim'
call plug#end()

filetype plugin indent on
set expandtab
syntax on
color hcol
set background=dark
set ts=4
set sw=4
set colorcolumn=81
set clipboard=unnamedplus
set hidden
set noswapfile
set nu
set rnu
set wildignore+=*.o,*/build/*,*/target/*
set mouse=a
let mapleader=" "

highlight ColorColumn ctermbg=236 guibg=lightgrey
highlight MatchParen cterm=none ctermbg=darkblue ctermfg=none
highlight LineNr cterm=none ctermfg=79
highlight CursorLineNr cterm=bold ctermfg=79

autocmd BufRead,BufNewFile *.htm,*.html setlocal tabstop=2 shiftwidth=2 softtabstop=2

set completeopt+=menuone
set completeopt+=noselect
set shortmess+=c
let g:mucomplete#enable_auto_at_startup = 1

nnoremap : ;
nnoremap ; :
nnoremap Y y$
nnoremap gp `[v`]
nnoremap <C-a> ^
nnoremap <A-x> <C-a>
nnoremap <C-e> $
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
vnoremap : ;
vnoremap ; :

nnoremap <C-j> }
nnoremap <C-k> {
nnoremap , :b#<Cr>
vnoremap <C-j> }
vnoremap <C-k> {
vnoremap , :b#<Cr>
nnoremap q @
nnoremap @ q

nnoremap <A-o> o<Esc>
nnoremap <A-O> O<Esc>
nnoremap <CR> i<CR><Esc>

nnoremap <A-m> :make<BAR>copen<CR><CR><C-w><C-p>
nnoremap <A-n> :cn<CR>
nnoremap <A-p> :cp<CR>

nnoremap <leader>s :noh<CR>
