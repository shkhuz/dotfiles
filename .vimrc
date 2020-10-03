call plug#begin('~/.vim/plugged')
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-commentary'
Plug 'sheerun/vim-polyglot'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'morhetz/gruvbox'
Plug '~/vim-plugins/aria'
Plug '~/vim-plugins/hcol'
call plug#end()

"" General Settings
set backspace=indent,eol,start
color hcol
syntax on
let mapleader=" "
if has("gui_running")
	set guifont="DejaVu Sans Mono":h11:b
endif
set clipboard=unnamedplus
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab
"" add files to ~/.trash
set backupdir=~/.trash//
set directory=~/.trash//
set undodir=~/.trash//
set autoindent
set hidden
set autoread
set nocompatible
set smartindent
set showmatch
set comments=sl:/*,mb:\ *,elx:\ */
set termencoding=utf-8
set enc=utf-8
set fenc=utf-8
set virtualedit+=block
set incsearch
set ignorecase
set smartcase
set hlsearch
set wildmenu
set nrformats+=alpha
set wrap
set number
set cursorline
set colorcolumn=80
set textwidth=80
hi ColorColumn ctermbg=234
let loaded_matchparen = 1

" Key Mappings
nnoremap <leader>[ `[V`]<
nnoremap <leader>] `[V`]>
nnoremap j gj
nnoremap k gk
nnoremap <leader>n :cn<CR>
nnoremap <leader>p :cp<CR>
nnoremap ; :
nnoremap : ;
nnoremap Y y$

nmap <Up> <Nop>
nmap <Down> <Nop>
nmap <Left> <Nop>
nmap <Right> <Nop>

map { <Nop>
map } <Nop>

noremap { <C-u>
noremap } <C-d>
noremap <C-j> }
noremap <C-k> {
noremap <C-h> B
noremap <C-l> W
noremap <C-n> <C-d>
noremap <C-s> <C-w>

noremap <silent> <leader>a :b#<CR>
noremap <silent> <leader>s :noh<CR>

nnoremap c* /\<<C-R>=expand('<cword>')<CR>\>\C<CR>``cgn
nnoremap c# ?\<<C-R>=expand('<cword>')<CR>\>\C<CR>``cgN

" Emacs kill-yank functionality
" (most important key bindings)
nnoremap <C-d> mm
nnoremap <C-w> d`m
nnoremap <C-q> y`m
nnoremap <C-f> P

" autocmd(s)
autocmd BufWritePre * %s/\s\+$//e
autocmd FileType html setlocal shiftwidth=2 softtabstop=2 expandtab noautoindent nosmartindent
autocmd FileType css setlocal shiftwidth=2 softtabstop=2 expandtab

if has("gui_running")
autocmd GUIEnter * set vb t_vb=
set guioptions -=m
set guioptions -=T
set guioptions -=r
set guioptions -=L
set guioptions -=b
endif

" ctrlp Settings
set wildignore+=*/target/*,*/build/*,*/bin/*,*/obj/*,*.o,*.so,*.a
let g:ctrlp_by_filename=0
let g:ctrlp_working_path_mode=0
