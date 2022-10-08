highlight clear 
if exists("syntax_on") 
 syntax reset 
endif

let g:colors_name="hcol"
set background=dark

hi Normal ctermfg=white ctermbg=black
hi Function cterm=bold
hi Keyword ctermfg=63 cterm=bold
hi Type ctermfg=63 cterm=bold
hi Statement ctermfg=63 cterm=bold
hi String ctermfg=79
hi Comment ctermfg=248
hi PreProc ctermfg=LightBlue cterm=bold
hi Todo ctermbg=black ctermfg=magenta cterm=bold
hi Search ctermbg=237 ctermfg=white
hi NonText ctermfg=63 cterm=bold
