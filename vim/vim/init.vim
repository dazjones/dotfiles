" Pathogen baby!
execute pathogen#infect()

" Map NerdTree to Ctrl-e
map <C-e> :NERDTreeToggle<CR>

" Get rid of those pesky swap files
set nobackup
set nowritebackup
set noswapfile

" Stops indentation on paste into terminal
set paste

 " Indent automatically depending on filetype
filetype indent on
set autoindent

" Turn on line numbering. Turn it off with set nonu
set number

" Set the tap spaces to 4 and expand to spaces
set tabstop=4 softtabstop=0 expandtab shiftwidth=4

" Set syntax on
syntax on

" Case insensitive search
set ic

" Higlhight search
set hls

" Wrap text instead of being on one line
set lbr

" OSX stupid backspace fix
set backspace=indent,eol,start

" Change colorscheme from default to delek
colorscheme nord

" Limit line length for git commit messages
au FileType gitcommit set tw=72


" Set sensible yml syntax
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType yml setlocal ts=2 sts=2 sw=2 expandtab

" Add ctrlp runtime
set runtimepath^=~/.vim/bundle/ctrlp/ctrlp.vim

set mouse=a
set nofoldenable

let g:concourse_tags_autosave = 0
let g:NERDTreeShowHidden = 1
let g:NERDTreeMinimalUI = 1
let g:NERDTreeIgnore = []
let g:NERDTreeStatusline = ''

" Automaticaly close nvim if NERDTree is only thing left open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" open new split panes to right and below
set splitright
set splitbelow
" turn terminal to normal mode with escape
tnoremap <Esc> <C-\><C-n>
" start terminal in insert mode
au BufEnter * if &buftype == 'terminal' | :startinsert | endif
" open terminal on ctrl+n
function! OpenTerminal()
  split term://zsh
  resize 10
endfunction
nnoremap <c-n> :call OpenTerminal()<CR>

set encoding=utf-8
let g:airline_powerline_fonts = 1

let g:auto_save = 1  " enable AutoSave on Vim startup
