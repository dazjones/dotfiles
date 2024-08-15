"set statusline+=col:\ %c, Pathogen baby!
"execute pathogen#infect()

let mapleader=','

" Get rid of those pesky swap files
set nobackup
set nowritebackup
set noswapfile

" Stops indentation on paste into terminal
set paste

 " Indent automatically depending on filetype
filetype indent on
set autoindent

" Show line and column in status bar
set ruler

" Turn on line numbering. Turn it off with set nonu
set number

" Replace tabs with spaces
set expandtab
" A tab shows as 2 columns wide
set tabstop=2
" How many spaces to indent text with (using << and >>)
set shiftwidth=2
" How many columns to insert when I press TAB
set softtabstop=2
" Highlight matching brackets
set showmatch

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
colorscheme catppuccin_frappe

" Limit line length for git commit messages
au FileType gitcommit set tw=72

" Set sensible yml syntax
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
autocmd FileType yml setlocal ts=2 sts=2 sw=2 expandtab

" open new split panes to right and below
set splitright
set splitbelow

" Up the undo / redo limit
set history=1000
" Show options during tab completion
set wildmenu
" Ignore crap in wildcard completion
set wildignore+=*.o,*.obj,.git,*.pyc,node_modules
" Make backspace sane (e.g. don't stop backspacing at the start of inserted text)
set backspace=indent,eol,start
" Set terminal title to current buffer
set title
let &titleold=getcwd()

" Write files with sudo if opened without priviliedges
cmap w!! w !sudo tee % >/dev/null

" Remove trailing whitespace
function! <SID>CleanWhitespace()
  " Preparation - save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " Do the business:
  %s/\s\+$//e
  " Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction
command CleanWhitespace call <SID>CleanWhitespace():

" Cut, copy and paste using the real clipboard
vnoremap <leader>y "+y
vnoremap <leader>x "+x
nnoremap <leader>p "+gp
nnoremap <leader>P "+gP

" yank to clipboard
if has("clipboard")
  set clipboard=unnamed " copy to the system clipboard

  if has("unnamedplus") " X11 support
    set clipboard+=unnamedplus
  endif
endif
