" Using folding
" vim:fdm=marker

"Editor {{{


set nocompatible              " Make sure it runs iMproved, required

filetype plugin indent on     " Load plugins according to detected filetype 
syntax on                     " Enable syntax highlighting

" Pathogen Initalisation
execute pathogen#infect()     

set number                    " Put line numbers in gutter
set autoindent                " Indent according to previous line.
set expandtab                 " Use spaces instead of tabs.
set softtabstop =4            " Tab key indents by 4 spaces.
set shiftwidth  =4            " >> indents by 4 spaces.
set shiftround                " >> indents by next multiple of 'shiftwidth'


set backspace   =indent,eol,start " Make backspace work as you would expect
set hidden                    " Switch between buffers without having to save first
set laststatus  =2            " Always show statusline.
set display     =lastline     " Show as much as possible of the last line.

set showmode                  " Show current mode in command-line.
set showcmd                   " Show already typed keys when more are expected.

set incsearch                 " Highlight while searching with / or ?.
set hlsearch                  " Keep matches highlighted.

set ttyfast                   " Fast redrawing.
set lazyredraw                " Only redraw when necessary.

set splitbelow                " Open new windows below the current window.
set splitright                " Open new windows right of the current window.

set cursorline                " Find the current line quickly.
set wrapscan                  " Searches wrap around end-of-file.
set report     =0             " Always report changed lines.
set synmaxcol  =200           " Only highlight the first 200 columns.

colorscheme gruvbox           " Set gruvbox colorscheme.
set background =dark          " Use dark theme.


set list                      " Show non-printable characters.
if has('multi_byte') && &encoding ==# 'utf-8'
  let &listchars = 'tab:▸ ,extends:❯,precedes:❮,nbsp:±'
else
  let &listchars = 'tab:> ,extends:>,precedes:<,nbsp:.'
endif


"}}}

"Other Settings {{{

let mapleader   =" "           " Set leader button

map <leader>s :source ~/.vimrc<CR> 
                              " Source a file

" Put all temporary files (.swp) under the same directory
set backup
set backupdir   =$HOME/.vim/files/backup/
set backupext   =-vimbackup
set backupskip  =
set directory   =$HOME/.vim/files/swap//
set updatecount =100
set undofile
set undodir     =$HOME/.vim/files/undo/
set viminfo     ='100,n$HOME/.vim/files/info/viminfo

" Python plugins
" Python 2
let g:python_host_prog = '/usr/bin/python'

"Python 3
let g:python3_host_prog = '/usr/bin/python3'

"}}}

" Plugins {{{

"1.Command-T
set wildignore+=*.log,*.sql,*.cache

"2.NERDTree
"keybinding
map <C-n> :NERDTreeToggle<CR>
"close vim when NERDTree is last window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
"Start NERD Tree when vim starts up
""autocmd vimenter * NERDTree

"}}}