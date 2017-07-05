" Using folding
" vim:fdm=marker

"dein Scripts {{{
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/home/v3rse/.vim/bundle/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/home/v3rse/.vim/bundle')
  call dein#begin('/home/v3rse/.vim/bundle')

  " Let dein manage dein
  " Required:
  call dein#add('/home/v3rse/.vim/bundle/repos/github.com/Shougo/dein.vim')

  " PLUGIN SETUP
  " Add or remove your plugins here:
  "call dein#add('Shougo/neosnippet.vim')
  "call dein#add('Shougo/neosnippet-snippets')

  " You can specify revision/branch/tag.
  "call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

  " Required:
  call dein#end()
  call dein#save_state()
endif

" If you want to install not installed plugins on startup.
"if dein#check_install()
"  call dein#install()
"endif

"}}}

"Editor {{{


set nocompatible              " Make sure it runs iMproved, required

filetype plugin indent on     " Load plugins according to detected filetype 
syntax enable                     " Enable syntax highlighting

set number                    " Put line numbers in gutter
set autoindent                " Indent according to previous line.
" set expandtab                 " Use spaces instead of tabs.
" set softtabstop =4            " Tab key indents by 4 spaces.
" set shiftwidth  =4            " >> indents by 4 spaces.
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

"colorscheme gruvbox           " Set gruvbox colorscheme.
"set background =dark          " Use dark theme.


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

" Python plugins
" Python 2
let g:python_host_prog = '/usr/bin/python'

"Python 3
let g:python3_host_prog = '/usr/bin/python3'

"}}}

" Plugins Settings {{{

"1.Command-T
"set wildignore+=*.log,*.sql,*.cache

"2.NERDTree
"Make NERD Tree show hidden files
let NERDTreeShowHidden=1

"3.VDebug
"let g:vdebug_options = {}
"let g:vdebug_options["port"] = 9000

"let g:vdebug_options["path_maps"] = {
"}


"keybinding
map <C-n> :NERDTreeToggle<CR>

"close vim when NERDTree is last window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
"Start NERD Tree when vim starts up
""autocmd vimenter * NERDTree

" Tagbar
nmap <F8> :TagbarToggle<CR>

"}}}
