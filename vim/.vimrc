" Using folding
" vim:fdm=marker

"Startup {{{
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
  call dein#add('Shougo/dein.vim')

  " PLUGIN SETUP
  "------ Examples -------
  "call dein#add('Shougo/neosnippet.vim')
  "call dein#add('Shougo/neosnippet-snippets')

  " You can specify revision/branch/tag.
  "call dein#add('Shougo/vimshell', { 'rev': '3787e5' })


  " Add or remove your plugins here:

  " Looks
  call dein#add('morhetz/gruvbox')
  call dein#add('tomasr/molokai')
  
  " IDE
  call dein#add('scrooloose/nerdtree')      " filesystem tree sidebar
  call dein#add('lifepillar/vim-cheat40')   " cheatsheet buffer
  call dein#add('Shougo/deoplete.nvim')     " dark powered neo-completion
  call dein#add('airblade/vim-gitgutter')   " show git changes in gutter
 
  " Tags
  call dein#add('majutsushi/tagbar')        " lists all tags for a file on sidebar
  call dein#add('xolox/vim-easytags')       " generates ctags usign exuberant-ctags
  call dein#add('xolox/vim-misc')           " required by vim-easytags

  " JS
  call dein#add('ternjs/tern_for_vim')
  call dein#add('carlitux/deoplete-ternjs', {'build': 'npm install -g tern'})
  call dein#add('othree/jspc.vim')

 "Required:
  call dein#end()
  call dein#save_state()
endif

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"}}}

"Editor Settings {{{
filetype plugin indent on     " Load plugins according to detected filetype 
syntax enable                     " Enable syntax highlighting

set number                    " Put line numbers in gutter
set autoindent                " Indent according to previous line.
set expandtab                 " Use spaces instead of tabs.
set softtabstop =2            " Tab key indents by 4 spaces.
set shiftwidth  =2            " >> indents by 4 spaces.
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

set updatetime=250            " Quicker gitgutter updates

set list                      " Show non-printable characters.
if has('multi_byte') && &encoding ==# 'utf-8'
  let &listchars = 'tab:▸ ,extends:❯,precedes:❮,nbsp:±'
else
  let &listchars = 'tab:> ,extends:>,precedes:<,nbsp:.'
endif


 "}}}

" Look settings {{{

let $NVIM_TUI_ENABLE_TRUE_COLOR=1

"colorscheme molokai           " Set molokai colorscheme.
colorscheme gruvbox           " Set gruvbox colorscheme.
set background =dark          " Use gruvbox dark theme.

" }}}

"Other Settings {{{

let mapleader=" "                             " Set leader button

map <leader>s :source ~/.vimrc<CR>              
                                              " Source a file
                              

" Python plugins
let g:python_host_prog = '/usr/bin/python'    " Python 2
let g:python3_host_prog = '/usr/bin/python3'  " Python 3

"}}}

"Key bindings {{{

" NERDTree
map <C-n> :NERDTreeToggle<CR>
" Tagbar
nmap <F8> :TagbarToggle<CR>


"}}}

" Plugins Settings {{{ 

"nerdtree
let NERDTreeShowHidden=1 "show hidden files 
" close NERDTree when it's the last window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
"autocmd vimenter * NERDTree " open nerdtree when vim starts

"deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#omni#functions = {}
let g:deoplete#omni#functions.javascript = [
  \ 'tern#Complete',
  \ 'jspc#omni'
  \]
set completeopt=longest,menuone,preview
let g:deoplete#sources = {}
let g:deoplete#sources['javascript.jsx'] = ['file', 'ternjs']
let g:tern#command = ['tern']
let g:tern#arguments = ['--persistent']

"deoplete-ternjs - JS
let g:tern_request_timeout = 1
let g:tern_show_signature_in_pum = '0'  " This do disable full signature type on autocomplete

"Add extra filetypes
let g:tern#filetypes = [
                \ 'jsx',
                \ 'javascript.jsx',
                \ 'vue',
                \ ]
"}}}
