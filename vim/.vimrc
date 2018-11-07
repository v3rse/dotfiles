"Using folding
" vim:fdm=marker

"Startup {{{
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/Users/v3rse/.vim/bundle/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/Users/v3rse/.vim/bundle')
  call dein#begin('/Users/v3rse/.vim/bundle')

  " Let dein manage dein
  " Required:
  call dein#add('Shougo/dein.vim')

  " PLUGIN SETUP

  " Add or remove your plugins here:

  " Looks
  call dein#add('morhetz/gruvbox')
  call dein#add('tomasr/molokai')
  call dein#add('drewtempelmeyer/palenight.vim')
  call dein#add('junegunn/rainbow_parentheses.vim') " rainbow parentheses for clojure
  call dein#add('godlygeek/tabular')                " lines up text
  call dein#add('junegunn/goyo.vim')                " distraction free coding
  call dein#add('junegunn/limelight.vim')           " light up a line
  call dein#add('itchyny/lightline.vim')            " lightline as status bar
  
  " IDE
  call dein#add('wsdjeg/dein-ui.vim')
  call dein#add('scrooloose/nerdtree')     " filesystem tree sidebar
  call dein#add('lifepillar/vim-cheat40')  " cheatsheet buffer
  call dein#add('airblade/vim-gitgutter')  " show git changes in gutter
  call dein#add('metakirby5/codi.vim')     " a wonderful REPL scratch pad
  call dein#add('plasticboy/vim-markdown') " better markdown
  call dein#add('junegunn/fzf')            " install fzf. the actual finder
  call dein#add('junegunn/fzf.vim')        " install fzf.vim. this the plugin
  call dein#add('Shougo/deoplete.nvim') " dark powered neo-completion
  if !has('nvim')                       " if we aren't runnin neovim
    call dein#add('roxma/nvim-yarp')
    call dein#add('roxma/vim-hug-neovim-rpc')
  endif
  call dein#add('Shougo/neosnippet.vim')      " snippets
  call dein#add('Shougo/neosnippet-snippets') 

  " Language client
  call dein#add('autozimu/LanguageClient-neovim', {
    \ 'rev': 'next',
    \ 'build': 'bash install.sh',
    \ })
 
  " JS
  call dein#add('jelera/vim-javascript-syntax') " better javascript syntax
  call dein#add('moll/vim-node')                " makes jumping into modules easier

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

set number
set relativenumber                    " Put line numbers in gutter
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

" True colors
if (has("nvim"))
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
 endif

if (has("termguicolors"))
  set termguicolors
endif


"colorscheme molokai           " Set molokai colorscheme.
"colorscheme gruvbox           " Set gruvbox colorscheme.
colorscheme palenight           " Set palenight colorscheme.
set background =dark          " Use gruvbox dark theme.

" lightline
let g:lightline = {
      \ 'colorscheme': 'palenight',
      \ }

" palenight
let g:palenight_terminal_italics=1
" }}}





"Other Settings {{{

let mapleader=" "                             " Set leader button

                                              " Source a file
                              

" Python plugins
let g:python_host_prog = '/usr/bin/python'    " Python 2
let g:python3_host_prog = '/usr/local/bin/python3'  " Python 3

"}}}





"Key bindings {{{

" Leader
map <leader>s :source ~/.vimrc<CR>              
map <leader>f :FZF<CR>            

" NERDTree
map <C-n> :NERDTreeToggle<CR>
" Tagbar
nmap <F8> :TagbarToggle<CR>

" Neosnippets
" Plugin key-mappings.
" Select and expang snippet
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior. Select next field in snippet
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
"imap <expr><TAB>
" \ pumvisible() ? "\<C-n>" :
" \ neosnippet#expandable_or_jumpable() ?
" \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
if has('conceal')
  set conceallevel=2 concealcursor=niv
endif

"}}}





" Plugins Settings {{{ 

" ----- language client
" Required for operations modifying multiple buffers like rename.
set hidden

let g:LanguageClient_serverCommands = {
    \ 'javascript': ['javascript-typescript-stdio'],
    \ 'python': ['/Users/v3rse/Library/Python/3.6/bin/pyls'],
    \ 'python3': ['/Users/v3rse/Library/Python/3.6/bin/pyls']
    \ }
let g:LanguageClient_autoStart = 1
set completefunc=LanguageClient#complete
set formatexpr=LanguageClient_textDocument_rangeFormatting()
nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>




" ----- deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1

" disable autocomplete by default
let b:deoplete_disable_auto_complete=1 
let g:deoplete_disable_auto_complete=1

call deoplete#custom#source('_',
            \ 'disabled_syntaxes', ['Comment', 'String'])

" set sources
let g:deoplete#sources = {}
let g:deoplete#sources.javascript = ['LanguageClient']
let g:deoplete#sources.python = ['LanguageClient']
let g:deoplete#sources.python3 = ['LanguageClient']

" ----- nerdtree
let NERDTreeShowHidden=1 "show hidden files 
" close NERDTree when it's the last window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
"autocmd vimenter * NERDTree " open nerdtree when vim starts

" ----- limelight
" Color name (:help cterm-colors) or ANSI code
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240

" Color name (:help gui-colors) or RGB color
let g:limelight_conceal_guifg = 'DarkGray'
let g:limelight_conceal_guifg = '#777777'

" Default: 0.5
let g:limelight_default_coefficient = 0.7

" Number of preceding/following paragraphs to include (default: 0)
 let g:limelight_paragraph_span = 1

" Beginning/end of paragraph
"   When there's no empty line between the paragraphs
"   and each paragraph starts with indentation
let g:limelight_bop = '^\s'
let g:limelight_eop = '\ze\n^\s'

" Highlighting priority (default: 10)
"   Set it to -1 not to overrule hlsearch
let g:limelight_priority = -1

" ----- goyo
autocmd! User GoyoEnter Limelight
autocmd! User GoyoLeave Limelight!

"}}}
