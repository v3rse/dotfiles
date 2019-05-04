
" vim:fdm=marker

"Startup {{{
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim
set rtp+=~/.fzf

" Required:
if dein#load_state('~/.cache/dein')
  call dein#begin('~/.cache/dein')

  " Let dein manage dein
  " Required:
  call dein#add('~/.cache/dein/repos/github.com/Shougo/dein.vim')

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
  call dein#add('junegunn/fzf.vim')
  if !has('nvim')                          " if we aren't runnin neovim
    call dein#add('roxma/nvim-yarp')
    call dein#add('roxma/vim-hug-neovim-rpc')
  endif
  call dein#add('scrooloose/nerdcommenter') " comment stuff out

  " language server
  call dein#add('neoclide/coc.nvim', {'build': 'yarn install'})

  " JS
  call dein#add('jelera/vim-javascript-syntax') " better javascript syntax
  call dein#add('moll/vim-node')                " makes jumping into modules easier
  call dein#add('leafgarland/typescript-vim')               " typescript highlighting

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
set cmdheight   =2            " better display for messages
set shortmess   +=c           " don't give |inc-completion-menu| messages
set signcolumn  =yes          " always show signcolumn


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
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'cocstatus', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'cocstatus': 'coc#status'
      \ },
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
map <leader>R :source ~/.vimrc<CR>              
map <leader>F :FZF<CR>            

" NERDTree
map <C-n> :NERDTreeToggle<CR>

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> for trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> for confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[c` and `]c` for navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K for show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if &filetype == 'vim'
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
vmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
vmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Use `:Format` for format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` for fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)


" COC
" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
"}}}



" Plugins Settings {{{ 

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
"autocmd! User GoyoEnter Limelight
"autocmd! User GoyoLeave Limelight!

"}}}
