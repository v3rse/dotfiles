" Using folding
" vim:fdm=marker

"Editor {{{


set nocompatible              " be iMproved, required
"set colors for terminal
set t_Co=256
execute pathogen#infect()

"detects filetype and indents based on it
if has("autocmd")
	filetype on
	filetype indent on
	filetype plugin on
endif

"syntax highlighting
syntax on

"I love line numbers for debugging
set number
"Don't really like this but relative line numbers
"set relativenumber

"switch this stuff on for GUI.Will move to .gvimrc if becomes to large
if has('gui running')
	set guioptions-=m  "remove menu bar
	set guioptions-=T  "remove toolbar
endif

"ColorScheme
colorscheme desert 



"}}}

"Other Settings {{{



"set nocompatible          " get rid of Vi compatibility mode. SET FIRST!
"filetype plugin indent on " filetype detection[ON] plugin[ON] indent[ON]
"set t_Co=256              " enable 256-color mode.
"syntax enable             " enable syntax highlighting (previously syntax
"on).
"colorscheme desert        " set colorscheme
"set number                " show line numbers
"set laststatus=2          " last window always has a statusline
"filetype indent on        " activates indenting for files
"set nohlsearch            " Don't continue to highlight searched phrases.
"set incsearch             " But do highlight as you type your search.
"set ignorecase            " Make searches case-insensitive.
"set ruler                 " Always show info along bottom.
"set autoindent            " auto-indent
"set tabstop=4             " tab spacing
"set softtabstop=4         " unify
"set shiftwidth=4          " indent/outdent by 4 columns
"set shiftround            " always indent/outdent to the nearest tabstop
"set expandtab             " use spaces instead of tabs
"set smarttab              " use tabs at the start of a line, spaces elsewhere
"set nowrap 	           " don't wrap text			



"}}}




"TODO ctags

" Plugins {{{

"1.NERDTree
"keybinding
map <C-n> :NERDTreeToggle<CR>
"close vim when NERDTree is last window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
"Start NERD Tree when vim starts up
""autocmd vimenter * NERDTree

"2.surround
"keybinding
"cs"<q> -- change "s to <q>s
"ysW( -- surround word with ()
"vllllS' -- visual select and surround with ''
"ds' -- delete surounding ''

"3.Syntastic
"4.Istanbul
"}}}
