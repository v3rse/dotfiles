" vim:fdm=marker

"Editor Startup {{{

set nocompatible               " be iMproved, remove vi compatibility
set showmatch 		       " show matching brackets or braces
set ignorecase		       " case-insensitive search

set incsearch              " find while pattern is being typed
set hlsearch               " keep match higlights

set tabstop=2              " number of spaces/columns per tab
set softtabstop=2          " see multiple spaces as tabstops in case of editting (in the case of backspace)
set expandtab              " indent with whitespaces
set shiftwidth=2           " number of spaces per indent
set autoindent             " indent based on previous line
set shiftround             " >> indents by next multiple of 'shiftwidth'
set smartindent            " indent when starting a new line

set number                 " add line numbers
set relativenumber         " use relative numbering
set colorcolumn=80         " color column to show when to stop coding long lines like this one
highlight ColorColumn ctermbg=darkgrey guibg=darkgrey  " color of color column

filetype plugin indent on  " allow plugins and auto-indenting based on file type
syntax on                  " syntax highlighting

set mouse=a                " mouse click for all modes
set backspace=indent,eol,start " backspace working as expected
set clipboard=unnamedplus  " using system clipboard
set hidden                    " Switch between buffers without having to save first
set laststatus  =2            " Always show statusline.
set display     =lastline     " Show as much as possible of the last line.
set cmdheight   =2            " better display for messages
set shortmess   +=c           " don't give |inc-completion-menu| messages
set signcolumn  =yes          " always show signcolumn

set showmode                  " Show current mode in command-line.
set showcmd                   " Show already typed keys when more are expected.

set splitbelow                " Open new windows below the current window.
set splitright                " Open new windows right of the current window.

set wrapscan                  " Searches wrap around end-of-file.
set report     =0             " Always report changed lines.
set synmaxcol  =200           " Only highlight the first 200 columns.

set updatetime=250            " Quicker gitgutter updates

set lazyredraw                " Only redraw when necessary.

set completeopt=menu,menuone,noselect " Configure completion dialogues



"}}}

"Plugin Startup {{{

" installation: https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/plugged')
  Plug 'folke/tokyonight.nvim', { 'branch': 'main' }             " color scheme

  Plug 'neovim/nvim-lspconfig'                                   " lsp configurations
  Plug 'williamboman/nvim-lsp-installer'                         " lsp installer
  Plug 'stevearc/aerial.nvim'                                    " symbol outline

  Plug 'folke/which-key.nvim'                                    " key hints

  Plug 'nvim-lua/plenary.nvim'                                   " telecope dependency
  Plug 'nvim-telescope/telescope.nvim'                           " fuzzy finder

  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}    " language parser

  Plug 'lewis6991/gitsigns.nvim'                                 " git gitter replacement

  Plug 'akinsho/toggleterm.nvim'                                 " intergrated terminal

  Plug 'kyazdani42/nvim-web-devicons'                            " for file icons
  Plug 'kyazdani42/nvim-tree.lua'                                " file explorer

  Plug 'hrsh7th/cmp-nvim-lsp'                                    " comp source for lsp client
  Plug 'hrsh7th/cmp-buffer'                                      " comp source for buffer
  Plug 'hrsh7th/cmp-path'                                        " comp source for path
  Plug 'hrsh7th/cmp-cmdline'                                     " comp source for vim cmd
  Plug 'saadparwaiz1/cmp_luasnip'                                " comp source for snippet eng
  Plug 'hrsh7th/nvim-cmp'                                        " completion engine
  Plug 'onsails/lspkind.nvim'                                    " completion pictograms
  Plug 'ray-x/lsp_signature.nvim'                                " function signatures

  Plug 'L3MON4D3/LuaSnip'                                        " snippets engine
  Plug 'rafamadriz/friendly-snippets'                            " commonly used snippets

  Plug 'terrortylor/nvim-comment'                                " commments
  Plug 'windwp/nvim-autopairs'                                   " pair brackets etc

  Plug 'lukas-reineke/indent-blankline.nvim'                     " styling for indentation

  Plug 'norcalli/nvim-colorizer.lua'                             " colorizer for css hex

  Plug 'nvim-lualine/lualine.nvim'                            " status line

  Plug 'karolbelina/uxntal.vim'                                  " uxn highlighting

  Plug '~/src/personal/bndr.vim'

  Plug 'elkowar/yuck.vim'
call plug#end()

"}}}

"Looks Startup {{{

" make colors work in themed terminals (most come before colorschem)
if (has('nvim'))
  let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
endif

if (has('termguicolors'))
  set termguicolors
endif

" day, storm, night
let g:tokyonight_style = "night"
let g:tokyonight_italic_functions = 1
" Change the "hint" color to the "orange" color, and make the "error" color bright red
let g:tokyonight_colors = {
  \ 'hint': 'orange',
  \ 'error': '#ff0000'
\ }
colorscheme tokyonight


"}}}

"LSP Plugin {{{

lua << EOF

-- completions
-- The nvim-cmp almost supports LSP's capabilities so You should advertise it to LSP servers..
local capabilities = require('cmp_nvim_lsp').default_capabilities()

local nvim_lsp = require('lspconfig')

-- when server attaches to buffer map keys for language server
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)
  buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)

  -- Attach function signatures for each buffer
  require("lsp_signature").on_attach({
    bind = true,
    handler_opts = {
      border = "rounded"
    }
  }, bufnr)

  -- Attach symbol outline source
  -- require("aerial").on_attach(client, bufnr)

end

local lsp_installer = require("nvim-lsp-installer")

-- setup lsp installer
lsp_installer.settings {
}

-- get list of installed servers
local servers = lsp_installer.get_installed_servers()

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
for _, server in ipairs(servers) do
  local opts = {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    },
    capabilities = capabilities
  }

  server:setup(opts)
end
EOF

"}}}

"Symbol Outline {{{
lua << EOF
require('aerial').setup{
  on_attach = function(bufnr)
   -- Toggle the aerial window with <leader>a
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>a', '<cmd>AerialToggle!<CR>', {})
    -- Jump forwards/backwards with '{' and '}'
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '{', '<cmd>AerialPrev<CR>', {})
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '}', '<cmd>AerialNext<CR>', {})
    -- Jump up the tree with '[[' or ']]'
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '[[', '<cmd>AerialPrevUp<CR>', {})
    vim.api.nvim_buf_set_keymap(bufnr, 'n', ']]', '<cmd>AerialNextUp<CR>', {})
  end
}
EOF
"}}}

"{{{ Completion
lua <<EOF
local cmp = require("cmp")
local lspkind = require("lspkind")
local cmp_autopairs = require("nvim-autopairs.completion.cmp")
local luasnip = require("luasnip")
local cmp_confirm_opts = {
  behavior = cmp.ConfirmBehavior.Replace,
  select = false,
}
local check_backspace = function()
  local col = vim.fn.col "." - 1
  return col == 0 or vim.fn.getline("."):sub(col, col):match "%s"
end

cmp.setup({
  snippet = {
    -- REQUIRED: specify snippet engine
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = {
    ["<C-k>"] = cmp.mapping.select_prev_item(),
    ["<C-j>"] = cmp.mapping.select_next_item(),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expandable() then
        luasnip.expand()
      elseif luasnip.jumpable() then
        luasnip.jump(1)
      elseif check_backspace() then
        fallback()
      elseif is_emmet_active() then
        return vim.fn["cmp#complete"]()
      else
        fallback()
      end
    end, { "i", "s", }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s", }),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.abort(),
    ["<CR>"] = cmp.mapping(function(fallback)
      if cmp.visible() and cmp.confirm(cmp_confirm_opts) then
        if luasnip.jumpable() then
          luasnip.jump(1)
        end
        return
      end

      if luasnip.jumpable() then
        if not luasnip.jump(1) then
          fallback()
        end
      else
        fallback()
      end
    end),
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'luasnip' }
  }, {
    { name = 'buffer' },
  }),
  formatting = {
    --  add in symbools/pictograms in completion
    format = lspkind.cmp_format({
      mode = 'symbol_text',
      maxwidth = 50,

      before = function (entry, vim_item)
        return vim_item
      end
    })
  }
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources({
    -- { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it. 
  }, {
    { name = 'buffer' },
  })
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

-- Auto pairs integration for completions
cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done({ map_char = { tex = '' } }))

EOF
"}}}

"{{{ Comments 
lua <<EOF
require('nvim_comment').setup()
EOF
"}}}

"{{{ Autopairs
lua <<EOF
require('nvim-autopairs').setup{}
EOF
"}}}

"File Explorer {{{
lua << EOF
-- deactivate netrw
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

require("nvim-tree").setup{}
EOF
"}}}

"Status Line {{{
lua << EOF
require('lualine').setup{
  options = {
    section_separators = { left = '', right = '' }, 
    component_separators = { left = '', right = ''}
  },
  extensions = {'nvim-tree', 'toggleterm', 'aerial'}
}
EOF
"}}}

"Key Hints Plugin {{{
lua << EOF
require("which-key").setup{
}
EOF
"}}}

"Fuzzy finder {{{

lua << EOF
local telescope = require('telescope')
telescope.load_extension('aerial')
telescope.setup({
  extensions = {
    aerial = {
      show_nesting = true
    }
  },
  defaults = {
    file_ignore_patterns = {"node_modules"}
  }
})
EOF

"}}}

"{{{Language Parser

lua <<EOF
require("nvim-treesitter.configs").setup({
  highlight = {
    enable = true
  },
  indent = {
    enable = true
  }
})
EOF

"}}}

"{{{Git

lua <<EOF
require("gitsigns").setup{}
EOF

"}}}

"{{{ Integrated Terminal

lua <<EOF
require("toggleterm").setup({
  open_mapping = [[<c-\>]], -- key binding
  direction = 'horizontal'
})
EOF

"}}}

"{{{ Snippets
lua <<EOF
-- vscode like snippet via friendly-snippets
require("luasnip.loaders.from_vscode").load()

-- custom snippets
-- require("luasnip.loaders.from_vscode").load({ paths = { "./my-cool-snippets" } })
EOF
"}}}

"Indentation {{{
lua << EOF
vim.opt.list = true
vim.opt.listchars:append("space:⋅")
vim.opt.listchars:append("eol:↴")

require("indent_blankline").setup {
    space_char_blankline = " ",
    show_current_context = true,
    show_current_context_start = true,
}
EOF
"}}}

"Colorizer {{{
lua << EOF
require("colorizer").setup({
 'css';
 'javascript';
  html = {
    mode = 'foreground';
  }
})
EOF
"}}}

"Keybindings {{{

" User defined
let mapleader=" "                             " Set leader button
map <leader>r :source ~/.config/nvim/init.vim<CR>
map <leader>n :bnext<CR>
map <leader>p :bprevious<CR>

" Telescope
nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>
nnoremap <leader>fa :Telescope aerial<cr>
nnoremap <leader>gc <cmd>lua require('telescope.builtin').git_commits()<cr>
nnoremap <leader>gd <cmd>lua require('telescope.builtin').git_bcommits()<cr>
nnoremap <leader>gb <cmd>lua require('telescope.builtin').git_branches()<cr>
nnoremap <leader>gs <cmd>lua require('telescope.builtin').git_status()<cr>
nnoremap <leader>gx <cmd>lua require('telescope.builtin').git_stash()<cr>

" Nvim Tree
nnoremap <C-n> :NvimTreeToggle<CR>
nnoremap <leader>tr :NvimTreeRefresh<CR>
nnoremap <leader>tn :NvimTreeFindFile<CR>

"}}}

