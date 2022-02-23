call plug#begin('~/.vim/plugged')
"Plug 'airblade/vim-rooter'
Plug 'rfratto/vim-go-testify'
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-telescope/telescope-file-browser.nvim'
Plug 'tami5/sqlite.lua'
Plug 'nvim-telescope/telescope-frecency.nvim'
Plug 'tami5/lspsaga.nvim', {'branch': 'nvim51'}
Plug 'moll/vim-bbye' " optional dependency
Plug 'aymericbeaumet/vim-symlink'
Plug 'gruvbox-community/gruvbox'
Plug 'ggandor/lightspeed.nvim'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'
Plug 'skywind3000/asyncrun.vim'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install'  }
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
Plug 'cljoly/telescope-repo.nvim'
Plug 'folke/tokyonight.nvim', { 'branch': 'main' }
Plug 'fatih/vim-go'
"Plug 'Xuyuanp/scrollbar.nvim'
Plug 'dstein64/nvim-scrollview'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update - also run TSInstall go
Plug 'numToStr/Comment.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'sheerun/vim-polyglot'
Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary' }
Plug 'nvim-lualine/lualine.nvim'
Plug 'kyazdani42/nvim-web-devicons'
"Plug 'p00f/nvim-ts-rainbow'
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'
Plug 'phaazon/hop.nvim'
Plug 'jreybert/vimagit'
Plug 'ray-x/lsp_signature.nvim'
Plug 'kyazdani42/nvim-web-devicons' " for file icons
Plug 'kyazdani42/nvim-tree.lua'
Plug 'ibhagwan/fzf-lua'
Plug 'vijaymarupudi/nvim-fzf'
Plug 'kyazdani42/nvim-web-devicons'
call plug#end()

lua require('Comment').setup()

let g:go_def_mode='gopls'
let g:go_info_mode='gopls'

set number
set completeopt=menu,menuone,noselect

" autocmd BufWritePre *.go lua vim.lsp.buf.formatting()
" autocmd BufWritePre *.go lua goimports(1000)

" let g:go_fmt_command = "goimports"

lua <<EOF
-- following options are the default
-- each of these are documented in `:help nvim-tree.OPTION_NAME`
require'nvim-tree'.setup {
  disable_netrw       = false,
  hijack_netrw        = false,
  open_on_setup       = false,
  ignore_ft_on_setup  = {},
  auto_close          = false,
  open_on_tab         = false,
  hijack_cursor       = false,
  update_cwd          = false,
  update_to_buf_dir   = {
    enable = true,
    auto_open = true,
  },
  diagnostics = {
    enable = false,
    icons = {
      hint = "",
      info = "",
      warning = "",
      error = "",
    }
  },
  update_focused_file = {
    enable      = false,
    update_cwd  = false,
    ignore_list = {}
  },
  system_open = {
    cmd  = nil,
    args = {}
  },
  filters = {
    dotfiles = false,
    custom = {}
  },
  view = {
    width = 30,
    height = 30,
    hide_root_folder = false,
    side = 'left',
    auto_resize = false,
    mappings = {
      custom_only = false,
      list = {}
    }
  }
}


  -- Setup nvim-cmp.
  local cmp = require'cmp'

  cmp.setup({
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
        -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
        -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
        -- require'snippy'.expand_snippet(args.body) -- For `snippy` users.
      end,
    },
    mapping = {
      ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
      ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
      ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
      ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
      ['<C-e>'] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ['<CR>'] = cmp.mapping.confirm({ select = true }),
    },
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'vsnip' }, -- For vsnip users.
      -- { name = 'luasnip' }, -- For luasnip users.
      -- { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
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

  -- Setup lspconfig.
  local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
  -- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
  local golang_setup = {
    on_attach = function(client, bufnr)
      require "lsp_signature".on_attach()  -- Note: add in lsp client on-attach
        local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

--Enable (broadcasting) snippet capability for completion
local capabilitiesHtml = vim.lsp.protocol.make_client_capabilities()
capabilitiesHtml.textDocument.completion.completionItem.snippetSupport = true

require'lspconfig'.html.setup {
  capabilities = capabilitiesHtml,
}

require'lspconfig'.html.setup{}

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
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
    end,
    capabilities = capabilities
  }

  require('lspconfig')['gopls'].setup(golang_setup)

  require'lspconfig'.yamlls.setup{
    settings = {
        yaml = {
            schemas = {
                ["kubernetes"] = "*.yaml"
            },
        },
    }
  }


  function goimports(timeoutms)
    local context = { source = { organizeImports = true } }
    vim.validate { context = { context, "t", true } }

    local params = vim.lsp.util.make_range_params()
    params.context = context

    -- See the implementation of the textDocument/codeAction callback
    -- (lua/vim/lsp/handler.lua) for how to do this properly.
    local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, timeout_ms)
    if not result or next(result) == nil then return end
    local actions = result[1].result
    if not actions then return end
    local action = actions[1]

    -- textDocument/codeAction can return either Command[] or CodeAction[]. If it
    -- is a CodeAction, it can have either an edit, a command or both. Edits
    -- should be executed first.
    if action.edit or type(action.command) == "table" then
      if action.edit then
        vim.lsp.util.apply_workspace_edit(action.edit)
      end
      if type(action.command) == "table" then
        vim.lsp.buf.execute_command(action.command)
      end
    else
      vim.lsp.buf.execute_command(action)
    end
  end

  vim.api.nvim_set_keymap('n', '<c-P>',
    "<cmd>lua require('fzf-lua').files()<CR>",
    { noremap = true, silent = true })

EOF

lua << END
require'lualine'.setup {
  options = {
    icons_enabled = true,
    theme = 'nightfly',
    component_separators = { left = '', right = ''},
    section_separators = { left = '', right = ''},
    disabled_filetypes = {},
    always_divide_middle = true,
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff',
                  {'diagnostics', sources={'nvim_diagnostic'}}},
  lualine_c = {
    {
      'filename',
      file_status = true, -- displays file status (readonly status, modified status)
      path = 2 -- 0 = just filename, 1 = relative path, 2 = absolute path
    }
    },
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = {}
  }

END
augroup ScrollbarInit
autocmd!
autocmd CursorMoved,VimResized,QuitPre * silent! lua require('scrollbar').show()
autocmd WinEnter,FocusGained           * silent! lua require('scrollbar').show()
autocmd WinLeave,BufLeave,BufWinLeave,FocusLost            * silent! lua require('scrollbar').clear()
augroup end

lua <<EOF
require("nvim-treesitter.configs").setup {
  rainbow = {
    enable = true,
    extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
    max_file_lines = nil, -- Do not enable for files with more than n lines, int
    -- colors = {}, -- table of hex strings
    -- termcolors = {} -- table of colour name strings
  },
  indent = {
    enable = false
  },
  highlight = {
    enable = true,
  },
}

local map = vim.api.nvim_set_keymap
local default_opts = {noremap = true}
map('n', '<leader>ff', "<cmd>lua require'telescope.builtin'.find_files({ find_command = {'rg', '--no-ignore-vcs', '--files', '--hidden', '-g', '!.git' }})<cr>", default_opts)
map('n', '<F9>', "<cmd>lua require'telescope'.extensions.file_browser.file_browser()<CR>", default_opts)

EOF

lua <<EOF
require'hop'.setup()

-- You dont need to set any of these options. These are the default ones. Only
-- the loading is important
local actions = require("telescope.actions")
require('telescope').setup {
  pickers = {
    find_files = {
      hidden = true
    }
  },
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    }
  },
  defaults = {
    path_display = function(opts, path)
              return string.gsub(path,os.getenv("HOME"),"~")
            end,
    mappings = {
      i = {
        ["<esc>"] = actions.close,
        ["<F1>"] = actions.close,
        ["<F2>"] = actions.close,
        ["<F3>"] = actions.close
      },
    },
  }
}
-- To get fzf loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require('telescope').load_extension('fzf')
require'telescope'.load_extension'repo'
require'telescope'.load_extension'frecency'
require("telescope").load_extension "file_browser"

--require('nvim_comment').setup()
EOF
"colorscheme tokyonight


nnoremap <F10> <cmd>vertical resize +5<cr>
nnoremap <F9> <cmd>vertical resize -5<cr>
nnoremap <F2> <cmd>Telescope oldfiles<cr>
nnoremap <F1> <cmd>lua require 'telescope'.extensions.file_browser.file_browser()<CR>
command! Filez execute (len(system('git rev-parse'))) ? ':Telescope find_files' : ':Telescope git_files'
map <F3> :Filez<CR>
map <F8> :lua require'telescope'.extensions.repo.cached_list{file_ignore_patterns={"/%.cache/", "/%.cargo/", "/%.vim/"}}<CR>
nnoremap <F4> <cmd>Telescope lsp_document_symbols<cr>
nnoremap <F5> <cmd>lua require('telescope.builtin').live_grep{ cwd = vim.fn.systemlist("git rev-parse --show-toplevel")[1] }<cr>
nnoremap <F6> <cmd>NvimTreeToggle<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

set autochdir

set clipboard+=unnamedplus
autocmd VimResized * wincmd =
autocmd FileType fugitive nmap <buffer> q gq
set scrollback=100000

:set number

:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
:  autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
:augroup END
":tnoremap <Esc> <C-\><C-n>
"
" let g:minimap_width = 10
" let g:minimap_auto_start = 1
" let g:minimap_auto_start_win_enter = 1
" let g:minimap_highlight_range = 1
" let g:minimap_highlight_search = 1
" let g:minimap_git_colors = 1

function! ToggleQuickFix()
    if empty(filter(getwininfo(), 'v:val.quickfix'))
        copen
    else
        cclose
    endif
endfunction

nnoremap <silent> <F7> :call ToggleQuickFix()<cr>
let g:asyncrun_open = 8
set nohlsearch
set hidden
set noerrorbells
"set nowrap
set noswapfile
set nobackup
set undodir=~/.vim/undodir
set undofile
set incsearch
set scrolloff=8
set colorcolumn=120
"highlight Normal guibg=none
"autocmd vimenter * hi Normal guibg=NONE ctermbg=NONE " transparent bg
let mapleader = " "

colorscheme gruvbox
highlight Normal     ctermbg=NONE guibg=NONE
highlight LineNr     ctermbg=NONE guibg=NONE
highlight SignColumn ctermbg=NONE guibg=NONE

lua <<EOF
local saga = require 'lspsaga'
saga.init_lsp_saga()

EOF

set cursorline

nnoremap <silent> gh <cmd>lua require'lspsaga.provider'.lsp_finder()<CR>
nnoremap <silent> g? <cmd>lua vim.diagnostic.open_float()<CR>

if has('shada') " ignore /tmp and /mnt in shada history
  set shada=!,'1000,<50,s100,h,r/tmp,r/mnt
endif
