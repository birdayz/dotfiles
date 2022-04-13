call plug#begin('~/.vim/plugged')
Plug 'chentau/marks.nvim'
"Plug 'airblade/vim-rooter'
Plug 'jose-elias-alvarez/null-ls.nvim'
Plug 'jose-elias-alvarez/nvim-lsp-ts-utils'
Plug 'EdenEast/nightfox.nvim'
"Plug 'sbdchd/neoformat'
Plug 'projekt0n/github-nvim-theme'
Plug 'xiyaowong/nvim-transparent'
Plug 'rfratto/vim-go-testify'
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-telescope/telescope-file-browser.nvim'
Plug 'tami5/sqlite.lua'
Plug 'tami5/lspsaga.nvim', {'branch': 'main'}
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
Plug 'dstein64/nvim-scrollview'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update - also run TSInstall go
Plug 'numToStr/Comment.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'sheerun/vim-polyglot'
Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary' }
Plug 'nvim-lualine/lualine.nvim'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'p00f/nvim-ts-rainbow'
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
Plug 'dense-analysis/ale'
Plug 'bufbuild/vim-buf'

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
local nightfox = require('nightfox')

-- This function set the configuration of nightfox. If a value is not passed in the setup function
-- it will be taken from the default configuration above
nightfox.setup({
  options = {
    inverse = {
      match_paren = true, -- inverse the highlighting of match_parens
    },
    transparent = true, -- Disable setting the background color
    styles = {
      comments = "italic", -- change style of comments to be italic
      keywords = "bold", -- change style of keywords to be bold
      functions = "italic,bold" -- styles can be a comma separated list
    },
  },
  palettes = {
    red = "#FF000", -- Override the red color for MAX POWER
    bg_alt = "#000000",
  },
  groups = {
    TSPunctDelimiter = { fg = "${red}" }, -- Override a highlight group with the color red
    LspCodeLens = { bg = "#000000", style = "italic" },
  }
})

-- Load the configuration set above and apply the colorscheme
vim.cmd("colorscheme nightfox")
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

require'marks'.setup {
  -- whether to map keybinds or not. default true
  default_mappings = true,
  -- which builtin marks to show. default {}
  builtin_marks = { ".", "<", ">", "^" },
  -- whether movements cycle back to the beginning/end of buffer. default true
  cyclic = true,
  -- whether the shada file is updated after modifying uppercase marks. default false
  force_write_shada = false,
  -- how often (in ms) to redraw signs/recompute mark positions. 
  -- higher values will have better performance but may cause visual lag, 
  -- while lower values may cause performance penalties. default 150.
  refresh_interval = 250,
  -- sign priorities for each type of mark - builtin marks, uppercase marks, lowercase
  -- marks, and bookmarks.
  -- can be either a table with all/none of the keys, or a single number, in which case
  -- the priority applies to all marks.
  -- default 10.
  sign_priority = { lower=10, upper=15, builtin=8, bookmark=20 },
  -- disables mark tracking for specific filetypes. default {}
  excluded_filetypes = {},
  -- marks.nvim allows you to configure up to 10 bookmark groups, each with its own
  -- sign/virttext. Bookmarks can be used to group together positions and quickly move
  -- across multiple buffers. default sign is '!@#$%^&*()' (from 0 to 9), and
  -- default virt_text is "".
  bookmark_0 = {
    sign = "⚑",
    virt_text = "hello world"
  },
  mappings = {}
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
    settings = {
        gopls = {
            gofumpt = true,
            buildFlags =  {"-tags=agentintegrationtests"},
        }
    },
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
            format = {
                enable = "false"
            },
            schemas = {
                ["kubernetes"] = "deployment.yaml",
                ["https://raw.githubusercontent.com/docker/cli/master/cli/compose/schema/data/config_schema_v3.9.json"] = "docker-compose.yaml",
                ["https://json.schemastore.org/taskfile.json"] = "Taskfile.yml"

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
    theme = 'nightfox',
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
    },
    git_files = {
      git_command = { "git", "ls-files", "--exclude-standard", "--cached", "--deduplicate" },
    },
    oldfiles = {
        theme = "ivy"
    },
  },
  extensions = {
    file_browser = {
      hidden = true,
      respect_gitignore = true
    },
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    },
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
require("telescope").load_extension "file_browser"

--require('nvim_comment').setup()

EOF

let g:tokyonight_transparent = 1

colorscheme tokyonight


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
set nowrap
set tw=500
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

"colorscheme gruvbox
highlight Normal     ctermbg=NONE guibg=NONE
highlight LineNr     ctermbg=NONE guibg=NONE
highlight SignColumn ctermbg=NONE guibg=NONE

lua <<EOF
require("transparent").setup({
  enable = true, -- boolean: enable transparent
  extra_groups = { -- table/string: additional groups that should be clear
    -- In particular, when you set it to 'all', that means all avaliable groups

    -- example of akinsho/nvim-bufferline.lua
    "BufferLineTabClose",
    "BufferlineBufferSelected",
    "BufferLineFill",
    "BufferLineBackground",
    "BufferLineSeparator",
    "BufferLineIndicatorSelected",
  },
  exclude = {}, -- table: groups you don't want to clear
})
local saga = require 'lspsaga'
saga.init_lsp_saga()

EOF

set cursorline

nnoremap <silent> gh <cmd>lua require'lspsaga.provider'.lsp_finder()<CR>
nnoremap <silent> g? <cmd>lua vim.diagnostic.open_float()<CR>

if has('shada') " ignore /tmp and /mnt in shada history
  set shada=!,'1000,<50,s100,h,r/tmp,r/mnt
endif


"colorscheme github_*
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
:set foldlevelstart=99
let g:transparent_enabled = v:true
" colorscheme nightfox
" Git commit messages
:autocmd FileType gitcommit set textwidth=72
:autocmd FileType gitcommit set colorcolumn=51,73

lua << EOF
local lspconfig = require("lspconfig")
local null_ls = require("null-ls")
local buf_map = function(bufnr, mode, lhs, rhs, opts)
    vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts or {
        silent = true,
    })
end
local on_attach = function(client, bufnr)
    vim.cmd("command! LspDef lua vim.lsp.buf.definition()")
    vim.cmd("command! LspFormatting lua vim.lsp.buf.formatting()")
    vim.cmd("command! LspCodeAction lua vim.lsp.buf.code_action()")
    vim.cmd("command! LspHover lua vim.lsp.buf.hover()")
    vim.cmd("command! LspRename lua vim.lsp.buf.rename()")
    vim.cmd("command! LspRefs lua vim.lsp.buf.references()")
    vim.cmd("command! LspTypeDef lua vim.lsp.buf.type_definition()")
    vim.cmd("command! LspImplementation lua vim.lsp.buf.implementation()")
    vim.cmd("command! LspDiagPrev lua vim.diagnostic.goto_prev()")
    vim.cmd("command! LspDiagNext lua vim.diagnostic.goto_next()")
    vim.cmd("command! LspDiagLine lua vim.diagnostic.open_float()")
    vim.cmd("command! LspSignatureHelp lua vim.lsp.buf.signature_help()")
    buf_map(bufnr, "n", "gd", ":LspDef<CR>")
    buf_map(bufnr, "n", "gr", ":LspRename<CR>")
    buf_map(bufnr, "n", "gy", ":LspTypeDef<CR>")
    buf_map(bufnr, "n", "K", ":LspHover<CR>")
    buf_map(bufnr, "n", "[a", ":LspDiagPrev<CR>")
    buf_map(bufnr, "n", "]a", ":LspDiagNext<CR>")
    buf_map(bufnr, "n", "ga", ":LspCodeAction<CR>")
    buf_map(bufnr, "n", "<Leader>a", ":LspDiagLine<CR>")
    buf_map(bufnr, "i", "<C-x><C-x>", "<cmd> LspSignatureHelp<CR>")
    if client.resolved_capabilities.document_formatting then
        vim.cmd("autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()")
    end
end
lspconfig.tsserver.setup({
    on_attach = function(client, bufnr)
        client.resolved_capabilities.document_formatting = false
        client.resolved_capabilities.document_range_formatting = false
        local ts_utils = require("nvim-lsp-ts-utils")
        ts_utils.setup({})
        ts_utils.setup_client(client)
        buf_map(bufnr, "n", "gs", ":TSLspOrganize<CR>")
        buf_map(bufnr, "n", "gi", ":TSLspRenameFile<CR>")
        buf_map(bufnr, "n", "go", ":TSLspImportAll<CR>")
        on_attach(client, bufnr)
    end,
})
null_ls.setup({
    sources = {
        null_ls.builtins.diagnostics.eslint,
        null_ls.builtins.code_actions.eslint,
    },
    on_attach = on_attach,
})
vim.o.updatetime = 250
vim.cmd [[autocmd! CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]]
EOF
let g:ale_linters = {
\   'proto': ['buf-lint',],
\}
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters_explicit = 1


"au FileType * lua << EOF | local parser = vim.treesitter.get_parser(0) | if parser ~= nil then parser:register_cbs({ on_bytes = function() parser:parse() end }) end | EOF
