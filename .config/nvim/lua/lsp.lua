-- Global mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', function() vim.diagnostic.jump({ count = -1, float = true }) end)
vim.keymap.set('n', ']d', function() vim.diagnostic.jump({ count = 1, float = true }) end)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', {}),
    callback = function(ev)
        -- Enable completion triggered by <c-x><c-o>
        vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

        -- Buffer local mappings.
        -- See `:help vim.lsp.*` for documentation on any of the below functions
        local opts = {buffer = ev.buf}
        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
        vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
        vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
        vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder,
                       opts)
        vim.keymap.set('n', '<space>wl', function()
            print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, opts)
        vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
        vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
        vim.keymap.set({'n', 'v'}, '<space>ca', vim.lsp.buf.code_action, opts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
        vim.keymap.set('n', '<space>f',
                       function() 
													vim.lsp.buf.format {async = false} 
												end, opts)
				vim.keymap.set('n', '<space>i',
                       function() vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true }) end, opts)

    end
})

--vim.cmd [[autocmd BufWritePre *.go lua vim.lsp.buf.format()]]

-- Set up nvim-cmp.
local cmp = require 'cmp'

cmp.setup({
  completion = {
    autocomplete = { require('cmp.types').cmp.TriggerEvent.TextChanged },
    completeopt = 'menu,menuone,noselect',
    keyword_length = 1,  -- Start suggesting after 1 character
  },
matching = {
    disallow_fuzzy_matching = true,
    disallow_partial_fuzzy_matching = true,
    disallow_partial_matching = true,
    disallow_prefix_unmatching = true,
  },
  sorting = {
    priority_weight = 2,
    comparators = {
      cmp.config.compare.exact,
      cmp.config.compare.score,
      cmp.config.compare.offset,
      cmp.config.compare.recently_used,
      cmp.config.compare.kind,
      cmp.config.compare.sort_text,
      cmp.config.compare.length,
      cmp.config.compare.order,
    },
  },
  performance = {
    debounce = 20,         -- ms to wait after keystroke before triggering completion
    throttle = 20,         -- ms to wait before triggering completion again
    fetching_timeout = 50 -- timeout for LSP responses
  },
    -- REQUIRED: without this, confirming any LSP snippet completion (gopls
    -- sends them, usePlaceholders=true) errors out or inserts raw ${1:...}.
    snippet = {
        expand = function(args)
            require('luasnip').lsp_expand(args.body)
        end
    },
    window = {
         completion = cmp.config.window.bordered(),
         documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({select = true}) -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
    		{ name = 'nvim_lsp_signature_help' },
        {name = 'nvim_lsp'},
				--{name = 'vsnip'} -- For vsnip users.
        { name = 'luasnip' }, -- For luasnip users.
        -- { name = 'ultisnips' }, -- For ultisnips users.
        -- { name = 'snippy' }, -- For snippy users.
    }, {{name = 'buffer'}})
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
        {name = 'git'} -- You can specify the `git` source if [you were installed it](https://github.com/petertriho/cmp-git).
    }, {{name = 'buffer'}})
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({'/', '?'}, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {{name = 'buffer'}}
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({{name = 'path'}}, {{name = 'cmdline'}})
})

-- Set up lspconfig.
local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp
                                                                      .protocol
                                                                      .make_client_capabilities())


vim.lsp.config.gopls = {
  -- -remote=auto: share one gopls daemon across nvim instances, auto-starting
  -- it if absent. A literal host:port only *dials* -- it never spawns a
  -- daemon, so gopls died with exit 2 whenever nothing was listening.
  cmd = {"gopls", "-remote=auto"},
  filetypes = {"go", "gomod", "gowork", "gotmpl"},
  root_markers = {"go.mod", ".git"},
  capabilities = capabilities,
  flags = {
    debounce_text_changes = 50,  -- Lowered from default 150ms to 50ms
  },
  settings = {
    gopls = {
      --memoryMode = "DegradeClosed",  -- or "DegradeAll" if you're RAM-starved
      analyses = {
        unusedparams = false,
        unreachable = false,
      },
      matcher = "fuzzy",
      completionBudget = "200ms", -- cap per completion request
      experimentalPostfixCompletions = true,
      staticcheck = false,
      buildFlags = { "-tags=integration" },
      directoryFilters = {
        "-.git",
        "-node_modules",
        "-apps/cloud-ui",
        "-apps/admin-ui",
        "-vendor",
        "-bazel-out",
        "-.build"
      },
      codelenses = {
        generate = false,
        gc_details = false,
        test = false,
        tidy = false,
        upgrade_dependency = false,
        vendor = false,
      },
      usePlaceholders = true, -- gopls takes the flat name, not the dotted path
    }
  }
}
vim.lsp.enable('gopls')

vim.lsp.config.buf_ls = {
  cmd = {"buf", "language-server"},
  filetypes = {"proto"},
  root_markers = {".git"},
}
vim.lsp.enable('buf_ls')

-- terraform-ls. cmd/filetypes/root_markers come from nvim-lspconfig.
vim.lsp.config.terraformls = {
  capabilities = capabilities,
}
vim.lsp.enable('terraformls')

-- rust-analyzer. cmd/filetypes/root_dir come from nvim-lspconfig's
-- lsp/rust_analyzer.lua (it resolves the cargo *workspace* root via
-- `cargo metadata`, not the nearest Cargo.toml) -- only deltas here.
vim.lsp.config.rust_analyzer = {
  capabilities = capabilities,
  settings = {
    ["rust-analyzer"] = {
      cargo = {
        allTargets = true,       -- analyse tests/benches/examples too
        buildScripts = { enable = true },
      },
      check = {
        command = "clippy",
        extraArgs = { "--all-targets" },
      },
      procMacro = { enable = true },
      inlayHints = {
        parameterHints = { enable = false },
        typeHints = { enable = true },
      },
    },
  },
}
vim.lsp.enable('rust_analyzer')

-- local golang_organize_imports = function(bufnr, isPreflight)
--   local params = vim.lsp.util.make_range_params(nil, vim.lsp.util._get_offset_encoding(bufnr))
--   params.context = { only = { "source.organizeImports" } }
--
--   if isPreflight then
--     vim.lsp.buf_request(bufnr, "textDocument/codeAction", params, function() end)
--     return
--   end
--
--   local result = vim.lsp.buf_request_sync(bufnr, "textDocument/codeAction", params, 3000)
--   for _, res in pairs(result or {}) do
--     for _, r in pairs(res.result or {}) do
--       if r.edit then
--         vim.lsp.util.apply_workspace_edit(r.edit, vim.lsp.util._get_offset_encoding(bufnr))
--       else
--         vim.lsp.buf.execute_command(r.command)
--       end
--     end
--   end
-- end
--
-- vim.api.nvim_create_autocmd("LspAttach", {
--   group = vim.api.nvim_create_augroup("LspFormatting", {}),
--   callback = function(args)
--     local bufnr = args.buf
--     local client = vim.lsp.get_client_by_id(args.data.client_id)
--
--     if client.name == "gopls" then
--       -- hack: Preflight async request to gopls, which can prevent blocking when save buffer on first time opened
--       golang_organize_imports(bufnr, true)
--
--       vim.api.nvim_create_autocmd("BufWritePre", {
--         pattern = "*.go",
--         group = vim.api.nvim_create_augroup("LspGolangOrganizeImports." .. bufnr, {}),
--         callback = function()
--           golang_organize_imports(bufnr)
--         end,
--       })
--     end
--   end,
-- })
--


vim.lsp.config.pyright = {
  cmd = {"pyright-langserver", "--stdio"},
  filetypes = {"python"},
  root_markers = {"pyproject.toml", "setup.py", "setup.cfg", "requirements.txt", "Pipfile", ".git"},
  capabilities = capabilities,
  on_attach = function(client, bufnr)
    -- your custom keybindings or settings, e.g.:
    local buf_map = function(mode, lhs, rhs)
      vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, { noremap=true, silent=true })
    end

    buf_map('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>')
    buf_map('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>')
    -- etc.
  end,
  settings = {
    python = {
      analysis = {
        typeCheckingMode = "basic", -- or "strict"
        autoSearchPaths = true,
        useLibraryCodeForTypes = true,
      }
    }
  }
}
vim.lsp.enable('pyright')
