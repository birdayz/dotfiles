-- Global mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
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
        vim.keymap.set({'n', 'v'}, '<space>ca', ':Lspsaga code_action<CR>', opts)
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
    -- snippet = {
    --     -- REQUIRED - you must specify a snippet engine
    --     expand = function(args)
    --         -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
    --         require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
    --         -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
    --         -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    --     end
    -- },
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


require('lspconfig').gopls.setup({
  cmd = {"gopls", "-remote=localhost:37373"},
  flags = {
    debounce_text_changes = 50,  -- Lowered from default 150ms to 50ms
  },
  settings = {
    gopls = {
      --memoryMode = "DegradeClosed",  -- or "DegradeAll" if you’re RAM-starved
      analyses = {
        unusedparams = false,
        unreachable = false,
      },
      matcher = "fuzzy", -- fastest matching mode
      completionBudget = "200ms", -- 2 full seconds for giant completion requests
      deepCompletion = true, -- complete deeply inside nested structs
      matcher = "fuzzy", -- fastest matching mode
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
      ["ui.completion.usePlaceholders"] = true,
    }
  }
})
--require'lspconfig'.gopls.setup {capabilities = capabilities}
--vim.cmd [[autocmd BufWritePre *.go :silent! lua vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })]]
vim.keymap.set('n', 'K', '<cmd>Lspsaga hover_doc')

require'lspconfig'.buf_ls.setup{}

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


require('lspconfig').pyright.setup({
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
})
