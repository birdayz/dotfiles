-- Diagnostics. [d / ]d are Neovim defaults; show what they land on.
vim.diagnostic.config({
	jump = {
		on_jump = function(_, bufnr)
			vim.diagnostic.open_float({ bufnr = bufnr, scope = "cursor", focus = false })
		end,
	},
})
vim.keymap.set("n", "<space>e", vim.diagnostic.open_float)
vim.keymap.set("n", "g?", vim.diagnostic.open_float)
vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist)

-- Buffer-local maps once a server attaches. K (hover) and omnifunc are Neovim defaults.
vim.api.nvim_create_autocmd("LspAttach", {
	group = vim.api.nvim_create_augroup("UserLspConfig", {}),
	callback = function(ev)
		local function map(mode, lhs, rhs)
			vim.keymap.set(mode, lhs, rhs, { buffer = ev.buf })
		end
		map("n", "gD", vim.lsp.buf.declaration)
		map("n", "gd", vim.lsp.buf.definition)
		map("n", "gi", vim.lsp.buf.implementation)
		map("n", "gr", vim.lsp.buf.references)
		map("n", "<C-k>", vim.lsp.buf.signature_help)
		map("n", "<space>D", vim.lsp.buf.type_definition)
		map("n", "<space>rn", vim.lsp.buf.rename)
		map({ "n", "v" }, "<space>ca", vim.lsp.buf.code_action)
		map("n", "<space>f", function()
			vim.lsp.buf.format({ async = false })
		end)
		map("n", "<space>i", function()
			vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })
		end)
	end,
})

-- Completion
local cmp = require("cmp")

cmp.setup({
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
		debounce = 20, -- ms to wait after keystroke before triggering completion
		throttle = 20, -- ms to wait before triggering completion again
		fetching_timeout = 50, -- timeout for LSP responses
	},
	-- REQUIRED: without this, confirming any LSP snippet completion (gopls
	-- sends them, usePlaceholders=true) errors out or inserts raw ${1:...}.
	snippet = {
		expand = function(args)
			require("luasnip").lsp_expand(args.body)
		end,
	},
	window = {
		completion = cmp.config.window.bordered(),
		documentation = cmp.config.window.bordered(),
	},
	mapping = cmp.mapping.preset.insert({
		["<C-b>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-Space>"] = cmp.mapping.complete(),
		["<C-e>"] = cmp.mapping.abort(),
		["<CR>"] = cmp.mapping.confirm({ select = true }),
	}),
	sources = cmp.config.sources({
		{ name = "nvim_lsp_signature_help" },
		{ name = "nvim_lsp" },
		{ name = "luasnip" },
	}, { { name = "buffer" } }),
})

cmp.setup.cmdline({ "/", "?" }, {
	mapping = cmp.mapping.preset.cmdline(),
	sources = { { name = "buffer" } },
})

cmp.setup.cmdline(":", {
	mapping = cmp.mapping.preset.cmdline(),
	sources = cmp.config.sources({ { name = "path" } }, { { name = "cmdline" } }),
})

-- Servers. cmd/filetypes/root detection come from nvim-lspconfig's lsp/*.lua;
-- only overrides live here.
vim.lsp.config("*", {
	capabilities = require("cmp_nvim_lsp").default_capabilities(),
})

vim.lsp.config("gopls", {
	flags = {
		debounce_text_changes = 50, -- default 150
	},
	settings = {
		gopls = {
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
				"-.build",
			},
			codelenses = {
				generate = false,
				test = false,
				tidy = false,
				upgrade_dependency = false,
				vendor = false,
			},
			usePlaceholders = true, -- gopls takes the flat name, not the dotted path
		},
	},
})

vim.lsp.config("rust_analyzer", {
	settings = {
		["rust-analyzer"] = {
			cargo = {
				allTargets = true, -- analyse tests/benches/examples too
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
})

vim.lsp.config("pyright", {
	settings = {
		python = {
			analysis = {
				typeCheckingMode = "basic",
			},
		},
	},
})

vim.lsp.enable({ "gopls", "buf_ls", "terraformls", "rust_analyzer", "pyright" })
