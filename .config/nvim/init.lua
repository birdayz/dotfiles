local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	"almo7aya/openingh.nvim",
	"mattn/vim-goimports",
	"nvim-lua/plenary.nvim",
	{
		"scottmckendry/cyberdream.nvim",
		lazy = false,
		priority = 1000,
	},
	{
		"MeanderingProgrammer/render-markdown.nvim",
		ft = { "markdown" },
		dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
		opts = {},
	},
	{
		-- `main` branch: full rewrite, requires nvim >= 0.12 and the tree-sitter CLI.
		-- Does NOT support lazy-loading, hence lazy = false.
		"nvim-treesitter/nvim-treesitter",
		branch = "main",
		lazy = false,
		build = ":TSUpdate",
		-- Actual setup lives in lua/treesitter.lua (required below).
	},
	"neovim/nvim-lspconfig",
	"hrsh7th/nvim-cmp",
	"hrsh7th/cmp-nvim-lsp",
	"hrsh7th/cmp-nvim-lsp-signature-help",
	"hrsh7th/cmp-buffer",
	"hrsh7th/cmp-path",
	"hrsh7th/cmp-cmdline",
	"L3MON4D3/LuaSnip",
	"saadparwaiz1/cmp_luasnip",
	{
		"nvim-lualine/lualine.nvim",
		opts = {
			sections = {
				lualine_b = { "branch", "diff" },
				lualine_c = { { "filename", path = 1 } }, -- relative to cwd, else ~/...
				lualine_x = {
					function()
						return #vim.lsp.get_clients({ bufnr = 0 }) > 0 and "LSP" or ""
					end,
				},
			},
		},
	},
	"tpope/vim-fugitive",
	{
		"goolord/alpha-nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("alpha").setup(require("alpha.themes.startify").config)
		end,
	},
	{ "nvim-telescope/telescope.nvim", branch = "master" },
	{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
	{
		"nvim-telescope/telescope-file-browser.nvim",
		dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
	},
})

-- Options
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.number = true
vim.opt.signcolumn = "no"
vim.opt.mouse = ""
vim.opt.updatetime = 100
vim.opt.undofile = true
vim.opt.clipboard:append("unnamedplus")
vim.opt.shada = "!,'20,<50,s10,h" -- limit oldfiles to prevent hangs

vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.opt.foldtext = "" -- the folded line itself, highlighted
vim.opt.foldlevel = 99

vim.cmd.colorscheme("cyberdream")
vim.cmd.highlight("Pmenu guibg=NONE")
vim.api.nvim_set_hl(0, "PmenuBorder", { fg = "grey" })

require("lsp")
require("treesitter")
require("_telescope")
local projects = require("projects")

-- Keymaps
local map = vim.keymap.set
local builtin = require("telescope.builtin")

local function git_root()
	local root = vim.fn.systemlist("git rev-parse --show-toplevel")[1]
	return vim.v.shell_error == 0 and root or nil
end

-- `i` on an empty line starts at the correct indentation.
map("n", "i", function()
	return vim.fn.getline(".") == "" and '"_cc' or "i"
end, { expr = true })

map("n", "<F1>", ":Telescope file_browser path=%:p:h select_buffer=true<CR>")
map("n", "<F2>", "<cmd>Telescope oldfiles<cr>")
map("n", "<F3>", function()
	if git_root() then
		builtin.git_files()
	else
		builtin.find_files()
	end
end)
map("n", "<F4>", function()
	builtin.lsp_document_symbols({ fname_width = 160, show_line = false, symbol_width = 70 })
end)
map("n", "<F5>", function()
	builtin.live_grep({
		file_ignore_patterns = {
			"node_modules/",
			".git/",
			".cache",
			"%.o",
			"%.a",
			"%.out",
			"%.class",
			"%.pdf",
			"%.mkv",
			"%.mp4",
			"%.zip",
		},
		cwd = git_root(),
	})
end)
map("n", "<F7>", function()
	for _, win in ipairs(vim.fn.getwininfo()) do
		if win.quickfix == 1 then
			vim.cmd.cclose()
			return
		end
	end
	vim.cmd.copen()
end, { silent = true })
map("n", "<F8>", projects.pick)
map("n", "<F9>", "<cmd>vertical resize -5<cr>")
map("n", "<F10>", "<cmd>vertical resize +5<cr>")
map("n", "<F11>", "<cmd>cnext<cr>")
map("n", "<leader>fg", "<cmd>Telescope live_grep<cr>")
map("n", "<leader>fb", "<cmd>Telescope buffers<cr>")
map("n", "<leader>fh", "<cmd>Telescope help_tags<cr>")
map("n", "<leader>fp", projects.pick)

vim.api.nvim_create_autocmd("FileType", {
	pattern = "fugitive",
	callback = function(ev)
		map("n", "q", "gq", { buffer = ev.buf, remap = true })
	end,
})
