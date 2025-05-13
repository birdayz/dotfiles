local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
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
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			local configs = require("nvim-treesitter.configs")

			configs.setup({
				ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "javascript", "html", "hcl", "terraform" },
				sync_install = false,
				highlight = { enable = true },
				indent = { enable = true },
			})
		end,
	},
	{
		"nvimdev/lspsaga.nvim",
		config = function()
			require("lspsaga").setup({
				implement = {
					enable = true,
				},
				ui = {
					code_action = "",
					enable = true,
					virtual_text = true,
				},
			})
		end,
		dependencies = {
			"nvim-treesitter/nvim-treesitter", -- optional
			"nvim-tree/nvim-web-devicons", -- optional
		},
	},
	"neovim/nvim-lspconfig",
	"hrsh7th/cmp-nvim-lsp-signature-help",
	"folke/tokyonight.nvim",
	{
		"ahmedkhalf/project.nvim",
		event = "VeryLazy",
		config = function()
			require("project_nvim").setup({
				-- Recommended settings
				detection_methods = { "pattern", "lsp" },
				patterns = { ".git", "Makefile", "package.json", "pyproject.toml" },
				show_hidden = true,
			})
			require("telescope").load_extension("projects")
		end,
		keys = {
			{ "<leader>fp", "<cmd>Telescope projects<cr>", desc = "Projects" },
		},
	},
	"hrsh7th/cmp-nvim-lsp",
	"hrsh7th/cmp-buffer",
	"hrsh7th/cmp-path",
	"hrsh7th/cmp-cmdline",
	"hrsh7th/nvim-cmp",
	"L3MON4D3/LuaSnip",
	"saadparwaiz1/cmp_luasnip",
	"sbdchd/neoformat",
	{
		"nvim-lualine/lualine.nvim",
		config = function()
			require("lualine").setup({
				options = {
					theme = "auto",
					icons_enabled = true,
					theme = "tokyonight",
					component_separators = { left = "", right = "" },
					section_separators = { left = "", right = "" },
					disabled_filetypes = {},
					always_divide_middle = true,
				},
				sections = {
					lualine_x = {
						function()
							local clients = vim.lsp.get_active_clients({ bufnr = 0 })
							if #clients > 0 then
								return "LSP"
							end
							return ""
						end,
					},

					lualine_b = { "branch", "diff" },
					lualine_c = {
						{
							"filename",
							path = 1, -- 1 = relative path; 2 = absolute
							fmt = function(str)
								return str:gsub(vim.env.HOME, "~")
							end,
						},
					},
				},
			})
		end,
	},
	"tpope/vim-fugitive",
	{
		"numToStr/Comment.nvim",
		opts = {
			-- add any options here
		},
		lazy = false,
	},
	{
		"goolord/alpha-nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
		config = function()
			require("alpha").setup(require("alpha.themes.startify").config)
		end,
	},
	{ "nvim-telescope/telescope.nvim", tag = "0.1.7" },
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
	},
	{
		"nvim-telescope/telescope-file-browser.nvim",
		dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
	},
})

vim.cmd([[set tabstop=2]])
vim.cmd([[set shiftwidth=2]])
vim.cmd([[colorscheme cyberdream]])
vim.cmd([[set mouse=]])

-- Automatically use correct indentation when pressing i on empty line
vim.cmd([[function! IndentWithI()
    if len(getline('.')) == 0
        return "\"_cc"
    else
        return "i"
    endif
endfunction
nnoremap <expr> i IndentWithI()]])

require("lsp")
require("treesitter")
require("_telescope")

vim.cmd([[set undodir=~/.vim/undodir]])
vim.cmd([[set undofile]])
vim.cmd([[set clipboard+=unnamedplus]])

-- Hotkeys
vim.cmd([[nnoremap <F10> <cmd>vertical resize +5<cr>]])
vim.cmd([[nnoremap <F11> <cmd>cnext<cr>]])
vim.cmd([[nnoremap <F9> <cmd>vertical resize -5<cr>]])
vim.cmd([[nnoremap <F2> <cmd>Telescope oldfiles<cr>]])
vim.keymap.set("n", "<F1>", ":Telescope file_browser path=%:p:h select_buffer=true<CR>")
vim.cmd([[command! Filez execute (len(system('git rev-parse'))) ? ':Telescope find_files' : ':Telescope git_files']])
vim.cmd([[map <F3> :Filez<CR>]])
vim.cmd([[map <F8> :Telescope projects<CR>]])
vim.cmd(
	[[nnoremap <F4> <cmd>lua require('telescope.builtin').lsp_document_symbols({fname_width = 160,show_line=false,symbol_width=70})<cr>]]
)
vim.cmd(
	[[nnoremap <F5> <cmd>lua require('telescope.builtin').live_grep{ file_ignore_patterns = {"node_modules/", ".git/", ".cache", "%.o", "%.a", "%.out", "%.class", "%.pdf", "%.mkv", "%.mp4", "%.zip"}, cwd = vim.fn.systemlist("git rev-parse --show-toplevel")[1] }<cr>]]
)
vim.cmd([[nnoremap <F6> <cmd>Neotree toggle<cr>]])
vim.cmd([[nnoremap <leader>fg <cmd>Telescope live_grep<cr>]])
vim.cmd([[nnoremap <leader>fb <cmd>Telescope buffers<cr>]])
vim.cmd([[nnoremap <leader>fh <cmd>Telescope help_tags<cr>]])
vim.cmd([[nnoremap <leader>p <cmd>lua require('window-picker').pick_window()<CR>]])

vim.cmd([[set number]])
vim.cmd([[set expandtab]])
vim.cmd([[autocmd FileType fugitive nmap <buffer> q gq]])
vim.cmd("highlight Pmenu guibg=NONE")
vim.api.nvim_set_hl(0, "PmenuBorder", { fg = "grey" })
vim.cmd([[nnoremap <silent> gh <cmd>lua require'lspsaga.provider'.lsp_finder()<CR>]])
vim.cmd([[nnoremap <silent> g? <cmd>lua vim.diagnostic.open_float()<CR>]])
vim.cmd([[set updatetime=100]])
vim.cmd([[
function! ToggleQuickFix()
    if empty(filter(getwininfo(), 'v:val.quickfix'))
        copen
    else
        cclose
    endif
endfunction
]])
vim.cmd([[nnoremap <silent> <F7> :call ToggleQuickFix()<cr>]])
vim.cmd([[set signcolumn=no]])
vim.opt.mouse = ""

vim.opt.foldtext = "v:lua.vim.treesitter.foldtext()"
vim.opt.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.cmd([[set foldmethod=expr]])
vim.opt.foldlevel = 99
