local actions = require("telescope.actions")
local telescope = require("telescope")

telescope.setup({
	pickers = {
		lsp_document_symbols = {
			theme = "ivy",
			fname_width = 500,
		},
		find_files = {
			hidden = true,
			-- NOTE: do NOT set cwd here -- it is evaluated once at setup time and
			-- would pin find_files to the startup buffer's dir forever.
		},
		git_files = {
			git_command = { "git", "ls-files", "--exclude-standard", "--cached", "--deduplicate" },
			previewer = false,
		},
		oldfiles = {
			sorter = require("telescope.sorters").fuzzy_with_index_bias(),
			theme = "ivy",
			previewer = false,
		},
	},
	extensions = {
		file_browser = {
			hidden = true,
			respect_gitignore = false,
		},
	},
	defaults = {
		vimgrep_arguments = {
			"rg",
			"--hidden",
			"--color=never",
			"--no-heading",
			"--with-filename",
			"--line-number",
			"--column",
			"--smart-case",
			"-u", -- also search gitignored files
		},
		file_ignore_patterns = { ".git/", "node_modules/", "dist/", "%.lock" },
		layout_config = { height = 0.95 },
		path_display = function(_, path)
			-- NOTE: must return exactly ONE value. gsub returns (str, count) and
			-- telescope now reads the 2nd return as a highlight-style table.
			local shortened = path:gsub("^" .. vim.pesc(os.getenv("HOME")), "~")
			return shortened
		end,
		mappings = {
			i = {
				["<esc>"] = actions.close,
				["<F1>"] = actions.close,
				["<F2>"] = actions.close,
				["<F3>"] = actions.close,
			},
		},
	},
})

telescope.load_extension("fzf")
telescope.load_extension("file_browser")
