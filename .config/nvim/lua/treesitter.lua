-- nvim-treesitter `main` branch.
--
-- No modules, no `configs.setup{}`. The plugin only installs parsers+queries;
-- highlighting/indent/folding are Neovim features we turn on ourselves.
--
-- Parsers and queries install to stdpath("data").."/site" (~/.local/share/nvim/site),
-- which the plugin prepends to 'runtimepath'.

local ts = require("nvim-treesitter")

ts.install({
	"bash",
	"c",
	"go",
	"gomod",
	"hcl",
	"html",
	"javascript",
	"json",
	"lua",
	"markdown",
	"markdown_inline",
	"proto",
	"python",
	"query",
	"rust",
	"terraform",
	"toml",
	"vim",
	"vimdoc",
	"yaml",
})

vim.api.nvim_create_autocmd("FileType", {
	group = vim.api.nvim_create_augroup("user_treesitter", { clear = true }),
	callback = function(ev)
		local lang = vim.treesitter.language.get_lang(ev.match)
		-- add() returns nil when no parser is installed for this language.
		if not lang or not vim.treesitter.language.add(lang) then
			return
		end

		vim.treesitter.start(ev.buf, lang)
		vim.bo[ev.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
	end,
})
