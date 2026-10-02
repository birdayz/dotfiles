-- Replaces project.nvim: cd into the current file's project root, and pick a
-- recent project (derived from open buffers and oldfiles) with telescope.
local M = {}

local markers = { ".git", "Makefile", "package.json", "pyproject.toml" }

local function is_file(name)
	return name ~= "" and not name:match("^%a+://")
end

vim.api.nvim_create_autocmd("BufEnter", {
	group = vim.api.nvim_create_augroup("user_project_root", { clear = true }),
	callback = function(ev)
		if vim.bo[ev.buf].buftype ~= "" or not is_file(vim.api.nvim_buf_get_name(ev.buf)) then
			return
		end
		local root = vim.fs.root(ev.buf, markers)
		if root and root ~= vim.fn.getcwd() then
			vim.api.nvim_set_current_dir(root)
		end
	end,
})

local function recent_roots()
	local files = {}
	for _, buf in ipairs(vim.api.nvim_list_bufs()) do
		if vim.bo[buf].buflisted then
			table.insert(files, vim.api.nvim_buf_get_name(buf))
		end
	end
	vim.list_extend(files, vim.v.oldfiles)

	local seen, roots = {}, {}
	for _, file in ipairs(files) do
		local root = is_file(file) and vim.fs.root(file, markers)
		if root and not seen[root] then
			seen[root] = true
			table.insert(roots, root)
		end
	end
	return roots
end

function M.pick()
	local actions = require("telescope.actions")
	local state = require("telescope.actions.state")
	require("telescope.pickers")
		.new({}, {
			prompt_title = "Projects",
			finder = require("telescope.finders").new_table({ results = recent_roots() }),
			sorter = require("telescope.config").values.generic_sorter({}),
			attach_mappings = function(bufnr)
				actions.select_default:replace(function()
					local entry = state.get_selected_entry()
					actions.close(bufnr)
					if entry then
						vim.api.nvim_set_current_dir(entry[1])
						require("telescope.builtin").find_files()
					end
				end)
				return true
			end,
		})
		:find()
end

return M
