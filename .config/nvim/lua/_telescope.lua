-- You dont need to set any of these options. These are the default ones. Only
-- the loading is important
local actions = require("telescope.actions")
local utils = require("telescope.utils")
require('telescope').setup {
  pickers = {
    lsp_document_symbols = {
      theme = "ivy",
      fname_width = 500
    },
    find_files = {
      hidden = true,
			cwd = utils.buffer_dir()
    },
    git_files = {
      git_command = { "git", "ls-files", "--exclude-standard", "--cached", "--deduplicate" },
      previewer = false
    },
    oldfiles = {
      sorter = require("telescope.sorters").fuzzy_with_index_bias(),
      theme = "ivy",
      previewer = false
    },
  },
  extensions = {
    file_browser = {
      hidden = true,
      respect_gitignore = false
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
    vimgrep_arguments = {
      'rg',
      '--hidden',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case',
      '-u' -- thats the new thing
    },
    find_command = { "fd", "--type", "f", "--strip-cwd-prefix" },
    file_ignore_patterns = { ".git/", "node_modules/", "dist/", "%.lock" },
    layout_config = { height = 0.95 },
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
require("telescope").load_extension "file_browser"
require("telescope").load_extension "projects"
