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
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function () 
      local configs = require("nvim-treesitter.configs")

      configs.setup({
          ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "javascript", "html" },
          sync_install = false,
          highlight = { enable = true },
          indent = { enable = true },  
        })
    end
 	},
  "neovim/nvim-lspconfig",
  "folke/tokyonight.nvim",
	"hrsh7th/cmp-nvim-lsp",
	"hrsh7th/cmp-buffer",
	"hrsh7th/cmp-path",
	"hrsh7th/cmp-cmdline",
	"hrsh7th/nvim-cmp",
	"L3MON4D3/LuaSnip",
	"saadparwaiz1/cmp_luasnip",
	"sbdchd/neoformat",
	"nvim-lualine/lualine.nvim",
	"tpope/vim-fugitive",
	{
    'numToStr/Comment.nvim',
    opts = {
        -- add any options here
    },
    lazy = false,
	},
	{
    'goolord/alpha-nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    config = function ()
        require'alpha'.setup(require'alpha.themes.startify'.config)
    end
	},
	{
    'nvimdev/lspsaga.nvim',
    config = function()
        require('lspsaga').setup({
					implement = {
					enable = true
				},
    			ui = {
        		code_action = '',
						enable = true,
						virtual_text = true
    			}
				})
    end,
    dependencies = {
        'nvim-treesitter/nvim-treesitter', -- optional
        'nvim-tree/nvim-web-devicons'     -- optional
    }
	},
	{
  	"ray-x/lsp_signature.nvim",
  	event = "VeryLazy",
  	opts = {},
  	config = function(_, opts) require'lsp_signature'.setup(opts) end
	},
	"nvim-telescope/telescope.nvim",
	{ 'nvim-telescope/telescope-fzf-native.nvim', build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build' },
	{
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
	},
	{
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
      "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
      {
        's1n7ax/nvim-window-picker',
        version = '2.*',
        config = function()
            require 'window-picker'.setup({
                filter_rules = {
                    include_current_win = false,
                    autoselect_one = true,
                    -- filter using buffer options
                    bo = {
                        -- if the file type is one of following, the window will be ignored
                        filetype = { 'neo-tree', "neo-tree-popup", "notify" },
                        -- if the buffer type is one of following, the window will be ignored
                        buftype = { 'terminal', "quickfix" },
                    },
            },
        })
        end,
      },
    }
	}
})



vim.cmd[[set tabstop=2]]
vim.cmd[[set shiftwidth=2]]
vim.cmd[[colorscheme tokyonight-night]]
vim.cmd[[set mouse=]]

-- Automatically use correct indentation when pressing i on empty line
vim.cmd[[function! IndentWithI()
    if len(getline('.')) == 0
        return "\"_cc"
    else
        return "i"
    endif
endfunction
nnoremap <expr> i IndentWithI()]]

require("lsp")
require("treesitter")
require("_telescope")
require("_lualine")

vim.cmd[[set undodir=~/.vim/undodir]]
vim.cmd[[set undofile]]
vim.cmd[[set clipboard+=unnamedplus]]

-- Hotkeys
vim.cmd[[nnoremap <F10> <cmd>vertical resize +5<cr>]]
vim.cmd[[nnoremap <F11> <cmd>cnext<cr>]]
vim.cmd[[nnoremap <F9> <cmd>vertical resize -5<cr>]]
vim.cmd[[nnoremap <F2> <cmd>Telescope oldfiles<cr>]]
vim.keymap.set("n", "<F1>", ":Telescope file_browser path=%:p:h select_buffer=true<CR>")
vim.cmd[[command! Filez execute (len(system('git rev-parse'))) ? ':Telescope find_files' : ':Telescope git_files']]
vim.cmd[[map <F3> :Filez<CR>]]
vim.cmd[[map <F8> :Telescope projects<CR>]]
vim.cmd[[nnoremap <F4> <cmd>Telescope lsp_document_symbols<cr>]]
vim.cmd[[nnoremap <F5> <cmd>lua require('telescope.builtin').live_grep{ file_ignore_patterns = {".git/", ".cache", "%.o", "%.a", "%.out", "%.class", "%.pdf", "%.mkv", "%.mp4", "%.zip"}, cwd = vim.fn.systemlist("git rev-parse --show-toplevel")[1] }<cr>]]
vim.cmd[[nnoremap <F6> <cmd>Neotree toggle<cr>]]
vim.cmd[[nnoremap <leader>fg <cmd>Telescope live_grep<cr>]]
vim.cmd[[nnoremap <leader>fb <cmd>Telescope buffers<cr>]]
vim.cmd[[nnoremap <leader>fh <cmd>Telescope help_tags<cr>]]
vim.cmd[[nnoremap <leader>p <cmd>lua require('window-picker').pick_window()<CR>]]

require 'window-picker'.setup({
    -- type of hints you want to get
    -- following types are supported
    -- 'statusline-winbar' | 'floating-big-letter'
    -- 'statusline-winbar' draw on 'statusline' if possible, if not 'winbar' will be
    -- 'floating-big-letter' draw big letter on a floating window
    -- used
    hint = 'floating-big-letter',

    -- when you go to window selection mode, status bar will show one of
    -- following letters on them so you can use that letter to select the window
    selection_chars = 'FJDKSLA;CMRUEIWOQP',

    -- This section contains picker specific configurations
    picker_config = {
        statusline_winbar_picker = {
            -- You can change the display string in status bar.
            -- It supports '%' printf style. Such as `return char .. ': %f'` to display
            -- buffer file path. See :h 'stl' for details.
            selection_display = function(char, windowid)
                return '%=' .. char .. '%='
            end,

            -- whether you want to use winbar instead of the statusline
            -- "always" means to always use winbar,
            -- "never" means to never use winbar
            -- "smart" means to use winbar if cmdheight=0 and statusline if cmdheight > 0
            use_winbar = 'never', -- "always" | "never" | "smart"
        },

        floating_big_letter = {
            -- window picker plugin provides bunch of big letter fonts
            -- fonts will be lazy loaded as they are being requested
            -- additionally, user can pass in a table of fonts in to font
            -- property to use instead

            font = 'ansi-shadow', -- ansi-shadow |
        },
    },

    -- whether to show 'Pick window:' prompt
    show_prompt = true,

    -- prompt message to show to get the user input
    prompt_message = 'Pick window: ',

    -- if you want to manually filter out the windows, pass in a function that
    -- takes two parameters. You should return window ids that should be
    -- included in the selection
    -- EX:-
    -- function(window_ids, filters)
    --    -- folder the window_ids
    --    -- return only the ones you want to include
    --    return {1000, 1001}
    -- end
    filter_func = nil,

    -- following filters are only applied when you are using the default filter
    -- defined by this plugin. If you pass in a function to "filter_func"
    -- property, you are on your own
    filter_rules = {
        -- when there is only one window available to pick from, use that window
        -- without prompting the user to select
        autoselect_one = false,

        -- whether you want to include the window you are currently on to window
        -- selection or not
        include_current_win = false,

        -- filter using buffer options
        bo = {
            -- if the file type is one of following, the window will be ignored
            filetype = { 'NvimTree', 'neo-tree', 'notify' },

            -- if the file type is one of following, the window will be ignored
            buftype = { 'terminal' },
        },

        -- filter using window options
        wo = {},

        -- if the file path contains one of following names, the window
        -- will be ignored
        file_path_contains = {},

        -- if the file name contains one of following names, the window will be
        -- ignored
        file_name_contains = {},
    },

    -- You can pass in the highlight name or a table of content to set as
    -- highlight
    highlights = {
        statusline = {
            focused = {
                fg = '#ededed',
                bg = '#e35e4f',
                bold = true,
            },
            unfocused = {
                fg = '#ededed',
                bg = '#44cc41',
                bold = true,
            },
        },
        winbar = {
            focused = {
                fg = '#ededed',
                bg = '#e35e4f',
                bold = true,
            },
            unfocused = {
                fg = '#ededed',
                bg = '#44cc41',
                bold = true,
            },
        },
    },
})
--vim.cmd[[set scrolloff=999]]
vim.cmd[[set number]]
vim.cmd[[set expandtab]]
vim.cmd[[autocmd FileType fugitive nmap <buffer> q gq]]

vim.cmd[[nnoremap <silent> gh <cmd>lua require'lspsaga.provider'.lsp_finder()<CR>]]
vim.cmd[[nnoremap <silent> g? <cmd>lua vim.diagnostic.open_float()<CR>]]
vim.cmd[[set signcolumn=yes]]
vim.cmd[[set updatetime=100]]
vim.cmd[[
function! ToggleQuickFix()
    if empty(filter(getwininfo(), 'v:val.quickfix'))
        copen
    else
        cclose
    endif
endfunction
]]
vim.cmd[[nnoremap <silent> <F7> :call ToggleQuickFix()<cr>]]
