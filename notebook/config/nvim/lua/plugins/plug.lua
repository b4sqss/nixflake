return {
	-- the colorscheme should be available when starting Neovim
	{ "shaunsingh/nord.nvim", 
	config = function()
		-- load the colorscheme here
		vim.cmd([[colorscheme nord]])
	end,
},

-- I have a separate config.mappings file where I require which-key.
-- With lazy the plugin will be automatically loaded when it is required somewhere
{ "folke/which-key.nvim", lazy = false },

{
	"nvim-neorg/neorg",
    lazy = false,
    config = true,
},

{
	"lervag/vimtex",
	lazy = false,     -- we don't want to lazy load VimTeX
},

{
	"KeitaNakamura/tex-conceal.vim",
},

{
	"tpope/vim-fugitive"
},

{
	'nvim-telescope/telescope.nvim', tag = '0.1.8',
	-- or                              , branch = '0.1.x',
	dependencies = { 'nvim-lua/plenary.nvim' }
},

{
	"ThePrimeagen/harpoon"
},

{
	"nvim-treesitter/nvim-treesitter",
},
{
	"mbbill/undotree"
},
{
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' }
},


{
        "neovim/nvim-lspconfig",
    },

{
"mateusbraga/vim-spell-pt-br"
},

-- lazy.nvim
{
  "folke/noice.nvim",
  event = "VeryLazy",
  opts = {
    -- add any options here
  },
  dependencies = {
    -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
    "MunifTanjim/nui.nvim",
    -- OPTIONAL:
    --   `nvim-notify` is only needed, if you want to use the notification view.
    --   If not available, we use `mini` as the fallback
    "rcarriga/nvim-notify",
    }
},
}
