return {
	{
		"stevearc/conform.nvim",
		-- event = 'BufWritePre', -- uncomment for format on save
		opts = require "configs.conform",
	},

	{
		"neovim/nvim-lspconfig",
		config = function()
			require "configs.lspconfig"
		end,
	},

	{
		"nvim-treesitter/nvim-treesitter",
		opts = {
			ensure_installed = {
				"vim", "lua", "vimdoc",
				"html", "css", "rust", "python"
			},
		},
	},

	{
		"mcchrish/nnn.vim",
		lazy = false
	},

  {
 "nvim-telescope/telescope.nvim",
    opts = function()
      local conf = require "nvchad.configs.telescope"
      return conf
    end,
    lazy = false
  },

	{
		"nvim-telescope/telescope-file-browser.nvim",
		dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
		lazy = false,
    config = function()
      require("telescope").setup {
        file_browser ={
          hijack_netrw = true
        },
      }

      require("telescope").load_extension "file_browser"
  end,
	},
}
