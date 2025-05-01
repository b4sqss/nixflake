require "nvchad.mappings"

-- add yours here

local map = vim.keymap.set

map("n", ";", ":", { desc = "CMD enter command mode" })
map("i", "jk", "<ESC>")
map("n", "<space>ff", ":Telescope file_browser<CR>")
map("n", "<space>fa", ":Telescope find_files <CR>")
-- map("i", "<C-w>", "<C-w>")
