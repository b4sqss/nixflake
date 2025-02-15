require "nvchad.options"

-- add yours here!

local o = vim.o
o.cursorlineopt ='both' -- to enable cursorline!

vim.api.nvim_create_user_command("Explore", "Telescope file_browser path=%:p:h", {})
