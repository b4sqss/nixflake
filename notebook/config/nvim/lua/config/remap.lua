vim.g.mapleader = " "  -- set global leader key

vim.keymap.set("n", "<leader>fm", vim.cmd.Ex)
vim.keymap.set('', '<Space>', '<NOP>')

-- Change default fold command
vim.keymap.set('n', 'zf', 'zc')
vim.keymap.set('n', 'zc', 'zf')

-- Easier edit commands
vim.keymap.set('n', '<Leader>ee', ':edit ')
vim.keymap.set('n', '<Leader>es', ':split ')
vim.keymap.set('n', '<Leader>ev', ':vsplit ')

-- For easy macro playback; note that this overrides entering Ex mode with Q
vim.keymap.set('n', 'Q', '@q')

-- Move selected lines up and down
-- See https://stackoverflow.com/questions/41084565/moving-multiple-lines-in-vim-visual-mode
vim.cmd([[
xnoremap <C-S-j> :m'>+<CR>gv=gv
xnoremap <C-S-k>  :m-2<CR>gv=gv
]])

-- Global substitute
vim.keymap.set('n', '<Leader>s', ':%s/')
vim.keymap.set('v', '<Leader>s', ':s/')

-- Enter/leave insert mode when entering/leaving a terminal
-- You could use nvim_list_bufs
vim.cmd[[
  autocmd BufEnter term://* startinsert
  autocmd BufLeave term://* stopinsert
]]

