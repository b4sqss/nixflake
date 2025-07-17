vim.opt.spelllang = 'pt'

vim.b.SPELL_PORT = true

function TogglePTSpell()
  if vim.b.SPELL_PORT then  -- if portuguese spelling is on, turn it off
    vim.opt.spelllang:append('en')
    vim.opt.spelllang:remove('pt')
    vim.b.SPELL_PORT = false
  else -- if Slovene spelling is off, turn it on
    vim.opt.spelllang:append('pt')
    vim.opt.spelllang:remove('en')
    vim.b.SPELL_PORT = true
  end
end

vim.keymap.set('n', '<Leader>zz', '<Cmd>set spell!<CR>')
vim.keymap.set('n', '<Leader>zs', TogglePTSpell)
