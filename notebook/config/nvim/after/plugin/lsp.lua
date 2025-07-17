return {

    {
        "neovim/nvim-lspconfig",
        config = function()

            vim.lsp.enable('pyright')

            vim.lsp.config('rust_analyzer', {
                -- Server-specific settings. See `:help lsp-quickstart`
                settings = {
                    ['rust-analyzer'] = {},
                },
            })

           --  vim.lsp.config('ltex_plus', {
           --      settings = {
           --          ltex = {
           --              language = 'pt-BR',
           --          },
           --      },
           --  })
        end,
    }
}
