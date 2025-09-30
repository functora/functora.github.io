local ok, avante_lib = pcall(require, "avante_lib")
if ok then
  avante_lib.load()
  require("avante").setup({provider = "gemini"})
end

local ok, minuet = pcall(require, "minuet")
if ok then
  minuet.setup({
    provider = "gemini",
    provider_options = {
      gemini = {
        model = 'gemini-2.5-flash-lite'
      }
    },
    virtualtext = {
        auto_trigger_ft = {},
        keymap = {
            -- accept whole completion
            accept = '<M-y>',
            -- accept one line
            accept_line = '<M-l>',
            -- accept n lines (prompts for number)
            -- e.g. "A-z 2 CR" will accept 2 lines
            accept_n_lines = '<M-z>',
            -- Cycle to prev completion item, or manually invoke completion
            prev = '<M-p>',
            -- Cycle to next completion item, or manually invoke completion
            next = '<M-n>',
            dismiss = '<M-e>',
        },
    }
  })
end
