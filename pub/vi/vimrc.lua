local ok, avante_lib = pcall(require, "avante_lib")
if ok then
  avante_lib.load()
  require("avante").setup({provider = "gemini"})
end
