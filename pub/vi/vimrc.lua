require("gp").setup({
  providers = {
    openai = {
      endpoint = "http://localhost:8080/v1/chat/completions"
    }
  }
})

require("avante_lib").load()
require("avante").setup({
  provider = "gemini"
})
