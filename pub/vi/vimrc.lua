require("gp").setup({
  providers = {
    openai = {
      endpoint = "http://localhost:8080/v1/chat/completions"
    }
  }
})
