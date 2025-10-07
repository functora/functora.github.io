local ok, avante_lib = pcall(require, "avante_lib")
if ok then
	avante_lib.load()
	require("avante").setup({ provider = "gemini-cli" })
end

local ok, codecompanion = pcall(require, "codecompanion")
if ok then
	codecompanion.setup({
		strategies = {
			chat = {
				adapter = "gemini_cli",
				opts = {
					completion_provider = "coc",
				},
			},
			inline = {
				adapter = "gemini_cli",
			},
			cmd = {
				adapter = "gemini_cli",
			},
		},
		adapters = {
			acp = {
				gemini_cli = function()
					return require("codecompanion.adapters").extend("gemini_cli", {
						defaults = {
							auth_method = "gemini-api-key",
							mcpServers = {},
							timeout = 20000, -- 20 seconds
						},
					})
				end,
			},
		},
	})
	vim.keymap.set("n", "<leader>zz", "<CMD>CodeCompanionChat Toggle<CR>", {
		desc = "ai: ch[a]t",
	})
	vim.keymap.set("n", "<leader>zx", "<CMD>CodeCompanionActions<CR>", {
		desc = "ai: action[s]",
	})
	vim.keymap.set("n", "<leader>zc", "<CMD>CodeCompanionCmd<CR>", {
		desc = "ai: cm[d]",
	})
	vim.keymap.set({ "n", "v" }, "<leader>zv", "<CMD>CodeCompanion<CR>", {
		desc = "ai: [f]ile write with diff",
	})
end

local ok, minuet = pcall(require, "minuet")
if ok then
	minuet.setup({
		provider = "gemini",
		provider_options = {
			gemini = {
				model = "gemini-2.5-flash-lite",
			},
		},
		virtualtext = {
			auto_trigger_ft = {},
			keymap = {
				-- accept whole completion
				accept = "<M-y>",
				-- accept one line
				accept_line = "<M-l>",
				-- accept n lines (prompts for number)
				-- e.g. "A-z 2 CR" will accept 2 lines
				accept_n_lines = "<M-z>",
				-- Cycle to prev completion item, or manually invoke completion
				prev = "<M-p>",
				-- Cycle to next completion item, or manually invoke completion
				next = "<M-n>",
				dismiss = "<M-e>",
			},
		},
	})
end
