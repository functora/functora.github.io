pcall(function()
  local ok, d = pcall(require, "dioxus")
  if ok and d and d.setup then d.setup({}) end
end)

local ok, avante_lib = pcall(require, "avante_lib")
if ok then
	avante_lib.load()
	require("avante").setup({ provider = "gemini-cli" })
end

local ok, codecompanion = pcall(require, "codecompanion")
if ok then
	local rust =
		"Read about the project in README.md. Your code must be in a strictly functional style while being highly efficient, avoid mutable variables (mut), avoid imperative statements like loops, for, while, and return, use functional style iterators instead, avoid redundant closures and redundant variables, use method and function chaining/compositions as much as possible, avoid the clone method, avoid redundant allocations, and follow the existing project code style. However, to achieve this, use only standard, simple Rust. Avoid redundant dependencies. Ensure that all functions, variables, types, and other identifiers in the code are meaningful and clear in their context, while still preferring shorter names, as in the original code. Do not write any comments in the code, as good code should always be clear on its own."
	codecompanion.setup({
		extensions = {
			spinner = {},
		},
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
		prompt_library = {
			rust = {
				strategy = "chat",
				prompts = { { role = "user", content = rust } },
				opts = { short_name = "rust", is_slash_cmd = true },
			},
		},
		adapters = {
			acp = {
				gemini_cli = function()
					return require("codecompanion.adapters").extend("gemini_cli", {
						defaults = {
							auth_method = "gemini-api-key",
							mcpServers = {},
							timeout = 50000,
						},
						commands = {
							default = {
								"gemini",
								"--experimental-acp",
								"-m",
								"gemini-2.5-flash-lite",
							},
							lite = {
								"gemini",
								"--experimental-acp",
								"-m",
								"gemini-2.5-flash-lite",
							},
							flash = {
								"gemini",
								"--experimental-acp",
								"-m",
								"gemini-2.5-flash",
							},
							pro = {
								"gemini",
								"--experimental-acp",
								"-m",
								"gemini-2.5-pro",
							},
						},
					})
				end,
			},
		},
	})
	vim.api.nvim_create_user_command("CodeCompanionRust", function(opts)
		local prev = opts.args
		local next = rust .. (prev and " " .. prev or "")
		vim.cmd("CodeCompanionChat " .. next)
	end, { nargs = "*", complete = "file" })
	vim.keymap.set({ "v", "n" }, "<leader>zz", "<CMD>CodeCompanionChat Toggle<CR>", {})
	vim.keymap.set({ "v", "n" }, "<leader>zx", "<CMD>CodeCompanionActions<CR>", {})
	vim.keymap.set({ "v", "n" }, "<leader>zc", ":CodeCompanionRust ", {})
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
