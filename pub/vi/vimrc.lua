pcall(function()
	local ok, d = pcall(require, "dioxus")
	if ok and d and d.setup then
		d.setup({})
	end
end)
