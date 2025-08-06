{pkgs}:
(pkgs.formats.json {}).generate "zed-user-settings" {
  auto_install_extensions = {
    nix = true;
    haskell = true;
  };
  features = {
    copilot = false;
  };
  telemetry = {
    metrics = false;
    diagnostics = false;
  };

  vim_mode = true;
  ui_font_size = 18;
  buffer_font_size = 18;
  soft_wrap = "editor_width";
  # buffer_font_family = "JetBrains Mono";
  # base_keymap = "JetBrains";
  autosave = "off";

  lsp = {
    nil = {
      binary = {
        path = pkgs.lib.getExe pkgs.nil;
        path_lookup = true;
      };
      initialization_options = {
        formatting.command = ["alejandra"];
      };
    };
    haskell-language-server = {
      binary.path_lookup = true;
    };
  };

  languages = {
    Nix = {
      language_servers = ["nil"];
      format_on_save = "on";
    };
    Haskell = {
      language_servers = ["haskell-language-server"];
      format_on_save = "on";
    };
  };
}
