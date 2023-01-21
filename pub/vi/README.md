# Features

- Syntax highlighting

- Auto-completion

- Auto-formatting

- Auto-refactoring

- Jump to definition

- Compiler and linter suggestions

- Hoogle docs

- Beginner friendly

# Installation

## Requirements

[Nix](https://nixos.org/) package manager should be installed.

## Minimal

```nix
./nix/install.sh mini
```

## Full

```nix
./nix/install.sh maxi
```

# Usage

```bash
vi .
```

# Some commands

It should support all the most standard Vim commands and shortcuts. Nevertheless, this bundle includes a lot of plugins and configs which might add new commands or override some standard bindings. If you already completed **vimtutor** and know basics, you almost ready to use Vim as your main text editor. Here I'll put some commands which probably have own equivalens in most of other IDEs. Most of the given commands should be executed in **normal** mode.

| Command | Description |
|:--------|:------------|
| **Esc** | Enter normal mode and remove highlighting of search matches. |
| **:BG** | Toggle dark/light color theme. |
| **:w** | Apply code formatter and save the file. |
| **Ctrl-f** | Fuzzy finder of files in current project. |
| **Ctrl-o** | Jump to previous opened buffer (similar to browser tab history movements). |
| **Ctrl-Shift-i** | Jump to next opened buffer (similar to browser tab history movements). |
| **Ctrl-p** | If current buffer is markdown document - render HTML and show it in Tor Browser. If browser already have opened markdown preview tab - refresh content.
| **:Ack HelloWorld** | Global search of text in files of current project. |
| **:e ./src/Foo.hs** | Edit file in current buffer. |
| **:sp ./src/Foo.hs** | Split screen horizontally and edit file in new buffer. |
| **:vsp ./src/Foo.hs** | Split screen vertically and edit file in new buffer. |
| **:sp\|term** | Split screen horizontally and open new terminal session. |
| **:vsp\|term** | Split screen vertically and open new terminal session. |
| **Esc** | Switch from **terminal** insert mode to normal (to scroll, copy text etc). To switch back to **terminal** insert mode just press **i**. |
| **Ctrl-ww** | Focus on next split window clockwise. |
| **Ctrl-wh** | Focus on next split window in given direction (left). |
| **Ctrl-wj** | Focus on next split window in given direction (down). |
| **Ctrl-wk** | Focus on next split window in given direction (up). |
| **Ctrl-wl** | Focus on next split window in given direction (right). |
| **Space-h** | Focus on first tab. |
| **Space-j** | Focus on prev tab. |
| **Space-k** | Focus on next tab. |
| **Space-l** | Focus on last tab. |
| **Space-t** | Open new tab. |
| **Space-e** | Open new tab with terminal in insert mode. |
| **Space-x** | Close current tab. |
| **:tabonly** | Close all tabs except current. |
| **gd** | Jump to expression definition. |
| **Shift-k** | Show expression type and documentation. |
| **to** | Apply refactoring according one hint at cursor position. |
| **ta** | Apply all refactoring suggestions in current buffer. |

<br>
<p align="center">
  <tt>
    Proudly Made by
    <a href="https://functora.github.io/" target="_blank">Functora</a>
  </tt>
</p>
