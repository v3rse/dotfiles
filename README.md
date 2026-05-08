# Introduction
This are a store house for my new dotfile folder. I gutted the previous one because I stuffed that I copied and I truly didn't understand everything in there.

# Prerequisite
- `i3blocks`: 
  - [i3blocks-contrib scripts](https://github.com/vivien/i3blocks-contrib)

# Management
GNU `stow` is what I use  to symlink the files and folders:

## Steps
- Install `stow`
- Navigate to your home directory
`cd ~`

- Clone the repo:
`git clone http://www.github.com/v3rse/dotfiles`

- Enter the dotfile directory
`cd dotfiles`

- Install zsh settings to your home
`stow fish`

- Unistall zsh settings
`stow -D fish`

- Install zsh settings to root user
`sudo stow fish -t /root`

# Features
i3  - i3wm related configs (includes i3status)
x11 - contains all X11 related config:
	* urxvt - keyboard settings, fonts and color schemes for urxvt
	* xmodmap - for swapping keys mostly used in vim
vim - vim configuration, no bundles
wallpaper - wallpaper for the various color schemes
compton - settings for composite config for shadows etc.
firefox - contains some CSS for fixing dark themes. N/B:  `stow -t ~/.mozilla/firefox/<generated-code>.default/chrome firefox`. Make sure you create a `chrome` directory.
kitty - kitty configurations and themes
i3blocks - replacement for i3status
pi-skills - AI agent skills for pi and Claude Code (see below)

## pi-skills

Shared skills for both [pi](https://github.com/earendil-works/pi) and Claude Code.

### Install

```bash
cd ~/dotfiles
./pi-skills/install.sh
```

This stows the skills for pi and symlinks them into `~/.claude/skills/`.

### Structure

| Path | Purpose |
|---|---|
| `~/.pi/agent/skills/{blindspots,tech-catchup,wiki-builder}` | Pi skill symlinks (via stow) |
| `~/.claude/skills/{blindspots,tech-catchup,wiki-builder}` | Claude Code skill symlinks |
| `~/org/news/feeds/*.txt` | **Runtime** feed config (mutable, not in dotfiles) |
| `~/org/news/profile.md` | **Runtime** catchup profile (mutable, not in dotfiles) |

### How it works

- **Skill code** (`SKILL.md`, scripts) lives in this repo and is symlinked into both agents.
- **Mutable data** (feed lists, tastemakers, seen URLs, health status) lives in `~/org/news/` and is created/copied on first run by `init.sh`.
- On a new machine, `init.sh` copies default feed templates from the skill dir into `~/org/news/feeds/` if they don't exist yet.


# Notes
- Had to fix locale environment variables to make glyphs work in `urxvt`
- Had to use `xrender` instead of `glx` for compton
- Had to use put most startup stuff in `.xprofile` instead of `.xinitrc` because lightdm sources it(`.xprofile`).
- Had to stop aliasing vim to nvim. Rather I create symlink in my home `bin` for vim to nvim which comes at the end of my PATH. That's far cleaner.

## Emacs daemon + emacsclient -nw troubleshooting
If `emacsclient -nw` looks un-themed while normal `emacs -nw` is fine, check daemon env from inside Emacs:

```elisp
(list custom-enabled-themes
      (getenv "TERM")
      (getenv "COLORTERM")
      (display-color-cells)
      (frame-parameter nil 'background-mode))
```

A bad sign is `TERM=nil`/`COLORTERM=nil` in daemon frames.

### Fix used here
Import terminal env into systemd user manager before starting the daemon:

```bash
systemctl --user stop emacs
systemctl --user import-environment TERM COLORTERM TERMINFO TERMINFO_DIRS
dbus-update-activation-environment --systemd TERM COLORTERM TERMINFO TERMINFO_DIRS
systemctl --user start emacs
```

And set explicit env in `systemctl --user edit emacs`:

```ini
[Service]
Environment=TERM=xterm-256color
Environment=COLORTERM=truecolor
```

Then:

```bash
systemctl --user daemon-reload
systemctl --user restart emacs
```

### Clipboard note (emacsclient -nw vs emacs GUI)
GUI Emacs uses native GUI clipboard integration.
Terminal `emacsclient -nw` relies on terminal/daemon environment and external clipboard bridge tools.
In this setup `xclip-mode` is enabled; under Wayland this can be less reliable than wl-clipboard-based integration.
