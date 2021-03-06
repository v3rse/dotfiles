# Introduction
This are a store house for my new dotfile folder. I gutted the previous one because I stuffed that I copied and I truly didn't understand everything in there.

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
`stow zsh`

- Unistall zsh settings
`stow -D zsh`

- Install zsh settings to root user
`sudo stow zsh -t /root`

# Features
i3  - i3wm related configs (includes i3status)
zsh - z-shell settings, aliases and prompts
urxvt - keyboard settings, fonts and color schemes for urxvt
xmodmap - for swapping keys mostly used in vim
vim - vim configuration, no bundles
wallpaper - wallpaper for the various color schemes
compton - settings for composite config for shadows etc.
vscode - settings for my vscode environment. extension can be install using `install.sh` in folder. Extension list can be created using `code --list-extensions > extensions-list`
firefox - contains some CSS for fixing dark themes. N/B:  `stow -t ~/.mozilla/firefox/<generated-code>.default/chrome firefox`. Make sure you create a `chrome` directory.

# Bugs
- [ ] Terminals flash original theme seconds before loading current.
- [ ] Prompt bugs out when command gets to end of line
- [x] Screen tears after addition of compton.
- [ ] Lock screen/PAM issue

# Notes
- Had to fix locale environment variables to make glyphs work in `urxvt`
- Had to use `xrender` instead of `glx` for compton
- Had to build `13-gaps` from source to get `set_from_resource` feature.(without this i3 colors go black)
- Had to use put most startup stuff in `.xprofile` instead of `.xinitrc` because lightdm sources it(`.xprofile`).
