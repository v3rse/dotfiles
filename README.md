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

# Bugs
- [ ] Terminals flash original theme seconds before loading current.
- [ ] Prompt bugs out when command gets to end of line
- [x] Screen tears after addition of compton.

# Notes
- Had to [create](https://wiki.ubuntu.com/CustomXSession) symlink of  `.xsession` to `.xinitrc` for Ubuntu's `gdm`
- Had to fix locale environment variables to make glyphs work in `urxvt`
- Had to use `xrender` instead of `glx` for compton
