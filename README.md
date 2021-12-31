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
x11 - contains all X11 related config:
	* urxvt - keyboard settings, fonts and color schemes for urxvt
	* xmodmap - for swapping keys mostly used in vim
vim - vim configuration, no bundles
wallpaper - wallpaper for the various color schemes
compton - settings for composite config for shadows etc.
firefox - contains some CSS for fixing dark themes. N/B:  `stow -t ~/.mozilla/firefox/<generated-code>.default/chrome firefox`. Make sure you create a `chrome` directory.


# Notes
- Had to fix locale environment variables to make glyphs work in `urxvt`
- Had to use `xrender` instead of `glx` for compton
- Had to use put most startup stuff in `.xprofile` instead of `.xinitrc` because lightdm sources it(`.xprofile`).
- Had to stop aliasing vim to nvim. Rather I create symlink in my home `bin` for vim to nvim which comes at the end of my PATH. That's far cleaner.
