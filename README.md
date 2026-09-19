# Dotfiles

## Sway on Arch Linux

The Sway/Waybar configuration is a selective reconstruction of the previous
workstation setup, not a bulk import of its home directory. Other existing files
in this repository (Hyprland, Neovim, X11, Git, Alacritty) are independent and are
**not** installed by the instructions below.

### Packages

Use current official Arch packages; do not copy old executables:

```sh
sudo pacman -S --needed sway swaybg swaylock foot wofi waybar \
  grim slurp wf-recorder playerctl mate-polkit
```

On a new/outdated installation, perform a normal full Arch upgrade first rather
than refreshing package databases without upgrading. `mate-polkit` is a standalone
GTK agent; this does not require installing the MATE desktop.

### Install only Sway and Waybar

Clone this repository to `~/projects/dotfiles`. Before installing, inspect the
config and any changes to startup/exec/include directives. Do not run an editor
against this repo with unreviewed local config/plugin loading enabled.

Validate before changing active configuration:

```sh
sway --validate --config "$HOME/projects/dotfiles/.config/sway/config"
python -m json.tool "$HOME/projects/dotfiles/.config/waybar/config.jsonc" >/dev/null
```

Back up any existing `~/.config/sway/config` and `~/.config/waybar` first. For
example, move them to a unique directory under `~/.local/state/dotfiles-backups/`.
Do not overwrite a previous backup. Then, with those destinations absent:

```sh
mkdir -p "$HOME/.config/sway"
ln -s "$HOME/projects/dotfiles/.config/sway/config" "$HOME/.config/sway/config"
ln -s "$HOME/projects/dotfiles/.config/waybar" "$HOME/.config/waybar"
sway --validate --config "$HOME/.config/sway/config"
swaymsg reload
```

If Sway was started with an explicit `-c` pointing somewhere else, reload still
uses that explicit file; check `swaymsg -t get_version`. A normal launch without
`-c` discovers the user config. Do not restart/logout with unsaved work just to
change its path.

Polkit uses `exec`, so it starts at login, not on reload. If no agent is already
running in the current session, start it once:

```sh
pgrep -af '^/usr/lib/mate-polkit/polkit-mate-authentication-agent-1'
# Only when the check above finds none:
swaymsg 'exec /usr/lib/mate-polkit/polkit-mate-authentication-agent-1'
```

Waybar is launched by Sway's `bar` lifecycle instead of `exec_always`; reloads
should leave exactly one bar. Its config contains built-in modules only, with no
custom executables, click scripts or downloaded assets.

### Controls and appearance

`Super` is the logo/Windows key.

| Action | Shortcut |
| --- | --- |
| Foot terminal | Super+Enter |
| Application launcher (Wofi) | Super+D |
| Focus / move | Super+H/J/K/L or arrows; add Shift to move |
| Workspace / move to workspace | Super+1–0; add Shift to move |
| Direct workspace access | Numeric keypad 1–9; keypad 0 selects workspace **0**, as before |
| Resize mode | Super+R, then H/J/K/L or arrows; Enter/Escape to finish |
| Horizontal / vertical split | Super+B / Super+V |
| Stacking / tabbed / split layout | Super+S / Super+W / Super+E |
| Fullscreen | Super+F |
| Toggle floating | Super+Shift+Space |
| Scratchpad move / show | Super+Shift+Minus / Super+Minus |
| Close focused window | Super+Shift+Q |
| Reload | Super+Shift+C |
| Lock | Super+Shift+X |
| Exit confirmation | Super+Shift+E |
| Full screenshot | Print |
| Select screenshot region | Ctrl+Shift+P; Escape cancels |
| Start/stop region recording | Ctrl+Shift+O; Escape cancels region selection |

Screenshots use Grim's default destination (XDG Pictures when configured,
otherwise the working directory). Recordings go to `~/recording_TIMESTAMP.mp4`.
Stopping sends SIGINT to this user's `wf-recorder` processes, including one
started manually; the recorder finalizes the file. Recordings have no audio by
default. Captures and recordings happen only when their shortcuts are pressed.

Preserved preferences: click-to-focus, 10-pixel inner gaps, 5-pixel borders,
180 ms key repeat delay and 50 Hz repeat rate. Useful Firefox/Picture-in-Picture,
volume, screen-sharing, Battle.net and Zoom floating rules are included. Those
rules do not install or launch the named apps.

### Deliberately not restored

- Old `ezbar` binary and `exec_always` startup: replaced with packaged Waybar.
- Old absolute background-helper script and Ctrl+Shift+H binding.
- OBS/F8 launch binding: reinstall and configure OBS separately if needed.
- Old wallpaper path: use Arch's installed wallpaper, not an unreviewed asset.
- Stale `DP-3` output binding: let Sway detect the connected monitor's preferred mode.
- Unexpanded `@sysconfdir@` include: use `/etc/sway/config.d/*`, including Arch's
  systemd/D-Bus environment setup.
- Old Waybar media script, laptop-only modules, and keyboard-device access.
- Old autostart entries, systemd user services, credentials, shell/editor plugins
  and executable files. No dotfiles-wide installer or automatic plugin fetch.

Automatic idle lock/suspend behavior remains unset, as in the old active config;
the explicit lock shortcut is available. Audio volume bindings/modules should be
added after the fresh audio session manager/server has been configured. This
configuration does not install or reconfigure that separate stack.

Review future pulls before reloading: the active configuration is linked into
this checkout, so edits here affect the next reload/login.
