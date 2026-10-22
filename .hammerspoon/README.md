# Hammerspoon window management

Keyboard shortcuts for moving and resizing the focused window.

| Shortcut                  | Effect                                     |
|---------------------------|--------------------------------------------|
| `ctrl + option + <arrow>` | Snap to nearest window edge or screen edge |
| `ctrl + option + c`       | Center horizontally                        |
| `ctrl + option + n`       | Move to next display                       |
| `ctrl + option + t`       | Cycle height: fill->full->fit->original    |
| `ctrl + option + w`       | Cycle width: fill->full->fit->original     |
| `ctrl + option + m`       | Double the width                           |
| `ctrl + option + l`       | Left half, center half, right half         |

Fill grows to the nearest windows.  Fit also shrinks to the nearest windows.

## Install

Install Hammerspoon.

```console
$ brew install --cask hammerspoon
```

Grant Accessibility permission in System Settings > Privacy & Security > Accessibility.  Restart Hammerspoon afterward; the permission does not take effect until it does.

Reload after editing `init.lua`, either from the Hammerspoon menu bar icon (Reload Config) or from a terminal.  An alert reading "Hammerspoon config loaded" confirms the reload.

```console
$ hs -c 'hs.reload()'
```

## Principles

1. **Maximize screen real estate.**  Windows snap to each other and to the screen edges, leaving no wasted gaps.
2. **Do what I mean.**  Repeated key presses keep going.
3. **Avoid clashing with other shortcuts.**  In my Emacs, command is Meta and option is Super, so ctrl+option is unbound, while ctrl+command would shadow `C-M-` commands.  ctrl+command+v belongs to Maccy, and ctrl+option+space switches input sources.
4. **Ergonomic.**  The left hand holds ctrl+option and the right hand stays near the arrow keys.
5. **Cycle.**  Repeated presses of a resize or center shortcut step through states, then return to the original.
