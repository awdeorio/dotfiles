# awdeorio's Hammerspoon config

My personal [Hammerspoon](https://www.hammerspoon.org) config: keyboard shortcuts for moving and resizing the focused window.

| Shortcut      | Effect                                                    |
|---------------|-----------------------------------------------------------|
| `C-s-<arrow>` | Snap to nearest window edge or screen edge                |
| `C-s-c`       | Cycle centering: center->original                         |
| `C-s-n`       | Move to next display                                      |
| `C-s-t`       | Cycle height: fill->full->fit->original                   |
| `C-s-w`       | Cycle width: fill->full->fit->original                    |
| `C-s-m`       | Cycle width: 2x->4x->...->full->original                  |
| `C-s-l`       | Cycle width: left half->center half->right half->original |

Shortcut notation: `C` is ctrl, `M` is command (Meta), and `s` is option (Super).

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
2. **Press again to keep going.**  A snap moves on to the next edge, and a cycle steps to its next state.
3. **Cycle back to undo.**  A cycle's last step restores the original frame, so pressing until the window comes back undoes it.
4. **Comfortable, clash-free keys.**  The left hand holds `C-s-` and the right hand stays near the arrow keys.  `C-s-` is unbound in my Emacs, where option is Super; `C-M-` would shadow Emacs commands.  Also taken: `C-s-SPC` switches input sources, and `C-M-v` belongs to Maccy.
