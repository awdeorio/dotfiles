# AGENTS.md

Guidance for coding agents working in `~/.hammerspoon`.  Read `README.md` first for the shortcuts and the design principles.  Check every new or changed shortcut against all of the principles before proposing it.

## Layout

- `init.lua`: pure functions on `{x, y, w, h}` tables, with thin glue that applies them.  Keep logic out of the glue.
- `test_init.lua`: unit tests for the window placement logic.  They need only `lua`, not Hammerspoon.  The file loads `init.lua` from `local function overlaps` up to `local function snap(dir)`, so keep pure functions in that range.  Add a test for every behavior change.

## Build and test

```console
$ luac -p init.lua
$ lua test_init.lua
$ hs -t 5 -c 'hs.reload()'
```

Always pass `-t` to `hs`; without a timeout it can hang.  `hs -c 'hs.reload()'` can hang even with `-t`, because the reload tears down the message port mid-reply.  The reload still takes effect, so run it in the background or kill it, then confirm with `hs -t 5 -c` on a query such as `hs.hotkey.getHotkeys()`.  Hammerspoon needs Accessibility permission and a restart after it is granted.  Do not move the user's windows without asking.
