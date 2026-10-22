require("hs.ipc")  -- lets the `hs` command-line tool talk to Hammerspoon
-- Window management shortcuts.  See README.md.
hs.window.animationDuration = 0

-- Screen whose full frame contains the window's center.
local function screenOf(win)
  local c = win:frame().center
  for _, s in ipairs(hs.screen.allScreens()) do
    local r = s:fullFrame()
    if c.x >= r.x and c.x < r.x + r.w and c.y >= r.y and c.y < r.y + r.h then
      return s
    end
  end
  return win:screen()
end

-- Run fn(frame, visibleFrame) -> new frame; apply only if it changed.
local function act(fn)
  return function()
    local win = hs.window.focusedWindow()
    if not win then return end
    local f = win:frame()
    local n = fn(f:copy(), screenOf(win):frame(), win)
    if n and not n:equals(f) then win:setFrame(n) end
  end
end

local function overlaps(a0, a1, b0, b1) return a0 < b1 and b0 < a1 end

-- Cycles.  Frames here are plain {x, y, w, h} tables.
local function rect(f) return {x = f.x, y = f.y, w = f.w, h = f.h} end
local function same(a, b)
  return a.x == b.x and a.y == b.y and a.w == b.w and a.h == b.h
end

-- Width w around the current center, clamped to the screen.
local function widthAround(f, s, w)
  local n = rect(f)
  n.w = math.min(w, s.w)
  local x = math.floor(f.x + f.w / 2 - n.w / 2)
  n.x = math.max(s.x, math.min(x, s.x + s.w - n.w))
  return n
end

local function centerSteps(o, s)
  local n = rect(o)
  n.x = s.x + math.floor((s.w - o.w) / 2)
  return {n}
end

-- Grow the top edge up and the bottom edge down until each touches a window
-- that overlaps horizontally, or the screen edge.
local function fillHeight(f, s, others)
  local top, bottom = s.y, s.y + s.h
  for _, o in ipairs(others) do
    if overlaps(f.x, f.x + f.w, o.x, o.x + o.w) then
      if o.y + o.h <= f.y then top = math.max(top, o.y + o.h) end
      if o.y >= f.y + f.h then bottom = math.min(bottom, o.y) end
    end
  end
  return {x = f.x, y = top, w = f.w, h = bottom - top}
end

-- Narrowest gap worth fitting into.
local minFit = 200

-- Like fillHeight, but windows that overlap f count too: the side of f's
-- center that a window's center is on decides which edge it limits.  May
-- shrink f.  If the gap is shorter than minFit, return f unchanged.
local function fitHeight(f, s, others)
  local top, bottom = s.y, s.y + s.h
  local c = f.y + f.h / 2
  for _, o in ipairs(others) do
    if overlaps(f.x, f.x + f.w, o.x, o.x + o.w) then
      if o.y + o.h / 2 < c then top = math.max(top, o.y + o.h)
      else bottom = math.min(bottom, o.y) end
    end
  end
  if bottom - top < minFit then return rect(f) end
  return {x = f.x, y = top, w = f.w, h = bottom - top}
end

-- Fill, full height, then fit.  Fit equals fill when nothing overlaps, so
-- skip it then.
local function heightSteps(o, s, others)
  local fill, fit = fillHeight(o, s, others), fitHeight(o, s, others)
  local steps = {fill, {x = o.x, y = s.y, w = o.w, h = s.h}}
  if not same(fit, fill) then table.insert(steps, fit) end
  return steps
end

-- Grow the left edge left and the right edge right until each touches a
-- window that overlaps vertically, or the screen edge.
local function fillWidth(f, s, others)
  local left, right = s.x, s.x + s.w
  for _, o in ipairs(others) do
    if overlaps(f.y, f.y + f.h, o.y, o.y + o.h) then
      if o.x + o.w <= f.x then left = math.max(left, o.x + o.w) end
      if o.x >= f.x + f.w then right = math.min(right, o.x) end
    end
  end
  return {x = left, y = f.y, w = right - left, h = f.h}
end

-- Like fillWidth, but windows that overlap f count too: the side of f's
-- center that a window's center is on decides which edge it limits.  May
-- shrink f.  If the gap is narrower than minFit, return f unchanged.
local function fitWidth(f, s, others)
  local left, right = s.x, s.x + s.w
  local c = f.x + f.w / 2
  for _, o in ipairs(others) do
    if overlaps(f.y, f.y + f.h, o.y, o.y + o.h) then
      if o.x + o.w / 2 < c then left = math.max(left, o.x + o.w)
      else right = math.min(right, o.x) end
    end
  end
  if right - left < minFit then return rect(f) end
  return {x = left, y = f.y, w = right - left, h = f.h}
end

-- Fill, full width, then fit.  Fit equals fill when nothing overlaps, so
-- skip it then.
local function widthSteps(o, s, others)
  local fill, fit = fillWidth(o, s, others), fitWidth(o, s, others)
  local steps = {fill, {x = s.x, y = o.y, w = s.w, h = o.h}}
  if not same(fit, fill) then table.insert(steps, fit) end
  return steps
end

local function doubleSteps(o, s)
  local steps, w = {}, 2 * o.w
  while w < s.w do
    table.insert(steps, widthAround(o, s, w))
    w = 2 * w
  end
  table.insert(steps, widthAround(o, s, s.w))
  return steps
end

-- Full height, half width: left, center, then right.
local function halvesSteps(o, s)
  local w = math.floor(s.w / 2)
  return {
    {x = s.x, y = s.y, w = w, h = s.h},
    {x = s.x + math.floor((s.w - w) / 2), y = s.y, w = w, h = s.h},
    {x = s.x + s.w - w, y = s.y, w = w, h = s.h},
  }
end

-- One press of a cycle.  st is {orig, i, last} from the previous press, or
-- nil.  Start over unless the window is where the last press left it.  Skip
-- steps that change nothing, and end with the original frame.  Returns the
-- target frame, or nil to do nothing, and the state for the next press.
local function advance(st, cur, build)
  if not (st and same(cur, st.last)) then st = {orig = rect(cur), i = 0} end
  local seq = {}
  for _, n in ipairs(build(st.orig)) do
    if not same(n, st.orig) and not (#seq > 0 and same(n, seq[#seq])) then
      table.insert(seq, n)
    end
  end
  if #seq == 0 then return nil, nil end
  table.insert(seq, st.orig)
  local i = st.i + 1
  if i >= #seq then return seq[#seq], nil end
  st.i, st.last = i, seq[i]
  return seq[i], st
end

local function otherFrames(win)
  local others = {}
  for _, o in ipairs(hs.window.orderedWindows()) do
    if o:id() ~= win:id() and o:isStandard() then
      table.insert(others, o:frame())
    end
  end
  return others
end

-- Bind a cycle to a hotkey.  build(orig, screen, win) returns the steps.
local function cycle(build)
  local states = {}
  return function()
    local win = hs.window.focusedWindow()
    if not win then return end
    local id, s = win:id(), rect(screenOf(win):frame())
    local target, st = advance(states[id], rect(win:frame()), function(o)
      return build(o, s, win)
    end)
    if target then
      win:setFrame(hs.geometry.rect(target.x, target.y, target.w, target.h))
    end
    -- The app may adjust the frame (e.g. to a character grid), so record
    -- where the window actually landed.
    if st then st.last = rect(win:frame()) end
    states[id] = st
  end
end

-- Move in dir to the closest spot where one of the window's edges touches
-- the opposite edge of another window, else the screen edge.  Only windows
-- that overlap it across the direction of motion count.  The result may
-- overlap other windows.
local function snapFrame(dir, f, s, others)
  local horiz = dir == "left" or dir == "right"
  local p, len = horiz and "x" or "y", horiz and "w" or "h"
  local q, qlen = horiz and "y" or "x", horiz and "h" or "w"
  local sign = (dir == "right" or dir == "down") and 1 or -1
  local lo, hi = s[p], s[p] + s[len] - f[len]

  local inline = {}
  for _, o in ipairs(others) do
    if overlaps(f[q], f[q] + f[qlen], o[q], o[q] + o[qlen]) then
      table.insert(inline, o)
    end
  end

  -- Candidate positions: either screen edge, and our edges on o's edges.
  local cands = {lo, hi}
  for _, o in ipairs(inline) do
    table.insert(cands, o[p] + o[len])
    table.insert(cands, o[p] - f[len])
  end

  local stop
  for _, c in ipairs(cands) do
    if c >= lo and c <= hi and (c - f[p]) * sign > 0
        and (stop == nil or (c - stop) * sign < 0) then
      stop = c
    end
  end
  if stop then f[p] = stop end
  return f
end

-- Pixels from a screen edge that still count as touching it.  Apps that size
-- to a character grid, like Emacs and terminals, stop short of the edge.
local slop = 20

-- Whether [p, p + len) touches the low and high edges of [s0, s0 + slen).
local function touching(p, len, s0, slen)
  return p <= s0 + slop, p + len >= s0 + slen - slop
end

-- Map one axis from screen [s0, s0 + slen) to [n0, n0 + nlen) with new
-- length w.  Touching both edges spans the new screen, touching one stays
-- flush with it, and otherwise the center keeps its relative position.
local function mapAxis(p, len, w, s0, slen, n0, nlen)
  local lo, hi = touching(p, len, s0, slen)
  if lo and hi then return n0, nlen end
  if lo then return n0, w end
  if hi then return n0 + nlen - w, w end
  local c = n0 + (p + len / 2 - s0) / slen * nlen
  return math.max(n0, math.min(math.floor(c - w / 2), n0 + nlen - w)), w
end

-- Frame f on screen s moved to screen n, edge by edge.  Shrink only to fit,
-- keeping the aspect ratio.  An axis that spans the screen doesn't count.
local function moveFrame(f, s, n)
  local function fit(p, len, s0, slen, nlen)
    local lo, hi = touching(p, len, s0, slen)
    return (lo and hi) and 1 or nlen / len
  end
  local k = math.min(1, fit(f.x, f.w, s.x, s.w, n.w),
    fit(f.y, f.h, s.y, s.h, n.h))
  local x, w = mapAxis(f.x, f.w, math.floor(f.w * k), s.x, s.w, n.x, n.w)
  local y, h = mapAxis(f.y, f.h, math.floor(f.h * k), s.y, s.h, n.y, n.h)
  return {x = x, y = y, w = w, h = h}
end

local function snap(dir)
  return function(f, s, win)
    return snapFrame(dir, f, s, otherFrames(win))
  end
end

local function nextDisplay(f, s, win)
  local n = moveFrame(rect(f), rect(s), rect(screenOf(win):next():frame()))
  return hs.geometry.rect(n.x, n.y, n.w, n.h)
end

-- Key map.  ctrl+option is unused by Emacs, where option is Super.
local ca = {"ctrl", "alt"}
hs.hotkey.bind(ca, "left",  act(snap("left")))
hs.hotkey.bind(ca, "right", act(snap("right")))
hs.hotkey.bind(ca, "up",    act(snap("up")))
hs.hotkey.bind(ca, "down",  act(snap("down")))
hs.hotkey.bind(ca, "c",     cycle(centerSteps))
hs.hotkey.bind(ca, "n",     act(nextDisplay))
hs.hotkey.bind(ca, "t",     cycle(function(o, s, win)
  return heightSteps(o, s, otherFrames(win))
end))
hs.hotkey.bind(ca, "w",     cycle(function(o, s, win)
  return widthSteps(o, s, otherFrames(win))
end))
hs.hotkey.bind(ca, "m",     cycle(doubleSteps))
hs.hotkey.bind(ca, "l",     cycle(halvesSteps))

hs.alert.show("Hammerspoon config loaded")
