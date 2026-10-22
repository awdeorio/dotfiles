-- Unit tests for the pure functions in init.lua.  Run: lua test_init.lua
local src = io.open(os.getenv("HOME") .. "/.hammerspoon/init.lua"):read("a")
local a = src:find("local function overlaps", 1, true)
local b = src:find("local function snap(dir)", 1, true)
local M = load(src:sub(a, b - 1) .. [[
return {snapFrame = snapFrame, advance = advance, centerSteps = centerSteps,
  heightSteps = heightSteps, widthSteps = widthSteps, doubleSteps = doubleSteps, halvesSteps = halvesSteps,
  moveFrame = moveFrame}
]])()
local snapFrame = M.snapFrame

local function R(x, y, w, h) return {x = x, y = y, w = w, h = h} end
local S = R(0, 0, 1000, 800)
local fails = 0
local function check(name, dir, f, others, want)
  local got = snapFrame(dir, R(f.x, f.y, f.w, f.h), S, others)
  local ok = got.x == want.x and got.y == want.y
  if not ok then fails = fails + 1 end
  print((ok and "ok   " or "FAIL ") .. name .. string.format(
    ": got (%d,%d) want (%d,%d)", got.x, got.y, want.x, want.y))
end

-- User's case: A on top of B, both at the right edge.  A's right edge
-- should land on B's left edge.
local B = R(500, 0, 500, 600)
check("left over B", "left", R(700, 100, 300, 300), {B}, R(200, 100, 0, 0))
-- Mirror cases.
local B2 = R(0, 0, 500, 600)
check("right over B", "right", R(0, 100, 300, 300), {B2}, R(500, 100, 0, 0))
local B3 = R(0, 400, 600, 400)
check("up over B", "up", R(100, 500, 300, 300), {B3}, R(100, 100, 0, 0))
local B4 = R(0, 0, 600, 400)
check("down over B", "down", R(100, 0, 300, 300), {B4}, R(100, 400, 0, 0))

-- Neighbor in the path: abut it, then hop past it, then the screen edge.
local C = R(100, 0, 200, 600)
check("abut C", "left", R(700, 100, 300, 300), {C}, R(300, 100, 0, 0))
check("no room past C: edge", "left", R(300, 100, 300, 300), {C},
  R(0, 100, 0, 0))
local C2 = R(400, 0, 200, 600)
check("hop past C2", "left", R(600, 100, 300, 300), {C2}, R(100, 100, 0, 0))
-- Closest edge wins, even if the result overlaps a window.
local D = R(250, 0, 100, 600)
check("closest edge D, over C2", "left", R(600, 100, 200, 300), {C2, D},
  R(350, 100, 0, 0))
check("gap too small: closest edge", "left", R(600, 100, 300, 300),
  {C2, R(0, 0, 150, 600)}, R(150, 100, 0, 0))
-- User's case: B and C snapped together, A touching C.  A covers C and stops
-- on the B|C edge, not past B.
local Bp, Cp = R(100, 0, 300, 600), R(400, 0, 200, 600)
check("stop on B|C edge", "left", R(600, 100, 300, 300), {Bp, Cp},
  R(400, 100, 0, 0))
check("stop on B|C edge, right", "right", R(100, 100, 300, 300),
  {R(400, 0, 200, 600), R(600, 0, 300, 600)}, R(300, 100, 0, 0))
-- Over B with a neighbor E to the left of B: land between them.
local E = R(0, 0, 200, 600)
check("between E and B", "left", R(700, 100, 300, 300), {B, E},
  R(200, 100, 0, 0))
check("no room between: closest edge", "left", R(700, 100, 300, 300),
  {B, R(0, 0, 300, 600)}, R(300, 100, 0, 0))
-- No overlap across the motion: ignored.
check("ignore B above", "left", R(700, 700, 300, 100), {R(500, 0, 500, 600)},
  R(0, 700, 0, 0))
-- Nothing around: screen edge; already there: unchanged.
check("screen edge", "right", R(100, 100, 300, 300), {}, R(700, 100, 0, 0))
check("already at edge", "right", R(700, 100, 300, 300), {}, R(700, 100, 0, 0))

-- Cycles: press repeatedly, feeding each result back in as the current frame.
local function press(build, start, n)
  local st, cur, out = nil, start, {}
  for _ = 1, n do
    local t
    t, st = M.advance(st, cur, build)
    if t then cur = t end
    table.insert(out, string.format("%d,%d,%d,%d", cur.x, cur.y, cur.w, cur.h))
  end
  return table.concat(out, " ")
end
local function checkCycle(name, got, want)
  local ok = got == want
  if not ok then fails = fails + 1 end
  print((ok and "ok   " or "FAIL ") .. name .. ": got " .. got
    .. (ok and "" or "  want " .. want))
end
local function bld(fn, others)
  return function(o) return fn(o, S, others) end
end
local W = R(100, 200, 200, 300)

checkCycle("center", press(bld(M.centerSteps), W, 3),
  "400,200,200,300 100,200,200,300 400,200,200,300")
checkCycle("center when centered: nothing",
  press(bld(M.centerSteps), R(400, 200, 200, 300), 2),
  "400,200,200,300 400,200,200,300")
checkCycle("double", press(bld(M.doubleSteps), W, 5),
  "0,200,400,300 0,200,800,300 0,200,1000,300 100,200,200,300 0,200,400,300")
checkCycle("halves", press(bld(M.halvesSteps), W, 5),
  "0,0,500,800 250,0,500,800 500,0,500,800 100,200,200,300 0,0,500,800")
checkCycle("halves from left half: skip it",
  press(bld(M.halvesSteps), R(0, 0, 500, 800), 3),
  "250,0,500,800 500,0,500,800 0,0,500,800")
-- Height: windows above (bottom at 100) and below (top at 600) overlap
-- horizontally; one off to the side does not count.
local above, below = R(0, 0, 400, 100), R(150, 600, 400, 200)
local side = R(700, 0, 300, 800)
checkCycle("height fill, full, original",
  press(bld(M.heightSteps, {above, below, side}), W, 4),
  "100,100,200,500 100,0,200,800 100,200,200,300 100,100,200,500")
checkCycle("height with no neighbors: full, original",
  press(bld(M.heightSteps, {}), W, 2), "100,0,200,800 100,200,200,300")
-- A on the bottom overlaps B on top.  Fill ignores B, so it equals full.
-- Fit shrinks A's top edge to B's bottom edge.
local Ab = R(100, 400, 300, 400)
checkCycle("height over B: full, fit, original",
  press(bld(M.heightSteps, {R(0, 0, 1000, 500)}), Ab, 4),
  "100,0,300,800 100,500,300,300 100,400,300,400 100,0,300,800")
-- B covers most of A: a 50px sliver is too short, so no fit step.
checkCycle("height sliver: full, original",
  press(bld(M.heightSteps, {R(0, 0, 1000, 750)}), Ab, 2),
  "100,0,300,800 100,400,300,400")
-- Width: windows to the left (right edge at 50) and right (left edge at 600)
-- overlap vertically; one above does not count.
local left, right = R(0, 100, 50, 300), R(600, 400, 300, 200)
local top = R(0, 0, 1000, 150)
checkCycle("width fill, full, original",
  press(bld(M.widthSteps, {left, right, top}), W, 4),
  "50,200,550,300 0,200,1000,300 100,200,200,300 50,200,550,300")
checkCycle("width with no neighbors: full, original",
  press(bld(M.widthSteps, {}), W, 2), "0,200,1000,300 100,200,200,300")
-- A on the right overlaps B on the left.  Fill ignores B, so it equals full.
-- Fit shrinks A's left edge to B's right edge.
local A, Bl = R(600, 200, 400, 300), R(0, 100, 700, 500)
checkCycle("width over B: full, fit, original",
  press(bld(M.widthSteps, {Bl}), A, 4),
  "0,200,1000,300 700,200,300,300 600,200,400,300 0,200,1000,300")
-- Fit grows one edge and shrinks the other: B overlaps on the left, a
-- neighbor sits clear to the right.
checkCycle("width fit grows and shrinks",
  press(bld(M.widthSteps, {R(0, 100, 400, 500), R(900, 0, 100, 800)}),
    R(300, 200, 300, 300), 4),
  "0,200,900,300 0,200,1000,300 400,200,500,300 300,200,300,300")
-- B covers A entirely: no gap, so no fit step.
checkCycle("width covered by B: full, original",
  press(bld(M.widthSteps, {R(0, 0, 1000, 800)}), A, 2),
  "0,200,1000,300 600,200,400,300")
-- B covers most of A: a 50px sliver is too narrow, so no fit step.  A
-- 200px gap is just enough.
checkCycle("width sliver: full, original",
  press(bld(M.widthSteps, {R(0, 0, 950, 800)}), A, 2),
  "0,200,1000,300 600,200,400,300")
checkCycle("width gap at minimum: fit",
  press(bld(M.widthSteps, {R(0, 0, 800, 800)}), A, 3),
  "0,200,1000,300 800,200,200,300 600,200,400,300")
-- Something else moves the window mid-cycle: start over from there.
local st, t = nil, nil
t, st = M.advance(st, W, bld(M.doubleSteps))
local moved = R(300, 200, 400, 300)
t, st = M.advance(st, moved, bld(M.doubleSteps))
checkCycle("restart after outside move",
  string.format("%d,%d,%d,%d", t.x, t.y, t.w, t.h), "100,200,800,300")

-- Next display: a small screen S and a big screen to its right.
local big = R(1000, 0, 2000, 1000)
local function move(f, from, to)
  local n = M.moveFrame(f, from, to)
  return string.format("%d,%d,%d,%d", n.x, n.y, n.w, n.h)
end
checkCycle("next: flush right stays flush right",
  move(R(600, 200, 400, 300), S, big), "2600,287,400,300")
checkCycle("next: flush left and top stay flush",
  move(R(0, 0, 400, 300), S, big), "1000,0,400,300")
checkCycle("next: full height spans new screen",
  move(R(100, 0, 200, 800), S, big), "1300,0,200,1000")
checkCycle("next: full screen spans new screen",
  move(R(0, 0, 1000, 800), S, big), "1000,0,2000,1000")
checkCycle("next: floating keeps relative center",
  move(R(400, 200, 200, 300), S, big), "1900,287,200,300")
checkCycle("next: too wide shrinks with aspect ratio, stays flush right",
  move(R(1200, 100, 1800, 300), big, S), "0,117,1000,166")
checkCycle("next: full height, too wide shrinks width only",
  move(R(1200, 0, 1800, 1000), big, S), "0,0,1000,800")
checkCycle("next: within slop of edges counts as touching",
  move(R(10, 0, 975, 790), S, big), "1000,0,2000,1000")
checkCycle("next: past slop does not count",
  move(R(30, 30, 200, 300), S, big), "1160,75,200,300")

os.exit(fails == 0 and 0 or 1)
