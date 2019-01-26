pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
--------------------------------
------ the letter b-------------
-- global game jam 2019 --------
--------------------------------

local cam_x = 0
local cam_x_screen_limit = 16

local t = 0      -- current time
local p =              -- player
 { x = 64, y = 64, move_timer = 0,
   cycling = false, cycle_timer = 0,
   col = 8, mode = 1, point_right = true,
   carry_sprite="food_green"}
local bee_gravity = 0
local bee_speed = 3
local bee_jank_skip_rate = 1
local bee_jank_factor_y = 2
local bee_jank_factor_x = .5
local normal_gravity = 5

-- the y position of the floor
local floor_y = 128 - 8

-------------------------- util --

printh("overwritten", "b", true)
function log(text,val1,val2,val3,val4)
  if text == nil then text = "nil" end
  if val1 == nil then val1 = "nil" end
  if val2 == nil then val2 = "nil" end
  if val3 == nil then val3 = "nil" end
  if val4 == nil then val4 = "nil" end
  printh("["..t.."] "..text.."|"..val1.."|"..val2.."|"..val3.."|"..val4, "b")
end
function len(x,y)
 return sqrt(len2(x,y))
end

function len2(x,y)
 return x*x + y*y
end

function pow2(x)
 return x * x
end

function step(n, inc)
 return round(n/inc)*inc
end

function round(n)
 local mod = n % 1
 return mod > .5 and ceil(n) or flr(n)
end

function rnd_index(a)
 return flr(rnd(#a))+1
end

function cycle_arr(t, skips)
 if skips > #t then error"skip too large" end
 for i=1,skips do
  t[i],t[i+skips-1] = t[i+skips-1],t[i]
 end
end

function shuffle(t)
 if type(t) ~= "table" then error"shuffle only works on tables" end
 for i=#t,1,-1 do
  local j = rnd_index(i-1)
  t[i],t[j] = t[j],t[i]
 end
end

function for_xy(x1,x2, y1,y2, func)
 for x=x1,x2,sgn(x2-x1) do
  for y=y1,y2,sgn(y2-y1) do
   func(x,y)
  end
 end
end


--------------------------------
----------------------- setup --
cls(); rect(0,0,127,127,1)


--------------------------------
------------ callback: update --

_b = {
 { count=0, isdown=false, used=false,
   sym="�", col= 8 }, -- left
 { count=0, isdown=false, used=false,
   sym="�", col= 9 }, -- right
 { count=0, isdown=false, used=false,
   sym="�", col=11 }, -- up
 { count=0, isdown=false, used=false,
   sym="�", col=12 }, -- down
 { count=0, isdown=false, used=false,
   sym="�", col=14 }, -- o
 { count=0, isdown=false, used=false,
   sym="�", col=15 }  -- x
}

function b(i)
 return _b[i+1]
end

-- sprite data
_s = {
  bee        = {n=  1, w=1, h=1, cx=4, cy=4, r=4},
  bee_green  = {n=  2, w=1, h=1, cx=4, cy=4, r=4},
  bee_blue   = {n= 17, w=1, h=1, cx=4, cy=4, r=4},
  bee_pink   = {n= 18, w=1, h=1, cx=4, cy=4, r=4},
  food       = {n= 16, w=1, h=1, cx=4, cy=4, r=4},
  food_green = {n= 32, w=1, h=1, cx=4, cy=4, r=4},
  food_pink  = {n= 33, w=1, h=1, cx=4, cy=4, r=4},
  food_blue  = {n= 34, w=1, h=1, cx=4, cy=4, r=4},

  hive   =     {n=  3, w=2, h=2, cx=8, cy=8, r=8},
  cloud  =     {n=  5, w=2, h=1, cx=8, cy=8},
  cloud2 =     {n= 48, w=2, h=1, cx=8, cy=8},
  floor  =     {n=  7, w=2, h=1, cx=4, cy=4},
  speech =     {n=  8, w=2, h=2, cx=12,cy=16},
  speech =     {n=  7, w=2, h=1, cx=12,cy=16},

  honeycomb =  {n= 35, w=2, h=2, cx=8, cy=8, r=8},
  -- todo: tree.
}

-- gathers spr parameters
function s(name, x, y, flip_x, flip_y)
  local sd = _s[name]
  return sd.n, x-sd.cx, y-sd.cy, sd.w, sd.h, flip_x, flip_y
end

-- entity reaction: delete
function er_delete(entity, entities)
  del(entities, entity)
end

-- entity reaction: carry
function er_carry(entity, entities)
  if p.carry_sprite ~= nil then return end
  p.carry_sprite = entity.type
  del(entities, entity)
end

-- entity reaction: drop
function er_drop(entity, entities)
  if p.carry_sprite == nil then return end
  add(entities, {type=p.carry_sprite,x=p.x,y=p.y})
  p.carry_sprite = nil
end

-- entity reaction: drop
function er_consume_carry(entity, entities)
  if p.carry_sprite == nil then return end
  p.carry_sprite = nil
end

function update_buttons()
 for i=1,6 do
  local cur = _b[i]
  isdown = btn(i-1)
  cur.count = cur.count + 1
  if isdown != cur.isdown then
   cur.count = 0
   cur.used = false
  end
  cur.isdown = isdown
 end
end

function spaces(n)
 local space = ""
 for i=1,n do
  space = space .. " "
 end
 return space
end

function dump(name, v)
 _dump(name, v, 0)
end

-- todo: have this sort tables
function _dump(name, v, depth)
 local padding = spaces(depth*2)
 if depth > 10 then
  log(padding .. "depth limit reached")
  return
 end
 if type(v) == "table" then
  log(padding .. name .. " (table) len: " .. #v)
  for k2,v2 in pairs(v) do
   _dump(k2, v2, depth+1)
  end
 else
  if type(v) == "boolean" then
   v = v and "true" or "false"
  elseif v == nil then
   v = "nil"
 elseif type(v) == "function" then
   v = "function"
  end
  log(padding .. name .. " " .. v)
 end
end

function update_bee()
  p.y = p.y + bee_gravity
  if t % bee_jank_skip_rate ~= 0 then
    local angle = rnd(1)
    local fx = cos(angle) * bee_jank_factor_x
    local fy = sin(angle) * bee_jank_factor_y
    p.x = p.x + fx
    p.y = p.y + fy
  end
end

function apply_floors(entities)
  if p.y > floor_y then p.y = floor_y end
end

function control()
  -- left
  if b(0).isdown then
    p.x = p.x - bee_speed
    p.point_right = false
  end
  -- right
  if b(1).isdown then
    p.x = p.x + bee_speed
    p.point_right = true
  end
  -- up
  if b(2).isdown then p.y = p.y - bee_speed end
  -- down
  if b(3).isdown then p.y = p.y + bee_speed end
end

-- determines which entities the bee is touching
function check_overlap(entities)
  local bee_r = _s["bee"].r
  for i=1,#entities do
    local e = entities[i]
    e.is_touched = false
    local sprite = _s[e.type]
    local e_r = sprite.r
    if e_r ~= nil then
      local dx = e.x - p.x
      local dy = e.y - p.y
      if abs(dx) < 16 and abs(dy) < 16 then
        local d2 = len2(dx,dy)
        local d2_limit = pow2(bee_r + e_r)
        if d2 < d2_limit then
          e.is_touched = true
          log("colliding", e.type, bee_r, e_r)
          log("colliding", dx, dy)
          log("colliding", d2, d2_limit)
          log("colliding", e.x, e.y, p.x, p.y)
        end
      end
    end
  end
end

-- reacts to the bee touching entities
function apply_overlap(entities)
  _s.bee.n = 1
  for i=1,#entities do
    local e = entities[i]
    if e.is_touched then
      _s.bee.n = 2
      local reactions = e.reactions
      if reactions ~= nil then
        log("#reactions", #reactions)
        for reaction in all(reactions) do
          reaction(e, entities)
        end
      end
    end
  end
end

function update_camera()
  cam_x_right_limit = p.x - cam_x_screen_limit
  cam_x_left_limit = p.x - (128 - cam_x_screen_limit)
  log("bee", cam_x_right_limit, cam_x_left_limit, p.x)
  if cam_x > cam_x_right_limit then cam_x = cam_x_right_limit end
  if cam_x < cam_x_left_limit then cam_x = cam_x_left_limit end
end

--------------------------------
-------------- callback: draw --

-- gets the (x,y) offset for rendering a screen worth of map (16,16)
function get_map_offset(map_number)
  local row = flr(map_number / 8)
  local column = (map_number - 1) % 8
  return column*16, row*16
end

-- gets the (sx,sy) offset for rendering a screen worth of
-- map a certain distance from home
-- 0 is home
-- 1 is "one screen right of home"
-- -1 is "one screen left of home"
function get_screen_offset(distance_from_home)
  local sx = 128 * distance_from_home
  local sy = 0
  return sx,sy
end

-- draws one map on the screen, offset appropriately
-- distance_from_home: 0 = origin; -1 = one screen left; +1 = one screen right
-- map_number: 1-indexed map id, starting from top left
function draw_screen(map_number, distance_from_home)
  map_x, map_y = get_map_offset(map_number)
  sx, sy = get_screen_offset(distance_from_home)
  map(map_x,map_y, sx,sy, 16,16)
end

function get_screen(sx)
  return flr(sx / 128)
end

local home_maps = {1}
local available_maps = {2,3,4,5,6,7,8}
function pick_map()
  local i = rnd_index(available_maps)
  log("map", i, #available_maps)
  return available_maps[rnd_index(available_maps)]
end

function draw_entities(entities)
  for i=1,#entities do
    local e = entities[i]
    spr(s(e.type, e.x, e.y))
  end
end


-------------------------------
-- utilities
-------------------------------

_mon_ordered = {}
_mon = {}

function monf(what, on_off, scale, col)
 mon(what, on_off, scale*12, col)
end

function mon(what, on_off, scale, col)
 cur = _mon[what] or {}
 cur.what = what
 cur.on_off = on_off
 cur.scale = scale
 cur.col = on_off and col or 5

 if not _mon[what] then
  add(_mon_ordered, cur)
 end

 _mon[what] = cur
end

function mon_draw(sx,sy)
 rectfill(sx,sy-1,sx+8*#_mon_ordered+1,sy+8,7)
 for k,v in pairs(_mon_ordered) do
  color(v.col)
  x = sx + k*8 - 6
  print(v.what, x, sy)
  if v.scale > 0 then
   line(x,sy+6,x+min(6,v.scale),sy+6)
  end
  if v.scale > 6 then
   line(x,sy+7,x+mid(0,6,v.scale-6),sy+7)
  end
 end
end

--------------------------------
------------- mode: overworld --

local home_map = 1
local map_list_right = {pick_map(),pick_map(),pick_map(),pick_map(),pick_map(),pick_map()}
local map_list_left = {pick_map(),pick_map(),pick_map(),pick_map(),pick_map()}

function _update_overworld()
  t = (t + 1) % 32767
  update_buttons()
  update_bee()
  control()
  apply_floors(overworld_entities)
  check_overlap(overworld_entities)

  apply_overlap(overworld_entities)

  update_camera()

  log("stats","mem",stat(0),"cpu",stat(1))
end

function _draw_overworld()
  cls"12"
  camera(cam_x,0)
  local screen = get_screen(cam_x)

  -- home
  draw_screen(home_map, 0)

  -- left
  for i=1,#map_list_left do
    local map = map_list_left[i]
    draw_screen(map, -i)
  end

  -- right
  for i=1,#map_list_right do
    local map = map_list_right[i]
    draw_screen(map, i)
  end

  -- entities
  draw_entities(overworld_entities)

  -- bee carry
  if p.carry_sprite ~= nil then
    local offset_y = _s.bee.cy + _s[p.carry_sprite].cy
    spr(s(p.carry_sprite, p.x, p.y + offset_y))
  end

  -- bee
  spr(s("bee", p.x, p.y, not p.point_right))
end

function go_to_overworld()
  _update = _update_overworld
  _draw = _draw_overworld
end

--------------------------------
------------------ mode: hive --

function _update_hive()
  t = (t + 1) % 32767
  update_buttons()
  update_bee()
  control()

  apply_floors(hive_entities)
  check_overlap(hive_entities)
  apply_overlap(hive_entities)

  update_camera()

  log("stats","mem",stat(0),"cpu",stat(1))
end

function _draw_hive()
  cls"15"
  -- map(0,64-16, 0,0, 16,16)

  -- entities
  draw_entities(hive_entities)

  -- bee carry
  if p.carry_sprite ~= nil then
    local offset_y = _s.bee.cy + _s[p.carry_sprite].cy
    spr(s(p.carry_sprite, p.x, p.y + offset_y))
  end
  -- bee
  spr(s("bee", p.x, p.y, not p.point_right))
end

function go_to_hive()
  _update = _update_hive
  _draw = _draw_hive
end

-- go_to_overworld()
go_to_hive()

-- prep entities
function _init()
  overworld_entities = {
    {type="hive", x=64, y=76,reactions={go_to_hive}},
    {type="food", x=136,y=floor_y,reactions={er_carry}}
  }

  hive_entities = {
    {type="hive",      x=64, y=128-_s.hive.cy,reactions={go_to_overworld}},
    {type="honeycomb", x=96,y=96,reactions={er_consume_carry}}
  }
end

__gfx__
00000000000000000000000000000002200000000000000000077700bbbbbbbb000000000000000000000bbbbbbbbbbbbbbbbbbbbbb000000000000000000000
0000000000770005006600060000022a922000000000000000777770b3b3b3b30000077777700000000bbbbbbbbbbbbbbbbbbbbbbbbbb0000000000000000000
007007000766705006556060000029a9a9a20000000000770777777033333333000777777777700000bb333333333333333333333333bb000000000000000000
0007700000766aa000655bb00002a9a9a9a9200000770777777777703333333300777777777777000bb33333333333333333333333333bb00000000000000000
000770000a474a1a0b363b1b002a9a9a9a9a92000777777777777777333333330777777777777770bb3333333333333333333333333333bb0000000000000000
007007004a4a4aaa3b3b3bbb02a9a9a9a9a9a9206776766767766766333333337777777777777777b33e333333333333393333333333333b0000000000000000
00000000049494000313130002a9a9a9a9a9a920666666666666666033333333777777777777777733e7e3333333333397933333333383330000000000000000
0000000000505000005050002a9a9a9a9a9a9a920660600606600600333333337777777777777777333e33333333333339333333333878330000000000000000
000aa000000000000000000029a9a9a9a9a9a9a20aa0000000000000000000007777777777777776333333333333333333333333333383330000000000000000
00aa9a0000660006006600062a9a9a922a9a9a920a8a0000020220200000000007777777777777603333333333833333333333e3333333330000000000000000
0aa979a0065560600655606029a9a92442a9a9a20aaa077020212202000000000677777777777760333333333878333333333e7e333333330000000000000000
0aaa9aa000655cc000655ee029a9a2444429a9a200887667002112000000000000667777777666003333333333833333333333e3333333330000000000000000
04aaaa400c161c1c0e868e1e2a9a9244442a9a92009a667000211200000000000000666777600000333333333333333333333333333333330000000000000000
004aa4001c1c1ccc8e8e8eee02a9a2444429a9200088770000282800000000000000000677000000333333333333333333333333333333330000000000000000
0004400001212100084848000029a2444429a200409aa00002022020000000000000000067000000333333333333333333333333339333330000000000000000
00000000005050000050500000022222222220000444000020000002000000000000000006000000333338333333333333333333397933330000000000000000
000bb000000cc000000ee00000000002200000000000522224440000000000000000000000000000333387833333333333333333339333330000000000000000
00bb1b0000cc2c0000ee4e000000022992200000000052222444000000000000000000000000000033333833333333333e333333333333330000000000000000
0bb161b00cc2f2c00ee4f4e0000229999992200000005222244400000000000000000000000000003333333333333333e7e33333333333330000000000000000
0bbb1bb00ccc2cc00eee4ee00229999229999220000052222444000000000000000000000000000033333333333333333e333333333333330000000000000000
03bbbb3001cccc1008eeee8029999224422999920000522224440000000000000000000000000000333333333333333333333333333333330000000000000000
003bb300001cc100008ee80029922444444229920000555244440000000000000000000000000000333333333333333333333333333333330000000000000000
0003300000011000000880002992444444442992000052222444000000000000000000000000000033333e333333333333333383333333330000000000000000
000000000000000000000000299244444444299200005222244400000000000000000000000000005333e7e33333393333333878333333330000000000000000
0007700000000000000000002992444444442992000052224444000000000000000000000000000035333e333333979333333383333333350000000000000000
00777777000000000000000029922444444229920000555244440000000000000000000000000000033533333333393333333333333353500000000000000000
00777777777000000000000029999224422999920000522224440000000000000000000000000000005335333333333333333333333335000000000000000000
07777777777777000000000052299992299992250000522224440000000000000000000000000000000353533333333333333333335330000000000000000000
07777777777777770000000005522999999225500005555244444000000000000000000000000000000005533333333333333333353500000000000000000000
67677676676776770000000000055229922550000055222244444400000000000000000000000000000000053333333333333333355000000000000000000000
66666666666666660000000000000552255000000552222222444440000000000000000000000000000000005353535353535353500000000000000000000000
06066060060660600000000000000005500000005555555522444444000000000000000000000000000000000535553535355530000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000032420000000000003242000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000033430000000000003343000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000032420000000000003242000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000033430000000000003343000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
71717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717171
__map__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000017
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000017
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000a0b0c0d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000001a1b1c1d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000002a2b2c2d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000003a3b3c3d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000252600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000252600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000252600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000252600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000252600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000353600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707
__sfx__
000200000613006140061400614006130071300712006120051100510005100051000510006100061200613005140051300511006100061200613006140061400612006110061000610006100051200413005140
010e00000d2201920016200141001722017100161001510016220241002410023100172001b22017100151001670023200247000000000000000000000000000232002120023200212001b200192001b20019200
010e000000000000000d6330d6000d600000000d6330d6000d600000000d633000000d600000000d633000000d600000000d633000000d600000000d633000000d600000000d633000000d600000000d63300000
000e00000d2210d2000d2200d2000d2200d2000d2200120014221000001422001200142200120014220012000c221002000c220002000c220002000c220002000f221032000f2200f2000f220032000f22003200
000e00003d6000000400000000003d6150000000000000003d6000000400000000003d6150000000000000003d6000000400000000003d6150000000000000003d6000000400000000003d615000000000000000
000e00000d2201920016200141001722017100161001510016220241002410023100172001b22017100151001670023200247000000000000000000000000000232102121023210212101b210192101b21019210
010800000664400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
01 01020304
02 05020304

