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
local floor_y = 128 - 32
local hive_wall_thickness = 8

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

function apply_hive_walls(entities)
  if p.y > 128-hive_wall_thickness then p.y = 128-hive_wall_thickness end
  if p.x > 128-hive_wall_thickness then p.x = 128-hive_wall_thickness end
  if p.y < hive_wall_thickness then p.y = hive_wall_thickness end
  if p.x < hive_wall_thickness then p.x = hive_wall_thickness end
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
  rectfill(cam_x,floor_y, 128+cam_x,128, 3)
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

  apply_hive_walls(hive_entities)
  check_overlap(hive_entities)
  apply_overlap(hive_entities)

  update_camera()

  log("stats","mem",stat(0),"cpu",stat(1))
end

function _draw_hive()
  cls"15"
  camera(cam_x,0)
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
  cam_x = 0
end

-- go_to_overworld()
go_to_hive()

-- prep entities
function _init()
  overworld_entities = {
    {type="hive", x=64, y=52,reactions={go_to_hive}},
    {type="food", x=136,y=floor_y,reactions={er_carry}}
  }

  hive_entities = {
    {type="hive",      x=64, y=128-_s.hive.cy,reactions={go_to_overworld}},
    {type="honeycomb", x=96,y=96,reactions={er_consume_carry}}
  }
end


__gfx__
00000000000000000000000000000002200000000000000000077700bbbbbbbb000000000000000000000bbbbbbbbbbbbbbbbbbbbbb000000000000aa0000000
0000000000770005006600060000022a922000000000000000777770b3b3b3b30000077777700000000bbbbbbbbbbbbbbbbbbbbbbbbbb00000a000a99a000a00
007007000766705006556060000029a9a9a20000000000770777777033333333000777777777700000bb333333333333333333333333bb000a9aaa9449aaa9a0
0007700000766aa000655bb00002a9a9a9a9200000770777777777703333333300777777777777000bb33333333333333333333333333bb000a994499449a900
000770000a474a1a0b363b1b002a9a9a9a9a92000777777777777777333333330777777777777770bb3333333333333333333333333333bb0a994999999499a0
007007004a4a4aaa3b3b3bbb02a9a9a9a9a9a9206776766767766766333333337777777777777777b33e333333333333393333333333333b0994000000004990
00000000049494000313130002a9a9a9a9a9a920666666666666666033333333777777777777777733e7e3333333333397933333333383330a490059005994a0
0000000000505000005050002a9a9a9a9a9a9a920660600606600600333333337777777777777777333e33333333333339333333333878330a490059005994a0
000aa000000000000000000029a9a9a9a9a9a9a20aa0000000000000000000007777777777777776333333333333333333333333333383330a499999999994a0
00aa9a0000660006006600062a9a9a922a9a9a920a8a0000020220200000000007777777777777603333333333833333333333e3333333330a499999909994a0
0aa979a0065560600655606029a9a92442a9a9a20aaa077020212202000000000677777777777760333333333878333333333e7e333333330994990009994990
0aaa9aa000655cc000655ee029a9a2444429a9a200887667002112000000000000667777777666003333333333833333333333e3333333330a994999999499a0
04aaaa400c161c1c0e868e1e2a9a9244442a9a92009a6670002112000007700000006667776000003333333333333333333333333333333300a9944994499a00
004aa4001c1c1ccc8e8e8eee02a9a2444429a9200088770000282800007557000000000677000000333333333333333333333333333333330a9aaa9449aaa9a0
0004400001212100084848000029a2444429a200409aa000020220200067760000000000670000003333333333333333333333333393333300a000a99a000a00
00000000005050000050500000022222222220000444000020000002000660000000000006000000333338333333333333333333397933330000000aa0000000
000bb000000cc000000ee0000000000220000000000052222444000000588800000000000000000033338783333333333333333333933333000000f000000000
00bb1b0000cc2c0000ee4e000000022992200000000052222444000000588888888000000000000033333833333333333e3333333333333300dd0ffff0000000
0bb161b00cc2f2c00ee4f4e0000229999992200000005222244400000088885448888000000000003333333333333333e7e333333333333300dddffff0000000
0bbb1bb00ccc2cc00eee4ee00229999229999220000052222444000008885f444f4888800000000033333333333333333e33333333333333000ddd2dddd00000
03bbbb3001cccc1008eeee8029999224422999920000522224440000885ffffffffff8880000000033333333333333333333333333333333000dd222dddd0000
003bb300001cc100008ee800299224444442299200005552444400005445666f456664f0000000003333333333333333333333333333333300ff22222dd00000
000330000001100000088000299244444444299200005222244400000445666f456664f00000000033333e333333333333333383333333330ffff222ff000000
000000000000000000000000299244444444299200005222244400000ff5666ff5666ff0000000005333e7e3333339333333387833333333000ffd2dff000000
000770000000000000000000299244444444299200005222444400000f45666445666f400000000035333e3333339793333333833333333500000dddfff00000
007777770000000000777700299224444442299200005552444400000f444f444f444f40000000000335333333333933333333333333535000000ddd3ff0b000
007777777770000007555570299992244229999200005222244400000ffffff88ffffff00000000000533533333333333333333333333500000000d003330000
07777777777777000700007052299992299992250000522224440000044f4488884f44f0000000000003535333333333333333333353300000000000003b0000
07777777777777770677776005522999999225500005555244444000044f4485684f44f0000000000000055333333333333333333535000000000000b0300000
676776766767767700666600000552299225500000552222444444000fffff8888fffff000000000000000053333333333333333355000000000000003300000
666666666666666600000000000005522550000005522222224444400f444f8818444f4000000000000000005353535353535353500000000000000003b00000
060660600606606000000000000000055000000055555555224444440f444f8888444f4000000000000000000535553535355530000000000000000033000000
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
00000000000000000000000033000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000032420000000000003242000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000033430000000000003343000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000032420000000000003242000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000033430000000000003343000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
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
000000000000000025260000000000000000000000002e2f002e2f2e2f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000035360000000000000000000000003e3f003e3f3e3f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707
__sfx__
000200000613006140061400614006130071300712006120051100510005100051000510006100061200613005140051300511006100061200613006140061400612006110061000610006100051200413005140
010c00000d22019200162001410017220171001610015100162202410024100231000d2001b200171001510023200232002470000000000000000000000222002120023200212001b200192001b2001920000000
010c00000000000000196230d6000d60000000196240d6000d600000001962300000196000000019623000000d6000000019623000000d6000000019624000000d6000000019623000000d600000001962300000
000c00000d2100d2000d2100d2000d2100d2000d21001200142100000014210012001421001200142100120012210002001221000200122100020012210002001721003200172100f20017210032001721003200
010c00003d6000000400000000003d6000000000000000003d6153d6003d600000003d600000003d600000003d6000000400000000003d6000000000000000003d615000043d615000003d6003d6003d60000000
000c00000d2201920016200141001722017100161001510016220241002410023100172001b22017100151001670023200247000000000000000000000000000232102121023210212101b210192101b21019210
0003000023e2423e2022e201fe2021e2020e201fe201ee201ce201ae2017e2014e2010e2010e10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000400001cf301ff3025f302bf302ef3034f3009100061000710009100071000a100091000810009100091000810008100071000a10008100081000a10008100081000710009100071000a100081000910009100
0004000031120291201e12017120141200e12009120041202bb3022b301bb3011b300ab3004b3001b202a310263101c3100f3100831002b100bd0005d0001d0001d0001d0001d000000000000000000000000000
010c00003d6000000400000000003d6000000000000000003d6150000400000000003d600000003d600000003d6000000400000000003d6000000000000000003d615000043d615000003d615000003d60000000
010c00000d2201920016200141001722017100161001510016220241002410023100172001b22017100151002322023200232200000000000000000000000000222202120022220212001b200192002022019200
__music__
01 01020304
00 0a020309
00 01020304
02 05020309

