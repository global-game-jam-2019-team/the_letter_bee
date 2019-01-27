pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
--------------------------------
------ the letter b-------------
-- global game jam 2019 --------
--------------------------------

local cam_x = 0
local cam_x_screen_limit = 48

local t = 0      -- current time
local p =              -- player
 { x = 64, y = 64, move_timer = 0,
   cycling = false, cycle_timer = 0,
   col = 8, mode = 1, point_right = true,
   r = 4,
   sprite="bee",
   carry_sprite=nil}
local bee_gravity = 0
local bee_gravity_carry = 1
local bee_speed = 3 -- 1.75
local bee_speed_carry = 1.5
local bee_jank_skip_rate = 3
local bee_jank_factor_y = 1.25
local bee_jank_factor_x = .5
local normal_gravity = 4
local normal_flee_speed = 4

-- the y position of the floor
local floor_y = 128 - 32
local hive_wall_thickness = 8

-- constants for the different screens
  map_screens = {
    hive = 1,
    blue_flowers_and_wasps = 2,
    spiders_and_trees = 3,
    houses_and_lake = 4,
    green_flowers = 5,
    pink_flowers_and_spiders = 6,
    houses_and_wasps = 7,
    open_space_variant_2 = 8,
    end_of_world = 9,
  }
  -- 1 = hive
  -- 2 = blue flowers & wasp
  -- 3 = spiders & two trees
  -- 4 = houses & lake?
  -- 5 = green flowers
  -- 6 = pink flowers + spiders
  -- 7 = houses with wasps
  -- 8 = (open space) tall grass?? (variant 2)
  -- 9 = end of world

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

function clone(input)
  local t = {}
  for key, value in pairs(input) do
    t[key] = value
  end
  return t
end


-- generate a rose function for entities
function euf_rose_xy(o)
  -- https://en.wikipedia.org/wiki/rose_(mathematics)
  o = {
    n = o.n and o.n or 1, -- the n value from wikipedia
    d = o.d and o.d or 1, -- the d value from wikipedia
    r = o.r and o.r or 16, -- the default radius
    f_cycle = o.f_cycle, -- the cycle time. 30fps
    f_offset = o.f_offset and o.f_offset or 0, -- the cycle offset
    x_r = o.x_r and o.x_r or o.r or 16, -- the x radius
    y_r = o.y_r and o.y_r or o.r or 16, -- the y radius
    rotate = o.rotate and o.rotate or 0, -- the tilt
    flip_x = o.flip_x and sgn(o.flip_x) or 1, -- flips x
    flip_y = o.flip_y and sgn(o.flip_y) or 1, -- flips y
  }

  local k = o.n / o.d
  log("rose1", o.n, o.d, o.r, o.f_cycle)
  log("rose2", o.f_offset, o.x_r, o.y_r, o.rotate)
  return function (entity, entities)
    local theta =
      ((t + o.f_offset) % (o.f_cycle * o.d)) / o.f_cycle
    -- theta = theta + o.rotate
    local offset_x = o.x_r * cos(k * theta) * cos(theta)
    local offset_y = o.y_r * cos(k * theta) * sin(theta)
    local xy_len = len(offset_x, offset_y)
    local angle = atan2(offset_x, offset_y) + o.rotate
    entity.x = entity.ox + xy_len * cos(angle) * o.flip_x
    entity.y = entity.oy + xy_len * sin(angle) * o.flip_y
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
  bee_busy   = {n= 41, w=1, h=1, cx=0, cy=0, r=1},
  bee_busier = {n= 57, w=1, h=1, cx=0, cy=0, r=1},

  food       = {n= 16, w=1, h=1, cx=4, cy=4, r=4, bouncy=true},
  food_green = {n= 32, w=1, h=1, cx=4, cy=4, r=4, bouncy=true},
  food_blue  = {n= 33, w=1, h=1, cx=4, cy=4, r=4, bouncy=true},
  food_pink  = {n= 34, w=1, h=1, cx=4, cy=4, r=4, bouncy=true},

  hive   =     {n=  3, w=2, h=2, cx=8, cy=8, r=8},
  exit   =     {n=  0, w=1, h=1, cx=8, cy=8, r=8},
  cloud  =     {n=  5, w=2, h=1, cx=8, cy=8},
  cloud2 =     {n= 48, w=2, h=1, cx=8, cy=8},
  floor  =     {n=  7, w=2, h=1, cx=4, cy=4},
  speech =     {n=  8, w=2, h=2, cx=8, cy=8, bouncy=true},
  sun    =     {n= 14, w=2, h=2, cx=8, cy=8},
  smoke_l=     {n= 50, w=1, h=1, cx=4, cy=4},
  smoke_s=     {n= 23, w=1, h=1, cx=4, cy=4},
  spider =     {n= 22, w=1, h=1, cx=4, cy=4, r=4},
  hornet =     {n= 21, w=1, h=1, cx=4, cy=4, r=4},

  heart     =  {n= 86, w=1, h=1, cx=4, cy=4, r=4, bouncy=true},

  honeycomb =  {n= 35, w=2, h=2, cx=8, cy=8, r=8},

  logo      =  {n=96, w=4, h=2, cx=16, cy=8}
  -- todo: tree.
}

_sfx = {
  beetalk    = 0,
  hurt       = 8,
  pickup     = 7,
  spider     = 6,
  hornet     = 6,
}

-- gathers spr parameters
function s(name, x, y, flip_x, flip_y)
  local sd = _s[name]
  return sd.n, x-sd.cx, y-sd.cy, sd.w, sd.h, flip_x, flip_y
end

-- gathers spr parameters
function sb(name, x, y, flip_x, flip_y)
  local sd = _s[name]
  local bounce = sd.bouncy and (t % 10 < 5) and 1 or 0
  return sd.n, x-sd.cx, y-sd.cy+bounce, sd.w, sd.h, flip_x, flip_y
end

-- entity reaction: delete
function er_delete(entity, entities)
  del(entities, entity)
end

-- entity reaction: carry
function er_carry(entity, entities)
  if p.carry_sprite ~= nil then return end
  p.carry_sprite = entity.type
  sfx(_sfx.pickup)
  del(entities, entity)
end

function erf_sound(sfx_id)
  return function(entity, entities)
    sfx(sfx_id)
  end
end

-- entity reaction: hurt
function er_hurt(entity, entities)
  local knockback = 10
  p.sprite = "bee_pink"
  sfx(_sfx.hurt)
  local theta = atan2(p.x-entity.x, p.y-entity.y)
  p.x = p.x + cos(theta) * knockback
  p.y = p.y + sin(theta) * knockback
end

-- entity reaction: drop
function er_drop(entity, entities)
  if p.carry_sprite == nil then return end
  add(
    entities,
    {
      type=p.carry_sprite,
      x=p.x,
      y=p.y + p.r + _s[p.carry_sprite].r + 2,
      updates={eu_fall},reactions={er_carry}})
  p.carry_sprite = nil
end

-- entity reaction: drop
function er_consume_carry(entity, entities)
  if p.carry_sprite == nil then return end
  p.carry_sprite = nil
end

-- entity reaction factory: consume carry only one type
function erf_consume_carry_only(type)
  return function(entity, entities)
    if p.carry_sprite == nil then return end
    if p.carry_sprite ~= type then return end
    p.carry_sprite = nil

    sfx(_sfx.pickup)
    goals[type] = true

    entity.post_draws = {
      function(entity, entities)
        if not goals[type] then return end
        -- log(type, entity.x, entity.y)
        local n, cx, cy, w, h, flip_x, flip_y = s(type, entity.x, entity.y)
        spr(n, cx, cy+1, w, h/2, flip_x, flip_y)
      end
    }

    -- log(type .. type, entity.x, entity.y)
  end
end

function epdf_lake(w,h, col)
  col = col and col or 12
  -- todo: fillp mask
  return function(entity, entities)
    circfill(entity.x, entity.y, h/2, col)
    circfill(entity.x+w, entity.y, h/2, col)
    rectfill(entity.x, entity.y-h/2, entity.x+w, entity.y+h/2, col)
  end
end

-- entity post draw factory: speech icon
function epdf_speech_text(text)
  return function(entity, entities)
    local sprite = _s[entity.type]
    local bounce = sprite.bouncy and (t % 10 < 5) and 1 or 0
    print(text, entity.x-sprite.cx/2, entity.y-sprite.cy/2+bounce, 0)
  end
end

-- entity post draw factory: speech icon
function epdf_speech_icon(type)
  return function(entity, entities)
    spr(sb(type, entity.x, entity.y))
  end
end

goals = {
  food_blue  = false,
  food_green = false,
  food_pink  = false,
}

-- entity post draw factory: goal
function epdf_speech_goal_indicator(goal)
  return function(entity, entities)
    if not goals[goal] then
      spr(sb(goal, entity.x, entity.y))
    else
      spr(sb("heart", entity.x, entity.y))
    end
  end
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
  p.sprite = "bee"
  local gravity = p.carry_sprite and bee_gravity_carry or bee_gravity
  p.y = p.y + gravity
  if t % bee_jank_skip_rate ~= 0 then
    local angle = rnd(1)
    local fx = cos(angle) * bee_jank_factor_x
    local fy = sin(angle) * bee_jank_factor_y
    p.x = p.x + fx
    p.y = p.y + fy
  end
end

function apply_floors(entities)
  if p.y < p.r then p.y = p.r end
  if p.y > floor_y then p.y = floor_y end
  local right_limit = get_screen_offset(#map_list_right)
  local left_limit = get_screen_offset(-#map_list_left + 1)
  if p.x > right_limit then p.x = right_limit end
  if p.x < left_limit then p.x = left_limit end
end

function apply_hive_walls(entities)
  if p.y > 128-hive_wall_thickness then p.y = 128-hive_wall_thickness end
  if p.x > 128-hive_wall_thickness then p.x = 128-hive_wall_thickness end
  if p.y < hive_wall_thickness then p.y = hive_wall_thickness end
  if p.x < hive_wall_thickness then p.x = hive_wall_thickness end
end

function control(entities)
  local used_speed = p.carry_sprite and bee_speed_carry or bee_speed
  -- left
  if b(0).isdown then
    p.x = p.x - used_speed
    p.point_right = false
  end
  -- right
  if b(1).isdown then
    p.x = p.x + used_speed
    p.point_right = true
  end
  -- up
  if b(2).isdown then p.y = p.y - bee_speed end
  -- down
  if b(3).isdown then p.y = p.y + bee_speed end

  if b(4).isdown then er_drop(null, entities) end
end

-- determines which entities the bee is touching
function check_overlap(entities)
  local bee_r = _s[p.sprite].r
  for i=#entities,1,-1 do
    local e = entities[i]
    e.was_touched = e.is_touched
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
          -- log("colliding", e.type, bee_r, e_r)
          -- log("colliding", dx, dy)
          -- log("colliding", d2, d2_limit)
          -- log("colliding", e.x, e.y, p.x, p.y)
        end
      end
      e.first_touch = e.is_touched and not e.was_touched
    end
  end
end

-- reacts to the bee touching entities
function apply_overlap(entities)
  -- _s.bee.n = 1
  for i=#entities,1,-1 do
    local e = entities[i]
    if e.is_touched then
      -- _s.bee.n = 2
      local reactions = e.reactions
      if reactions ~= nil then
        -- log("#reactions", #reactions)
        for reaction in all(reactions) do
          reaction(e, entities)
        end
      end
    end
  end
end

function apply_entity_updates(entities)
  for i=#entities,1,-1 do
    local e = entities[i]
    local updates = e.updates
    if updates ~= nil then
      -- log("#updates", #updates)
      for update in all(updates) do
        update(e, entities)
      end
    end
  end
end

function update_camera()
  cam_x_right_limit = p.x - cam_x_screen_limit
  cam_x_left_limit = p.x - (128 - cam_x_screen_limit)

  if cam_x > cam_x_right_limit then cam_x = cam_x_right_limit end
  if cam_x < cam_x_left_limit then cam_x = cam_x_left_limit end
end

--------------------------------
-------------- callback: draw --

-- gets the (x,y) offset for rendering a screen worth of map (16,16)
function get_map_offset(map_number)
  local row = flr((map_number - 1) / 8)
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
  rect(sx,sy, sx+128, sy+128)
  map(map_x,map_y, sx,sy, 16,16)
end

function get_screen(sx)
  return flr(sx / 128)
end

function pick_map_old()
  local i = rnd_index(available_maps)
  -- log("map", i, #available_maps)
  return available_maps[rnd_index(available_maps)]
end

function draw_entities(entities)
  for i=1,#entities do
    local e = entities[i]
    spr(sb(e.type, e.x, e.y))

    local post_draws = e.post_draws
    if post_draws ~= nil then
      -- log("#post_draws", #post_draws)
      for post_draw in all(post_draws) do
        post_draw(e, entities)
      end
    end
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


home_maps = {1}
available_maps = {2,3,4,5,6,7,8}

home_map = 1
map_list_right = {pick_map_old(),pick_map_old(),pick_map_old(),pick_map_old(),pick_map_old(),pick_map_old()}
map_list_left = {pick_map_old(),pick_map_old(),pick_map_old(),pick_map_old(),pick_map_old()}

function _update_overworld()
  t = (t + 1) % 32767
  update_buttons()
  update_bee()
  control(overworld_entities)
  apply_floors(overworld_entities)
  apply_entity_updates(overworld_entities)
  check_overlap(overworld_entities)

  apply_overlap(overworld_entities)
  apply_map_updates(overworld_updates)

  update_camera()

  log("stats","mem",stat(0),"cpu",stat(1))
end

function apply_map_updates(updates)
  for u in all(updates) do
    u()
  end
end

function parallax_clouds(seed, count, speed, scale, y)
  local y_spread = 24
  local reseed = rnd(10000)
  srand(seed)
  local right_limit = get_screen_offset(#map_list_right)
  local left_limit = abs(get_screen_offset(-#map_list_left + 1))
  right_limit = right_limit / (speed * 2)
  left_limit = left_limit / (speed * 2)
  for i=1,count do
    local sx = rnd(right_limit + left_limit) - left_limit
    local sy = y + rnd(y_spread)
    sx = sx + speed * -cam_x
    spr(s("cloud", sx, sy))
  end
  srand(reseed)
end

function _draw_overworld()
  cls"12"
  rectfill(cam_x-64,floor_y, cam_x+128+64,128, 3)
  camera(cam_x,0)
  parallax_clouds(100, 50, 0.25, 1, 16)
  parallax_clouds(105, 50, 0.125, 1, 24)

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
    local offset_y = _s[p.sprite].cy + _s[p.carry_sprite].cy
    spr(s(p.carry_sprite, p.x, p.y + offset_y))
  end

  -- bee
  spr(s(p.sprite, p.x, p.y, not p.point_right))
end

function go_to_overworld()
  music(0)
  _update = _update_overworld
  _draw = _draw_overworld
  cam_x = 0
  p.y = 52 + 8 + p.r + p.r
  p.x = 64
end

-- returns the map_id of an available map, probabilistically
function pick_map()
  local i = rnd_index(available_maps)
  log("map", i, #available_maps)
  return available_maps[rnd_index(available_maps)]
end

function init_overworld()
  -- 1 = hive
  -- *** 2 = blue flowers & wasp
  -- *** 3 = spiders & two trees
  -- *** 4 = houses & lake?
  -- *** 5 = green flowers
  -- *** 6 = pink flowers + spiders
  -- *** 7 = houses with wasps
  -- *** 8 = (open space) tall grass?? (variant 2)
  -- 9 = end of world

  map_list_left = {
    map_screens.spiders_and_trees,
    map_screens.open_space_variant_2,
    map_screens.blue_flowers_and_wasps,
    map_screens.end_of_world,
  }

  map_list_right = {
    map_screens.houses_and_lake,
    map_screens.green_flowers,
    map_screens.houses_and_wasps,
    map_screens.pink_flowers_and_spiders,
    map_screens.end_of_world,
  }

  overworld_entities = {}
  overworld_updates = {}
  init_map_data_all({map_screens.hive}, 0)
  init_map_data_all(map_list_left, -1)
  init_map_data_all(map_list_right, 1)
end

function init_map_data_all(map_ids, direction)
  for i=1,#map_ids do
    local map_id = map_ids[i]
    local map_data = map_setup[map_id]
    local distance_from_home = direction * i
    init_map_data_one(map_setup[map_id], map_id, distance_from_home)
  end
end

function init_map_data_one(map_data, map_id, distance_from_home)
  local screen_offset_x, _ = get_screen_offset(distance_from_home)
  if map_data.update ~= nil then
    local closure_object = {}
    add(overworld_updates, function()
      map_data.update(
        closure_object,
        map_data,
        distance_from_home,
        screen_offset_x)
    end)
  end

  log("init_map_data_one", map_id, distance_from_home, screen_offset_x, _)
  for entity in all(map_data.default_entities) do
    inject_entity(entity, screen_offset_x)
  end
end

--------------------------------
------------------ mode: hive --

function _update_hive()
  t = (t + 1) % 32767
  update_buttons()
  update_bee()
  control(hive_entities)

  apply_hive_walls(hive_entities)
  apply_entity_updates(hive_entities)
  check_overlap(hive_entities)
  apply_overlap(hive_entities)

  update_camera()

  log("stats","mem",stat(0),"cpu",stat(1))
end

function draw_busy_bees(sprite, count, seed, tx, ty)
  local reseed = rnd(1000)
  srand(seed)
  for i=1,count do
    local x = ((rnd(192) + t*tx) % 192) - 32
    local y = ((rnd(192) + t*ty) % 192) - 32
    spr(sprite.n, x,y, sprite.w,sprite.h)
  end
  srand(reseed)
end

function _draw_hive()
  cls"15"
  camera(0,0)
  draw_busy_bees(_s.bee_busy,   50, 100,  5, 1)
  draw_busy_bees(_s.bee_busier, 50, 101, -5, 1)
  draw_busy_bees(_s.bee_busy,   50, 102, -3, 2)
  draw_busy_bees(_s.bee_busier, 50, 103,  3, 2)

  map( 0,64-16, 0,0, 16,16)
  map(16,64-16, 0,0, 16,16)

  -- entities
  draw_entities(hive_entities)

  -- bee carry
  if p.carry_sprite ~= nil then
    local offset_y = _s[p.sprite].cy + _s[p.carry_sprite].cy
    spr(s(p.carry_sprite, p.x, p.y + offset_y))
  end
  -- bee
  spr(s(p.sprite, p.x, p.y, not p.point_right))
end

function go_to_hive()
  music(4)
  _update = _update_hive
  _draw = _draw_hive
  p.y = 128 - 8 - p.r - p.r
  p.x = 64
end

function go_to_title()
  _update = _update_title
  _draw = _draw_title
end

cardioid_loop = euf_rose_xy{n=1,d=3,r=46,r_x=46,f_cycle=-30*3,rotate=-0.25}

function _update_title()
  t = (t + 1) % 32767
  update_buttons()

  update_bee()
  p.ox = 64
  p.oy = 64
  cardioid_loop(p, nil)

  apply_hive_walls(title_entities)
  apply_entity_updates(title_entities)
  check_overlap(title_entities)
  apply_overlap(title_entities)

  if b(5).isdown then
    game_start()
  end

  if b(0).isdown and b(1).isdown then
    game_start()
    go_to_overworld()
  end
end

function _draw_title()
  cls(15)

  draw_busy_bees(_s.bee_busy,   50, 100,  5, 1)
  draw_busy_bees(_s.bee_busier, 50, 101, -5, 1)
  draw_busy_bees(_s.bee_busy,   50, 102, -3, 2)
  draw_busy_bees(_s.bee_busier, 50, 103,  3, 2)

  map( 0,64-16, 0,0, 16,16)
  map(32,64-16, 0,0, 16,16)

  sspr(0, 48, 32, 16, 10, 10, 108, 32)
  spr(46, 26, 58, 2, 2)

  -- entities
  draw_entities(title_entities)

  -- bee carry
  if p.carry_sprite ~= nil then
    local offset_y = _s[p.sprite].cy + _s[p.carry_sprite].cy
    spr(s(p.carry_sprite, p.x, p.y + offset_y))
  end
  -- bee
  spr(s(p.sprite, p.x, p.y, not p.point_right))

  -- title text
  print ("press x to begin", 32, 128-30)
end

-- go_to_hive()
-- go_to_overworld()
go_to_title()

-- the happy bee dance
eu_bee_jank = euf_rose_xy{n=1,d=6,r=5,f_cycle=15}
eu_bee_jank_45 = euf_rose_xy{n=1,d=6,r=5,f_cycle=15,x_r=2,rotate=0.125}

-- entity update: hornet roaming behavior
function eu_hornet_cycle(entity, entities)
  -- https://en.wikipedia.org/wiki/rose_(mathematics)
  local n = 4;
  local d = 6;
  local k = n / d -- petal count; doubled if even? see article
  local cycle_over_frames = 20 -- flr(rnd(45) + 45)
  local theta = ((t + entity.petal_r_offset) % (cycle_over_frames * d)) / cycle_over_frames
  local offset_x = entity.petal_r * cos(k * theta) * cos(theta)
  local offset_y = entity.petal_r * cos(k * theta) * sin(theta)
  entity.x = entity.ox + offset_x
  entity.y = entity.oy + offset_y
end

-- entity update: fall (to ground)
function eu_fall(entity, entities)
  entity.y = entity.y + normal_gravity
  if entity.y > floor_y then entity.y = floor_y end
end

function eu_fall_n_run_off(entity, entities)
  entity.y = entity.y + normal_gravity
  if entity.y <= floor_y then return end
  entity.y = floor_y
  if entity.has_run_off then return end
  entity.has_run_off = true
  entity.run_off_speed = rnd(normal_flee_speed * 2) - normal_flee_speed
  if entity.run_off_speed < 1 then entity.run_off_speed = sgn(entity.run_off_speed) end
  add(entity.updates, eu_run_off)
end

function eu_run_off(entity, entities)
  if entity.run_off_speed == nil then return end

  entity.x = entity.x - entity.run_off_speed
  if abs(entity.x - p.x) > 128 then
    er_delete(entity, entities)
  end
end

-- entity update: fall (entirely offscreen)
function eu_fall_off(entity, entities)
  entity.y = entity.y + normal_gravity
  if entity.y < -16 then er_delete(entity, entities) end
end

function reset_goals()
  for k,v in pairs(goals) do
    goals[k] = false
  end
end

-- initializes and starts the game
function game_start(entity, entities)
  p.carry_sprite = nil
  reset_goals()
  go_to_hive()
  p.x = 64
  p.y = 80
end

function er_spawn_on_hit(entity, entities)
  if not entity.first_touch then return end
  for v in all(entity.spawn_on_hit) do
    add(entities, v)
  end
end

-- prep entities
function _init()
  -- music(0)
  overworld_updates = {}
  overworld_entities = {
    {type="food", x=136,y=floor_y,reactions={er_carry}}
  }

  title_entities = {
    {type="exit",      x=64,   y=112,
      spawn_on_hit={
        {type="food_blue", x=32,y=64,reactions={er_carry}},
      },
      reactions={er_spawn_on_hit, reset_goals}},
    {type="honeycomb", x=96,   y=64,    reactions={erf_consume_carry_only("food_blue")}},
    {type="bee_blue",  x=96,   y=64-20,
      updates={eu_bee_jank}},
    {type="speech",    x=96-4, y=64-20-12,
      post_draws={epdf_speech_goal_indicator("food_blue")}},
    -- {type="food_blue", x=96-4, y=96-20-12,},
  }

  hive_entities = {
    {type="exit",      x=64,   y=128,   reactions={go_to_overworld}},

    {type="honeycomb", x=96,   y=96,    reactions={erf_consume_carry_only("food_blue")}},
    {type="bee_blue",  x=96,   y=96-20,
      updates={eu_bee_jank}},
    {type="speech",    x=96-4, y=96-20-12,
      post_draws={epdf_speech_goal_indicator("food_blue")}},
    -- {type="food_blue", x=96-4, y=96-20-12,},

    {type="honeycomb", x=64,   y=48,    reactions={erf_consume_carry_only("food_green")}},
    {type="bee_green", x=64,   y=48-20,
      updates={eu_bee_jank}},
    {type="speech",    x=64-4, y=48-20-12,
      post_draws={epdf_speech_goal_indicator("food_green")}},
    -- {type="food_green",x=64-4, y=48-20-12,},

    {type="honeycomb", x=32,   y=96,    reactions={erf_consume_carry_only("food_pink")}},
    {type="bee_pink",  x=32,   y=96-20,
      updates={eu_bee_jank}},
    {type="speech",    x=32-4, y=96-20-12,
      post_draws={epdf_speech_goal_indicator("food_pink")}},
    -- {type="food_pink", x=32-4, y=96-20-12,},
  }
  for entity in all(hive_entities) do
    entity.ox = entity.x
    entity.oy = entity.y
  end
  for entity in all(title_entities) do
    entity.ox = entity.x
    entity.oy = entity.y
  end

  map_setup = {
    [map_screens.hive] = { -- 1
      default_entities = {
        {type="hive", x=64, y=52,reactions={go_to_hive}},
        {type="exit", x=32, y=112,post_draws={epdf_lake(16,16,12)}},
        {type="exit", x=32+16, y=112+12,post_draws={epdf_lake(16,16,12)}},
      },
      update = function(o, map_data, distance_from_home, screen_offset_x)
        -- log("update 1", distance_from_home, screen_offset_x)
      end
    },
    [map_screens.blue_flowers_and_wasps] = { -- 2
      default_entities = {
        {type="food_blue", x=79,y=86,reactions={er_carry}},
        {type="food_blue", x=55,y=86,reactions={er_carry}},
        {type="food_blue", x=95,y=86,reactions={er_carry}},
        {type="hornet",x=64,y=16,petal_r=12,petal_r_offset=flr(rnd(3000)),
          updates={eu_hornet_cycle},reactions={er_drop, er_hurt}},
        {type="hornet",x=48,y=48,petal_r=32,petal_r_offset=flr(rnd(3000)),
          updates={eu_hornet_cycle},reactions={er_drop, er_hurt}},
        {type="hornet",x=96,y=30,petal_r=8,petal_r_offset=flr(rnd(3000)),
          updates={eu_hornet_cycle},reactions={er_drop, er_hurt}},
      },
      update = function(o, map_data, distance_from_home, screen_offset_x)
        -- log("update 2", distance_from_home, screen_offset_x)
      end
    },
    [map_screens.spiders_and_trees] = { -- 3
      default_entities = {
      },
      extra_entities = {
        {type="spider", x=16,y=-4,
          updates={eu_fall_n_run_off},reactions={er_drop, er_hurt, erf_sound(_sfx.spider)}},
        {type="spider", x=64,y=-8,
          updates={eu_fall_n_run_off},reactions={er_drop, er_hurt, erf_sound(_sfx.spider)}},
        {type="spider", x=112,y=-12,
          updates={eu_fall_n_run_off},reactions={er_drop, er_hurt, erf_sound(_sfx.spider)}},
      },
      update = function(o, map_data, distance_from_home, screen_offset_x)
        local local_px = p.x - screen_offset_x
        if local_px > 48 and local_px < 96 and not o.has_attacked then
          o.has_attacked = true
          for extra_entity in all(map_data.extra_entities) do
            inject_entity(extra_entity, screen_offset_x)
          end
          sfx(_sfx.spider)
        end
      end
    },
    [map_screens.houses_and_lake] = { -- 4
      default_entities = {
      },
      extra_entities = {
      },
      update = function(o, map_data, distance_from_home, screen_offset_x)
      end
    },
     [map_screens.green_flowers] = { -- 5
      default_entities = {
        {type="food_green", x=22,y=86,reactions={er_carry}},
        {type="food_green", x=62,y=86,reactions={er_carry}},
        {type="food_green", x=118,y=86,reactions={er_carry}},
      },
      extra_entities = {
      },
      update = function(o, map_data, distance_from_home, screen_offset_x)
      end
    },
    [map_screens.pink_flowers_and_spiders] = { -- 6
      default_entities = {
        {type="food_pink", x=38,y=86,reactions={er_carry}},
        {type="food_pink", x=54,y=86,reactions={er_carry}},
        {type="food_pink", x=102,y=86,reactions={er_carry}},
      },
      extra_entities = {
        {type="spider", x=16,y=-4,
          updates={eu_fall_n_run_off},reactions={er_drop, er_hurt, erf_sound(_sfx.spider)}},
        {type="spider", x=64,y=-8,
          updates={eu_fall_n_run_off},reactions={er_drop, er_hurt, erf_sound(_sfx.spider)}},
        {type="spider", x=112,y=-12,
          updates={eu_fall_n_run_off},reactions={er_drop, er_hurt, erf_sound(_sfx.spider)}},
      },
      update = function(o, map_data, distance_from_home, screen_offset_x)
        local local_px = p.x - screen_offset_x
        if local_px > 48 and local_px < 96 and not o.has_attacked then
          o.has_attacked = true
          for extra_entity in all(map_data.extra_entities) do
            inject_entity(extra_entity, screen_offset_x)
          end
          sfx(_sfx.spider)
        end
      end
    },
    [map_screens.houses_and_wasps] = { -- 7
      default_entities = {
        {type="hornet",x=96,y=16,petal_r=12,petal_r_offset=flr(rnd(3000)),
          updates={eu_hornet_cycle},reactions={er_drop, er_hurt}},
        {type="hornet",x=16,y=48,petal_r=32,petal_r_offset=flr(rnd(3000)),
          updates={eu_hornet_cycle},reactions={er_drop, er_hurt}},
        {type="hornet",x=48,y=30,petal_r=8,petal_r_offset=flr(rnd(3000)),
          updates={eu_hornet_cycle},reactions={er_drop, er_hurt}},
      },
      extra_entities = {
      },
      update = function(o, map_data, distance_from_home, screen_offset_x)
      end
    },
    [map_screens.open_space_variant_2] = { -- 8
      default_entities = {
      },
      extra_entities = {
      },
      update = function(o, map_data, distance_from_home, screen_offset_x)
      end
    },
    [map_screens.end_of_world] = { -- 9
      default_entities = {
      },
      extra_entities = {
      },
      update = function(o, map_data, distance_from_home, screen_offset_x)
      end
    },
  }

  init_overworld()
end

function inject_entity(entity, screen_offset_x)
  local new_entity = clone(entity)
  new_entity.x = new_entity.x + screen_offset_x
  new_entity.ox = new_entity.x
  new_entity.oy = new_entity.y
  add(overworld_entities, new_entity)
end

__gfx__
00000000000000000000000000000002200000000000000000077700bbbbbbbb000000000000000000000bbbbbbbbbbbbbbbbbbbbbb000000000000aa0000000
0000000000770005007700050000022a922000000000000000777770b3b3b3b30000077777700000000bbbbbbbbbbbbbbbbbbbbbbbbbb00000a000a99a000a00
000000000766705007667050000029a9a9a20000000000770777777033333333000777777777700000bb333333333333333333333333bb000a9aaa9449aaa9a0
0000000000766aa000766bb00002a2a9a929200000770777777777703333333300777777777777000bb33333333333333333333333333bb000a994499449a900
000000000a474a1a0b373b1b002a9a22229a92000777777777777777333333330777777777777770bb3333333333333333333333333333bb0a994999999499a0
000000004a4a4aaa3b3b3bbb02a2a9a9a9a929206776766767766766333333337777777777777777b33e333333333333393333333333333b0994000000004990
00000000049494000313130002a922a9a922a920666666666666666033333333777777777777777733e7e3333333333397933333333383330a490059005994a0
0000000000505000005050002a9a9a22229a9a920660600606600600333333337777777777777777333e33333333333339333333333878330a490059005994a0
000aa000000000000000000029a9a9a9a9a9a9a20aa0000000000000000000007777777777777776333333333333333333333333333383330a499999999994a0
00aa9a000077000500770005222a9a922a9a92220a8a0000020220200000000007777777777777603333333333833333333333e3333333330a499999909994a0
0aa979a0076670500766705029a2292442a229a20aaa077020212202000000000677777777777600333333333878333333333e7e333333330994990009994990
0aaa9aa000766cc000766ee029a9a2444429a9a200887667002112000000000000667777777660003333333333833333333333e3333333330a994999999499a0
00aaaa000c171c1c0e878e1e2a9a9244442a9a92009a6670002112000007700000006667776000003333333333333333333333333333333300a9944994499a00
000aa0001c1c1ccc8e8e8eee02a9a2444429a9200088770000282800007557000000000677000000333333333333333333333333333333330a9aaa9449aaa9a0
0000000001212100084848000029a2444429a200409aa000020220200067760300000000670000003333333333333333333333333393333300a000a99a000a00
00000000005050000050500000022222222220000444000020000002000660000000000006000000333338333333333333333333397933330000000aa0000000
000bb00000011000000ee00000000002200000000000522224440000005888000000000044000000333387833333333333333333339333330000000080000000
00bb1b000011d10000ee4e000000022992200000000052222444000000588888888000000000000033333833333333333e333333333333330000008880000000
0bb161b0011dfd100ee4f4e0000229999992200000005222244400000088885448888000000000003333333333333333e7e33333333333330ddd088880000000
0bbb1bb00111d1100eee4ee00229999229999220000052222444000008885f444f4888800000000033333333333333333e3333333333333300ddd888ddd00000
00bbbb000011110000eeee0029999224422999920000522224440000885ffffffffff8880000000033333333333333333333333333333333000ddd2dddddd000
000bb00000011000000ee000299224444442299200005552444400005445666f456664f00000000033333333333333333333333333333333000dd222dddd0000
000000000000000000000000299244444444299200005222244400000445666f456664f00000000033333e33333333333333338333333333008822222dd00000
000000000000000000000000299244444444299200005222244400000ff5666ff5666ff0000000005333e7e33333393333333878333333330888822288d00000
000770000000000000000000299244444444299200005222444400000f45666445666f405500000035333e3333339793333333833333333500088d2d88880000
007777770000000000777700299224444442299200005552444400000f444f444f444f40000000000335333333333933333333333333535000000ddd88883000
007777777770000007555570299992244229999200005222244400000ffffff88ffffff0000000000053353333333333333333333333350000000ddd3383b000
07777777777777000700007052299992299992250000522224440000044f4488884f44f00000000000035353333333333333333333533000000000d003330000
07777777777777770677776005522999999225500005555244444000044f4485684f44f0000000000000055333333333333333333535000000000000003b0000
676776766767767700666600000552299225500000552222444444000fffff8888fffff0000000000000000533333333333333333550000000000000b0300000
666666666666666600000000000005522550000005522222224444400f444f8818444f4000000000000000005353535353535353500000000000000003300000
060660600606606000000000000000055000000055555555224444440f444f8888444f4000000000000000000535553535355530000000000000000003b00000
00005ddd000000000066600000000000000000000000000003000030000000000000000000000000000000000000000000000000000000000000000000000000
00005ddddd000000077776000000000000000000000000003b3003b3004000000000077777000000000000000000000000000000000000000000000000000000
0000dddddddd0000077777600000000000000000000000003b3333b3044000000000007557700000000000000000000000000000000000000000000000000000
000ddd88848dd000057777600000000000066600000000003bb33bb3044f00000000005775570000000000000000000000000000000000000000000000000000
00dd44444444dd00705777760000000000677766000000003bbbbbb30444f0000000000777757000000000000000000000000000000000000000000000000000
0004885666848000777777776000000006777077660000003bbbbbb3044440000000022222222700000000000000000000000000000000000000000000000000
00048856668480007777777776000000060770007766660003bbbb30004400000000022552257500000000000000000000000000000000000000000000000000
00044456664440005777777777766000677070007007076000333300000300000000022572257500000000000000000000000000000000000000000000000000
0008845666888000055577777777760067077770707770760ee0ee00000300000000000077767500000000000000000000000000000000000000000000000000
000884888488800000000077775777666777777777777776eeeeeee0000300000000000772777500000000000000000000000000000000000000000000000000
0004444dd444400000000005770577775776005555555550eeeeeee0000030000000002227775000000000000000000000000000000000000000000000000000
000488dddd848000000000057770577756676607070707000eeeee00000030000000007777767000000000000000000000000000000000000000000000000000
000488d56d8480000000000057770575577776666666600000eee000000030000000755775570000000000000000000000000000000000000000000000000000
000444dddd44400000000000057770500567777777777500000e0000000003000000077557000000000000000000000000000000000000000000000000000000
000884dd1d8880000000000005777700005777555555500000000000000003000000007770000000000000000000000000000000000000000000000000000000
000884dddd8880000000000000555000000555000000000000000000000003000000000000000000000000000000000000000000000000000000000000000000
11110111011100011110101011001111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
21210122012200021210101012102221000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01010100010000001010101010100012000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01010100010000001010101010100010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01010100010000001010101010100010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01110111011101101110101010101111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01210122012202201210101010102122000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01010100010000001010101010100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01010100010000001010101010100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01010100010000001010101010101200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11110111011100011110111011201111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
22220222022200022220222022002222000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
22222222222222222222222222222222000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000032
42000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000033
43000000000000000000000000000033433343334333433343334333433343334333433343334333433343334333433300000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000032
42000000000000000000000000000032000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000033
43000000000000000000000000000033000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000032
42000000000000000000000000000032000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000033
43000000000000000000000000000033000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000032
42000000000000000000000000000032000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000033
43000000000000000000000000000033000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000032
42000000000000000000000000000032000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000033
43000000000000000000000000000033000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000032
42000000000000000000000000000032000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000033
43000000000000000000000000000033000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000032
42000000000000000000000000000032000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000033
43000000000000000000000000000033000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000032
42000000000000000000000000000032000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000033
43000000000000000000000000000033000000000000320000420000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000032
42000000000000000000000000000032423242324232430000334232423242324232423242324232423242324232423200000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000033
__map__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000a0b0c0d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0b0c0d00000000000000
000000000000001a1b1c1d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001a1b1c1d00000000000000
000000000000002a2b2c2d000000000000000000000000000000000000000000000000000000000000000a0b0c0d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a0b0c0d2a2b2c2d00000000000000
000000000000003a3b3c3d0000000000000000000000000000000000000000000000000000000a0b0c0d1a1b1c1d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001a1b1c1d3a3b3c3d00000a0b0c0d00
00000000000000002526000000000000000000000000000000000000000000000000000000001a1b1c1d2a2b2c2d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002a2b2c2d0025260000001a1b1c1d00
00000000000000002526000000000000000000000000000000000000000000000000000000002a2b2c2d3a3b3c3d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003a3b3c3d0025260000002a2b2c2d00
00000000000000002526000000000000000000000000000000000000000000000000000000003a3b3c3d0025260000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002526000025260000003a3b3c3d00
0000000000000000252600000000000000000000000000000000000000000000000000000000002526000025260000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002526000025260000000025260000
000000000000000025260000000000000000000000002e2f002e2f2e2f000000000000000000002526000025260000000000000000272840412728404140410000002e2f0000002e2f00000000002e2f000000002e2f2e2f000000002e2f00000000000040412728404100002728272800002526000025260000000025260000
000000000000000035360000000000000000000000003e3f003e3f3e3f000000000000000000003536000035360000000000000000373850513738505150510000003e3f0000003e3f00000000003e3f000000003e3f3e3f000000003e3f00000000000050513738505100003738373800003536000035360000000035360000
0707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707
0000000000000000000000000000000000000000000000000000000000000000000000000044450000004243000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000042430054550042435253000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000052530000000052530000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000a0b0c0d0000000a0b0c0d0000000000000a0b0c0d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001a1b1c1d0000001a1b1c1d0000000000001a1b1c1d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00002a2b2c2d0000002a2b2c2d0000000000002a2b2c2d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00003a3b3c3d0000003a3b3c3d0000000000003a3b3c3d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000002526000000000025260000000000000000252600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000002526000000000025260000000000000000252600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2e2f002526002e2f0000252600404100002e2f002526002e2f00000027284041000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3e3f003536003e3f0000353600505100003e3f003536003e3f00000037385051000000000000000000000000000000000007070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707070707
__sfx__
0006000006150061500815008150071400614007140091500815007150071500815008140061400614006140061500615007140081400815009140091500a1300915009140081400815008140071500715007150
010e00000d22019200162001410017220171001610015100162202410024100231000d2001b200171001510023200232002470000000000000000000000222002120023200212001b200192001b2001920000000
010e00000000000000196230d6000d60000000196240d6000d600000001962300000196000000019623000000d6000000019623000000d6000000019624000000d6000000019623000000d600000001962300000
000e00000d2100d2000d2100d2000d2100d2000d21001200142100000014210012001421001200142100120012210002001221000200122100020012210002001721003200172100f20017210032001721003200
010e00003d6000000400000000003d6000000000000000003d6153d6003d600000003d600000003d600000003d6000000400000000003d6000000000000000003d615000043d615000003d6003d6003d60000000
000e00000d2201920016200141001722017100161001510016220241002410023100172001b22017100151001670023200247000000000000000000000000000232002120023210212101b210192101b21019210
0003000023e2423e2022e201fe2021e2020e201fe201ee201ce201ae2017e2014e2010e2010e10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000400001cf301ff3025f302bf302ef3034f3009100061000710009100071000a100091000810009100091000810008100071000a10008100081000a10008100081000710009100071000a100081000910009100
0002000031120291201e12017120141200e12009120041202bb0022b001bb0011b000ab0004b0001b002a300263001c3000f3000830002b000bd0005d0001d0001d0001d0001d000000000000000000000000000
010e00003d6000000400000000003d6000000000000000003d6150000400000000003d600000003d600000003d6000000400000000003d6000000000000000003d615000043d615000003d615000003d60000000
010e00000d2201920016200141001722017100161001510016220241002410023100172001b22017100151002321023200232100000000000000000000000000222102120022210212001b200192002021019200
010e00000d2100d2000d2100d2000d2100d2000d21001200142100000014210012001421001200142100120017210032001721000200172100320017210002001221000200122100f20012210002001221003200
__music__
01 01020b04
00 0a020309
00 01020b04
02 05020309
03 00424344

