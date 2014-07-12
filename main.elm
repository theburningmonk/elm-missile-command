module MissileCommand where

import Mouse
import Random
import Window

type Pos = { x:Float, y:Float }
type Ms  = Float -- milliseconds

groundH      = 30   -- height of the ground
commandH     = 40   -- height of the command
missileW     = 10   -- width of a missile object
speed        = 50   -- speed of missiles per second in a straight line
explosionSpd = 30   -- speed of the explosion per second
blastRadius  = 50   -- max radius of an explosion

type Velocity = { vx:Float, vy:Float }
type Radius   = Float
data MissileStatus = Flying Pos Pos Velocity -- the starting, end positions and velocity of flight
                   | Exploding Radius
data MissileKind   = Friendly | Enemy

type Missile   = { x:Float, y:Float, kind:MissileKind, status:MissileStatus }
type GameState = { friendlyMissiles:[Missile]
                 , enemyMissiles:[Missile]
                 , score:Int
                 , health:Int }

data Input = Time Ms                  -- time ticker
           | UserAction Pos           -- user action on the canvas
           | EnemyLaunch [(Pos, Pos)] -- enemy missile launches

defaultGame : GameState
defaultGame = { friendlyMissiles=[], enemyMissiles=[], score=0, health=100 }

filterJust : Maybe a -> Bool
filterJust x =
  case x of
    Just _ -> True
    _      -> False
    
extractJust : Maybe a -> a
extractJust x =
  case x of
    Just x' -> x'

choose : (a -> Maybe b) -> [a] -> [b]
choose f lst = map f lst |> filter filterJust |> map extractJust

calcVelocity : Pos -> Pos -> Velocity
calcVelocity start end = 
  let distH = end.x-start.x
      distV = end.y-start.y
      angle = atan2 distV distH
  in { vx=speed*cos angle, vy=speed*sin angle }
  
-- check if missile 1 is in the blast radius of missile 2
hitTest : Missile -> Missile -> Bool
hitTest missile1 missile2 =
  let isCollission x y centreX centreY radius =
    let (minX, maxX, minY, maxY) = (centreX-radius, centreX+radius, centreY-radius, centreY+radius)
    in minX <= x
       && x <= maxX
       && minY <= y 
       && y <= maxY
  in case (missile1.status, missile2.status) of
      -- when caught in another missile's blast then it's a 'hit'
      (Flying _ _ _, Exploding radius) -> isCollission missile1.x missile1.y missile2.x missile2.y radius        
      -- when different kinds of missiles collide they 'hit'
      (Flying _ _ _, Flying _ _ _) ->
        missile1.kind /= missile2.kind 
        && isCollission missile1.x missile1.y missile2.x missile2.y (missileW/2)
      (_, _) -> False

stepMissile : Float -> GameState -> Missile -> Maybe Missile
stepMissile delta { friendlyMissiles, enemyMissiles } missile =
  case missile.status of
    -- for a flying missile, move its position
    Flying _ end { vx, vy } -> 
      let newX    = missile.x + vx*delta
          newY    = missile.y + vy*delta
          inBlastRadius = any (hitTest missile) <| friendlyMissiles++enemyMissiles
          explode = if vy > 0 then newY > end.y else newY < end.y
      in if explode || inBlastRadius
         then Just { missile | x<-newX, y<-newY, status<-Exploding 0 }
         else Just { missile | x<-newX, y<-newY }
    -- for an exploding missile, expand the radius of the explosion
    Exploding radius -> 
      let newRadius = radius + explosionSpd*delta
          disappear = newRadius > blastRadius
      in if disappear 
         then Nothing
         else Just { missile | status<-Exploding newRadius }
         
newMissile : Pos -> Pos -> MissileKind -> Missile
newMissile start end kind =
  let velocity = calcVelocity start end
  in { x=start.x, y=start.y, kind=kind, status=Flying start end velocity }

stepGame : (Input, (Int, Int)) -> GameState -> GameState
stepGame (input, (windowW, windowH)) gameState =   
  case input of
    EnemyLaunch lst -> 
      let missiles = map (\(start, end) -> newMissile start end Enemy) lst
      in { gameState | enemyMissiles <- missiles++gameState.enemyMissiles }
    UserAction end -> 
      let commandTop = { x=0, y=toFloat -windowH/2 + groundH + commandH - 10 }
      in { gameState | friendlyMissiles <- newMissile commandTop end Friendly::gameState.friendlyMissiles }
    Time delta -> 
      let enemyMissiles    = choose (stepMissile delta gameState) gameState.enemyMissiles
          friendlyMissiles = choose (stepMissile delta gameState) gameState.friendlyMissiles
      in { gameState | enemyMissiles    <- enemyMissiles
                     , friendlyMissiles <- friendlyMissiles }

drawMissile : Missile -> Maybe Form
drawMissile { x, y, kind, status } = 
  case status of
    Flying _ _ _ ->
      let color = if kind==Enemy then red else blue
      in ngon 4 (missileW/2) |> filled color |> move (x, y) |> Just
    _ -> Nothing
  
drawTrail : Missile -> Maybe Form
drawTrail { x, y, kind, status } = 
  case status of
    Flying start _ _ ->
      let (colour, alphaVal, lineW) = if kind==Enemy then (red, 0.5, 5) else (blue, 0.2, 1)
          lineStyle = solid colour
      in path [ (x, y), (start.x, start.y) ] 
         |> traced ({ lineStyle | width<-lineW })
         |> alpha alphaVal
         |> Just
    _ -> Nothing
    
drawExplosions : Missile -> Maybe Form
drawExplosions { x, y, status } =
  case status of
    Exploding radius ->
      circle radius
      |> filled (rgb 150 170 150)
      |> alpha 0.5
      |> move (x, y)
      |> Just
    _ -> Nothing

display : (Int, Int) -> GameState -> Element
display (windowW, windowH) gameState =
  let (w, h)  = (toFloat windowW, toFloat windowH)
      groundY = -h/2 + groundH/2
      centreY = -h/2 + commandH
  in collage windowW windowH
     <| concat [
          [ rect w h |> filled (rgb 0 0 0)
          , rect w groundH  |> filled (rgb 255 255 40)
                            |> move (0, groundY)
          , ngon 3 (commandH/2) 
            |> filled (rgb 255 255 40)
            |> rotate (degrees 90)
            |> move (0, centreY) ]
            
          , choose drawExplosions gameState.friendlyMissiles
          , choose drawExplosions gameState.enemyMissiles          
          , choose drawTrail gameState.friendlyMissiles
          , choose drawTrail gameState.enemyMissiles
          , choose drawMissile gameState.friendlyMissiles
          , choose drawMissile gameState.enemyMissiles
        ]

userInput : Signal Input
userInput = 
  -- converts the (x, y) coordinates of a mouse click to the coordinate system used by the collage
  let convert (w, h) (x, y) = (toFloat x - toFloat w/2, toFloat h/2 - toFloat y)
  in convert<~Window.dimensions~Mouse.position
     |> sampleOn Mouse.clicks
     |> lift (\(x, y) -> UserAction { x=x, y=y })
            
pairWise : [a] -> [(a, a)]
pairWise lst = 
  let loop lst acc = 
    case lst of
      a::b::tl -> loop tl ((a, b)::acc)
      [] -> acc
  in loop lst []
  
getEnemyLaunches : (Int, Int) -> [Float] -> Input
getEnemyLaunches (windowW, windowH) lst =
  let (w, h) = (toFloat windowW, toFloat windowH)
      getX x = w*x - w/2
      startY = h/2
  in pairWise lst
     |> map (\(start, end) -> ({ x=getX start, y=startY }, { x=getX end, y=30 - h/2 }))
     |> EnemyLaunch
            
enemyLaunch : Signal Input
enemyLaunch =
  Random.range 0 5 (every <| 2*second) -- how many missiles to launch
  |> lift ((*) 2)                 -- every missile needs a pair of coordinates
  |> Random.floatList             -- turn each signal into n floats [0..1]
  |> lift2 getEnemyLaunches Window.dimensions

delta = fps 60
timer : Signal Input
timer = sampleOn delta <| (\n -> Time <| n / 1000)<~delta

input : Signal Input
input = merge timer userInput |> merge enemyLaunch

gameState : Signal GameState
gameState = foldp stepGame defaultGame ((,)<~input~Window.dimensions)

main = display<~Window.dimensions~gameState