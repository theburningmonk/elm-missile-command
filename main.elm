module MissileCommand where

import Mouse
import Random
import Text
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
data GameStatus    = NotStarted | Started | Ended
data MissileStatus = Flying Pos Pos Velocity -- the starting, end positions and velocity of flight
                   | Exploding Radius        -- the current radius of the blast
                   | Exploded
data MissileKind   = Friendly | Enemy
data ExplodeReason = ReachedTarget | CaughtInBlast | Collision

type Missile       = { x:Float, y:Float, kind:MissileKind, status:MissileStatus }
type GameState     = { friendlyMissiles:[Missile]
                     , enemyMissiles:[Missile]
                     , score:Int
                     , hp:Int
                     , status:GameStatus }

data Input = Time Ms                      -- time ticker
           | UserAction Pos               -- user action on the canvas
           | EnemyLaunch [(Float, Float)] -- enemy missile launches

defaultGame : GameState
defaultGame = { friendlyMissiles=[], enemyMissiles=[], score=0, hp=10, status=NotStarted }

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
         
newMissile : Pos -> Pos -> MissileKind -> Missile
newMissile start end kind =
  let velocity = calcVelocity start end
  in { x=start.x, y=start.y, kind=kind, status=Flying start end velocity }

pairWise : [a] -> [(a, a)]
pairWise lst = 
  let loop lst acc = 
    case lst of
      a::b::tl -> loop tl ((a, b)::acc)
      [] -> acc
  in loop lst []
  
getEnemyMissile : (Int, Int) -> (Float, Float) -> Missile
getEnemyMissile (windowW, windowH) (start, end) =
  let (w, h) = (toFloat windowW, toFloat windowH)
      getX x = w*x - w/2
      startPos = { x=getX start, y=h/2 }
      endPos   = { x=getX end, y=groundH - h/2 }
  in newMissile startPos endPos Enemy
  
stepMissile : Float -> GameState -> Missile -> (Missile, Maybe ExplodeReason)
stepMissile delta { friendlyMissiles, enemyMissiles } missile =
  case missile.status of
    -- for a flying missile, move its position
    Flying _ end { vx, vy } -> 
      let newX    = missile.x + vx*delta
          newY    = missile.y + vy*delta
          inBlastRadius = any (hitTest missile) <| friendlyMissiles++enemyMissiles
          reachedEnd    = if vy > 0 then newY > end.y else newY < end.y
      in if reachedEnd || inBlastRadius
         then ({ missile | x<-newX, y<-newY, status<-Exploding 0 }
               , Just <| if reachedEnd then ReachedTarget else CaughtInBlast )
         else ({ missile | x<-newX, y<-newY }, Nothing)
    -- for an exploding missile, expand the radius of the explosion
    Exploding radius -> 
      let newRadius = radius + explosionSpd*delta
          disappear = newRadius > blastRadius
      in if disappear 
         then ({ missile | status<-Exploded }, Nothing)
         else ({ missile | status<-Exploding newRadius }, Nothing)

stepGame : (Input, (Int, Int)) -> GameState -> GameState
stepGame (input, (windowW, windowH)) gameState =
  case (gameState.status, input) of
    (Started, EnemyLaunch lst) -> 
      let missiles = map (getEnemyMissile (windowW, windowH)) lst
      in { gameState | enemyMissiles<-missiles++gameState.enemyMissiles }
    (NotStarted, UserAction _) -> { gameState | status<-Started }
    (Started, UserAction end) -> 
      let commandTop = { x=0, y=toFloat -windowH/2 + groundH + commandH - 10 }
      in { gameState | friendlyMissiles<-newMissile commandTop end Friendly::gameState.friendlyMissiles }
    (Started, Time delta) -> 
      let (enemyMissiles, explodeReasons) = unzip <| map (stepMissile delta gameState) gameState.enemyMissiles
          (friendlyMissiles, _)           = unzip <| map (stepMissile delta gameState) gameState.friendlyMissiles
          -- deduct 1 HP for every enemy that managed to pass our defense
          enemyWins = filter ((==) (Just ReachedTarget)) explodeReasons |> length
          
          newHp     = gameState.hp-enemyWins  |> max 0
          newStatus = if newHp == 0 then Ended else Started
      in { gameState | enemyMissiles<-filter (((/=) Exploded) . .status) enemyMissiles
                     , friendlyMissiles<-filter (((/=) Exploded) . .status) friendlyMissiles
                     , hp<-newHp
                     , status<-newStatus}
    (_, _) -> gameState

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

drawBackground : (Int, Int) -> Element
drawBackground (windowW, windowH) =
  let (w, h)  = (toFloat windowW, toFloat windowH)
      groundY = -h/2 + groundH/2
      centreY = -h/2 + commandH
  in collage windowW windowH
     <| [ rect w h |> filled (rgb 0 0 0)
          , rect w groundH |> filled (rgb 255 255 40) |> move (0, groundY)
          , ngon 3 (commandH/2) 
            |> filled (rgb 255 255 40)
            |> rotate (degrees 90)
            |> move (0, centreY)]

drawGameInfo : (Int, Int) -> GameState -> Element
drawGameInfo (windowW, windowH) { hp, score }  =
  let (w, h) = (toFloat windowW, toFloat windowH)
  in collage windowW windowH
     <| [ txt (move (-w/3, h/2-10)) ("Score "++show score)
          , txt (move (w/3, h/2-10)) ("HP "++show hp)]
            
drawGame : (Int, Int) -> GameState -> Element
drawGame (windowW, windowH) gameState =  
  let content = 
    case gameState.status of
      NotStarted -> [ txt id "CLICK anywhere to START." ]
      Started    -> concat [ choose drawExplosions gameState.friendlyMissiles
                           , choose drawExplosions gameState.enemyMissiles
                           , choose drawTrail gameState.friendlyMissiles
                           , choose drawTrail gameState.enemyMissiles
                           , choose drawMissile gameState.friendlyMissiles
                           , choose drawMissile gameState.enemyMissiles ]
      Ended      -> [ txt id "GAME OVER!" ]
  in collage windowW windowH content

display : (Int, Int) -> GameState -> Element
display dim gameState = layers [ drawBackground dim
                               , drawGame dim gameState
                               , drawGameInfo dim gameState]

txt : (Form -> Form) -> String -> Form
txt f msg = msg |> toText |> Text.color white |> Text.monospace |> leftAligned |> toForm |> f

userInput : Signal Input
userInput = 
  -- converts the (x, y) coordinates of a mouse click to the coordinate system used by the collage
  let convert (w, h) (x, y) = (toFloat x - toFloat w/2, toFloat h/2 - toFloat y)
  in convert<~Window.dimensions~Mouse.position
     |> sampleOn Mouse.clicks
     |> lift (\(x, y) -> UserAction { x=x, y=y })              
            
enemyLaunch : Signal Input
enemyLaunch = Random.range 0 5 (every <| 2*second) 
              |> lift ((*) 2)     -- every missile needs a pair of coordinates
              |> Random.floatList -- turn each signal into n floats [0..1]
              |> lift pairWise
              |> lift EnemyLaunch

delta = fps 60
timer : Signal Input
timer = sampleOn delta <| (\n -> Time <| n / 1000)<~delta

input : Signal Input
input = merge timer userInput |> merge enemyLaunch

gameState : Signal GameState
gameState = foldp stepGame defaultGame ((,)<~input~Window.dimensions)

main = display<~Window.dimensions~gameState