import AnimationFrame exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Keyboard
import Mouse
import Signal exposing (..)
import Text exposing (fromString)
import Time exposing (..)
import Window
import Random exposing (..)
import List

-- PORTS INCOMING --

port movement : Signal (Float, Float)
port isLocked : Signal Bool


-- PORTS OUTGOING --

port requestPointerLock : Signal ()
port requestPointerLock =
 Signal.map (always ())
  <| Signal.filter not False
  <| Signal.sampleOn Mouse.clicks isLocked

port exitPointerLock : Signal ()
port exitPointerLock =
  Signal.map (always ())
    <| Signal.filter identity False
    <| Signal.sampleOn (Keyboard.isDown 27) isLocked


-- CONFIG --

width   = 900
height  = 600
hWidth  = width / 2
hHeight = height / 2

-- pixels per frame
velMagIncrease = 0.005
velMagIncrease' = velMagIncrease / 16


-- HELPERS --

vecAdd : Vec -> Vec -> Vec
vecAdd (ax, ay) (bx, by) = (ax + bx, ay + by)

vecMul : Vec -> Vec -> Vec
vecMul (ax, ay) (bx, by) = (ax * bx, ay * by)

vecSub : Vec -> Vec -> Vec
vecSub (ax, ay) (bx, by) = (ax - bx, ay - by)

vecLen : Vec -> Float
vecLen (x, y) = sqrt (x * x + y * y)

boundVel : Float -> Float -> Float
boundVel a da = if abs a > abs da then 0 else a

chaseVec : Float -> Vec -> Vec -> Vec
chaseVec mag (ax, ay) (bx, by) = let dx = ax - bx
                                     dy = ay - by
                                     a  = atan2 dy dx
                                     x  = (cos a) * mag
                                     y  = (sin a) * mag
                                 in ( boundVel x dx, boundVel y dy )

toMove : Vec -> Vec
toMove (x, y) = (x - hWidth, -y + hHeight)

toVec : (Int, Int) -> Vec
toVec (x, y) = (toFloat x, toFloat y)

hit : Actor -> Actor -> Bool
hit player enemy = (vecLen <| vecSub player.pos enemy.pos) < player.radius + enemy.radius

boundX : number -> number
boundX = clamp 0 width

boundY : number -> number
boundY = clamp 0 height

boundedVec : Vec -> Vec
boundedVec (x, y) = (boundX x, boundY y)


-- MODEL --

type alias Vec = (Float, Float)
type alias Game = { player: Actor
                  , enemy: Actor
                  , state: GameState
                  , food: List Actor }
type alias Actor = { pos: Vec, velMag: Float, radius: Float, color: Color }

type GameEvent = Update (Float, Vec) | Locked | Reset | Unlocked | Food Actor
type GameState = Waiting | Play | Dead

initialPlayer : Actor
initialPlayer = { pos = (0,0), velMag = 0, radius = 15, color = black }

initialEnemy : Actor
initialEnemy = { initialPlayer | pos <- (500, 500), velMag <- 5, radius <- 30, color <- darkRed }

initialFood : Actor
initialFood = { initialPlayer | radius <- 10, color <- green }

initialGame : Game
initialGame = { player = initialPlayer,
                enemy  = initialEnemy,
                state  = Waiting,
                food   = [] }

randomFoodPosition = Random.pair (float 0 width) (float 0 height)

newFood : Time -> Actor
newFood t =  { initialFood | pos <- round t |> initialSeed |> generate randomFoodPosition |> fst }


-- UPDATE --

updatePlayer : Vec -> Actor -> Actor
updatePlayer p player = { player | pos <- p }

updateEnemy : Time -> Actor -> Actor -> Actor
updateEnemy dt enemy player = { enemy
                              | pos    <- vecAdd enemy.pos (chaseVec enemy.velMag player.pos enemy.pos)
                              , velMag <- enemy.velMag + dt * velMagIncrease'
                              }

updateGameInPlay : Time -> Vec -> Game -> Game
updateGameInPlay dt p g = let player' = updatePlayer p g.player
                              enemy   = updateEnemy dt g.enemy player'
                              isHit   = hit player' enemy
                          in if isHit then
                               { g | state <- Dead }
                             else
                               { g | player <- player'
                                   , enemy  <- enemy
                                   , food   <- List.filter (not << (hit player')) g.food }

updateGame : GameEvent -> Game -> Game
updateGame e g = case e of
                   Food f ->
                     case g.state of
                       Play -> if   List.length g.food < 6
                               then { g | food <- f :: g.food }
                               else g
                       _    -> g
                   Update (dt, p) ->
                     case g.state of
                       Play -> updateGameInPlay dt p g
                       _    -> g
                   Locked ->
                     case g.state of
                       Waiting -> { g | state <- Play }
                       _       -> g
                   Unlocked ->
                     { g | state <- Waiting }
                   Reset  ->
                     case g.state of
                       Dead -> { initialGame | state <- Play }
                       _    -> g


-- RENDER --

renderActor : Actor -> Form
renderActor {pos, color, radius} = circle radius |> filled color |> move (toMove pos)

renderToCollage : Game -> List Form
renderToCollage game = case game.state of
                         Waiting ->
                           [ scale 2 <| text <| fromString "Click to lock pointer and start game"]
                         Play ->
                           List.map renderActor <| game.player :: game.enemy :: game.food
                         Dead ->
                           [ scale 2 <| text <| fromString "You're dead",
                             move (0, -30) <| scale 1.5 <| text <| fromString "Click to play again" ]

render : (Int, Int) -> Game -> Element
render (w, h) game = color gray <| container w h middle
                                <| color white
                                <| collage width height
                                <| renderToCollage game


-- SIGNAL GRAPH --

food : Signal Time
food = every (2 * second)

position : Signal Vec
position = foldp (\p p' -> vecAdd p p' |> boundedVec) (0, 0) movement

events : Signal GameEvent
events = mergeMany
          [ map (\s -> if s then Locked else Unlocked) isLocked,
            map2 ((\dt p -> Update (dt, p))) frame <| sampleOn frame position,
            map (always Reset) Mouse.clicks,
            map (\t -> Food (newFood t)) food ]

main : Signal Element
main = render <~ Window.dimensions ~ Signal.foldp updateGame initialGame events
