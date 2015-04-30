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
velMagIncrease = 0.0001

startGameMessage = fromString "Click to lock pointer and start game"
playAgainMessage = fromString "Click to play again"
deadMessage = fromString "You're dead"

-- HELPERS --

vecOp : (Float -> Float -> Float) -> Vec -> Vec -> Vec
vecOp op (ax, ay) (bx, by) = (op ax bx, op ay by)

vecAdd : Vec -> Vec -> Vec
vecAdd = vecOp (+)

vecMul : Vec -> Vec -> Vec
vecMul = vecOp (*)

vecSub : Vec -> Vec -> Vec
vecSub = vecOp (-)

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
                  , food: List Actor
                  , points: Int }

type alias Actor = { pos: Vec, velMag: Float, radius: Float, color: Color }

type GameEvent = Update (Float, Vec) | Locked | Reset | Unlocked | Food Actor
type GameState = Waiting | Play | Dead

initialPlayer : Actor
initialPlayer = { pos = (0,0), velMag = 0, radius = 15, color = black }

initialEnemy : Actor
initialEnemy = { initialPlayer | pos <- (500, 500), velMag <- 5, radius <- 30, color <- darkRed }

initialFood : Actor
initialFood = { initialPlayer | radius <- 10, color <- darkGreen }

initialGame : Game
initialGame = { player = initialPlayer
              , enemy  = initialEnemy
              , state  = Waiting
              , food   = []
              , points = 0 }

randomFoodPosition = Random.pair (float 0 width) (float 0 height)

newFood : Time -> Actor
newFood t =  { initialFood | pos <- round t |> initialSeed |> generate randomFoodPosition |> fst }


-- UPDATE --

updatePlayer : Vec -> Actor -> Actor
updatePlayer pos player = { player | pos <- pos }

updateEnemy : Time -> Actor -> Actor -> Actor
updateEnemy dt player enemy = { enemy
                              | pos    <- vecAdd enemy.pos (chaseVec enemy.velMag player.pos enemy.pos)
                              , velMag <- enemy.velMag + dt * velMagIncrease
                              }

updateFood : Actor -> List Actor -> List Actor
updateFood player food = List.filter (hit player >> not) food

updatePoints : List Actor -> List Actor -> Int -> Int
updatePoints food food' points = points + (List.length food) - (List.length food')

updateGameInPlay : Time -> Vec -> Game -> Game
updateGameInPlay dt pos game = let player' = updatePlayer pos game.player
                                   enemy'  = updateEnemy  dt player' game.enemy
                                   food'   = updateFood   player' game.food
                                   points' = updatePoints game.food food' game.points
                            in if hit player' enemy' then
                                 { game | state <- Dead
                                        , points <- points' }
                               else
                                 { game | player <- player'
                                        , enemy  <- enemy'
                                        , food   <- food'
                                        , points <- points' }

updateGame : GameEvent -> Game -> Game
updateGame e g = case e of
                   Update (dt, p) ->
                     case g.state of
                       Play -> updateGameInPlay dt p g
                       _    -> g
                   Food f ->
                     case g.state of
                       Play -> if   List.length g.food < 6
                               then { g | food <- f :: g.food }
                               else g
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

renderPoints : Int -> Form
renderPoints p = toString p |> fromString |> text |> scale 2 |> move (0, hHeight - 30)

renderActor : Actor -> Form
renderActor {pos, color, radius} = circle radius |> filled color |> move (toMove pos)

renderToCollage : Game -> List Form
renderToCollage game = case game.state of
                         Play ->
                           renderPoints game.points :: (List.map renderActor <| game.player :: game.enemy :: game.food)
                         Waiting ->
                           [ scale 2 <| text <| startGameMessage]
                         Dead ->
                           [ scale 2 <| text <| deadMessage,
                             move (0, -30) <| scale 1.5 <| text <| playAgainMessage,
                             renderPoints game.points ]

render : (Int, Int) -> Game -> Element
render (w, h) game = color darkRed <| container w h middle
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

game : Signal Game
game = Signal.foldp updateGame initialGame events

main : Signal Element
main = Signal.map2 render Window.dimensions game
