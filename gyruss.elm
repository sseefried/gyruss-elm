 -- See this document for more information on making Pong:
-- http://elm-lang.org/blog/pong
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Mouse
import Text
import Time exposing (..)
import Window
import Maybe
import Debug
import Transform2D


-- MODEL

worldWidth = 100

shipCircleRadius = 40
blasterVel = 200

type State = Play | Pause


type alias Input =
  { space : Bool
  , mousePos: (Float, Float)
  , delta : Time
  }

type alias Ship =
  { pos: (Float, Float)
  , blaster: Maybe (Float, Float) -- Nothing means it hasn't been fired
  }

type alias Game =
  { state : State
  , ship: Ship
  }


angleFor : (Float, Float) -> Float
angleFor (x,y) = atan2 y x

ship : (Float, Float) -> Ship
ship (x,y) = let pos = calcShipPos (x,y)
              in  Ship pos Nothing

calcShipPos: (Float, Float) -> (Float,Float)
calcShipPos (x,y) = let ang = angleFor (x, y)
                     in (shipCircleRadius*cos ang, -shipCircleRadius*sin ang)

defaultGame : Game
defaultGame =
  { state = Pause
  , ship = ship (worldWidth/2, worldWidth)

  }



-- UPDATE

update : Input -> Game -> Game
update inp g =
  let pos' = calcShipPos inp.mousePos
      s = g.ship
      ship' = { s | pos = pos', blaster = b'}
      b = s.blaster
      ang = angleFor s.pos
      b' = case s.blaster of
             Just (d, ang') ->
               if d > shipCircleRadius then Nothing else Just (d + blasterVel*inp.delta, ang')
             Nothing ->
               if inp.space then Just (blasterVel*inp.delta, ang) else Nothing
   in { g | ship = ship' }

--physicsUpdate dt obj =
--  { obj |
--      x = obj.x + obj.vx * dt,
--      y = obj.y + obj.vy * dt
--  }

----------------------------------------------------------------------------------------------
-- VIEW

view : (Int,Int) -> Game -> Element
view bounds game =
  toScreenElement bounds <|
    [ shipForm game.ship ]

-- Scales from world co-ordinates to screen co-ordinates
toScreenElement : (Int,Int) -> List Form -> Element
toScreenElement (w,h) forms =
  let side = min (toFloat w) (toFloat h)
      groupedForm = scale (side/worldWidth) <| group <| forms
   in container w h middle <| collage w h [ groupedForm ]


shipForm :  Ship -> Form
shipForm s =
  let (x,y) = s.pos
      bForm = case s.blaster of
               Just binp -> [blasterForm binp]
               Nothing   -> []
  in group ((ngon 3 3.0
               |> filled black
               |> move  s.pos
               |> rotate (atan2 y x + pi))::bForm)


blasterForm: (Float,Float) -> Form
blasterForm (d, ang) =
  let ball x = circle 0.7 |> filled red |> moveX x
      twoBalls = rotate (ang + pi/2) <| group [ball 1, ball (-1)]
      d' = shipCircleRadius - d
   in twoBalls |> move (d'*cos ang, d'*sin ang)
textGreen =
  rgb 160 200 160

txt f string =
  Text.fromString string
    |> Text.color textGreen
    |> Text.monospace
    |> f
    |> leftAligned


-- SIGNALS

main =
  Signal.map2 view Window.dimensions gameState


gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input


delta =
  Signal.map inSeconds (fps 35)

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map3 Input
      Mouse.isDown
      (Signal.map2 mouseToWorld Window.dimensions Mouse.position)
      delta

-- UTILITIES

mouseToWorld : (Int,Int) -> (Int, Int) -> (Float, Float)
mouseToWorld (w,h) (x,y) =
  let x' = toFloat x
      y' = toFloat y
      w' = toFloat w
      h' = toFloat h
      side = min w' h'
      sf = worldWidth/side
   in (sf*(x' - w'/2), sf*(y' - h'/2))