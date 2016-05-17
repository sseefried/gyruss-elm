 -- See this document for more information on making Pong:
-- http://elm-lang.org/blog/pong
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard
import Mouse
import Text
import Time exposing (..)
import Window exposing (Size)
import Maybe
import Html.App as App
import Html exposing(..)
import AnimationFrame
import Task
import Debug


-- MODEL

worldWidth = 100

shipCircleRadius = 40
blasterVel = 200

type State = Play | Pause


type  Msg
  = Fire
  | MouseMove (Size -> (Float, Float))
  | Tick Time
  | NoOp
  | Resize Size

type alias Ship =
  { pos: (Float, Float)
  , blaster: Maybe (Float, Float) -- Nothing means it hasn't been fired
  }

type alias Game =
  { state :      State
  , ship :       Ship
  , screenSize : Size
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
  , screenSize = Size 0 0
  }

init: (Game, Cmd Msg)
init = (defaultGame, Task.perform (\_ -> NoOp) Resize (Window.size))


-- UPDATE

update : Msg -> Game -> Game
update msg g =
  let s = g.ship
  in case msg of
       NoOp -> g
       Resize sz -> { g | screenSize = sz }
       MouseMove f ->
         let pos' = calcShipPos (f g.screenSize)
         in  { g | ship = { s | pos = pos' }}
       Fire ->
         let b = s.blaster
             ang = angleFor s.pos
             b' =
               case s.blaster of
                 Nothing -> Just (0, ang)
                 _ -> b
         in { g |  ship = { s | blaster = b' } }
       Tick delta ->
         let b = s.blaster
             ship' = { s | blaster = b'}
             b' =
               case s.blaster of
                 Just (d, ang') ->
                   if d > shipCircleRadius
                     then Nothing
                     else Just (d + blasterVel*delta, ang')
                 Nothing -> b
         in  { g | ship = ship'}

--------------------------------------------------------------------------
-- VIEW

view : Game -> Html Msg
view game = toHtml <|
  toScreenElement game.screenSize <|
    [ shipForm game.ship ]

-- Scales from world co-ordinates to screen co-ordinates
toScreenElement : Size -> List Form -> Element
toScreenElement sz forms =
  let h = sz.height
      w = sz.width
      side = min (toFloat w) (toFloat h)
      groupedForm = scale (side/worldWidth) <| group <| forms
   in container w h middle <| collage w h [ groupedForm ]


shipForm : Ship -> Form
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

--------------------------------------------------------------------------
-- WIRING

keyboardProcessor down keyCode =
  case (down, keyCode) of
    (True, 32) -> Fire
    _          -> NoOp

main =
  App.program
  { init = init
  , update = \msg m -> update msg m ! []
  , view = view
  , subscriptions =
      (\_ -> Sub.batch
        [ Window.resizes Resize
        , Keyboard.downs (keyboardProcessor True)
        , Keyboard.ups (keyboardProcessor False)
        , Mouse.moves (MouseMove << mouseToWorld)
        , AnimationFrame.diffs (Tick << inSeconds)
        ])

  }

--------------------------------------------------------------------------
-- UTILITIES

mouseToWorld : Mouse.Position -> Size -> (Float, Float)
mouseToWorld pos sz =
  let x' = toFloat pos.x
      y' = toFloat pos.y
      w' = toFloat sz.width
      h' = toFloat sz.height
      side = min w' h'
      sf = worldWidth/side
   in (sf*(x' - w'/2), sf*(y' - h'/2))