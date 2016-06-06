module Fun exposing (..)

import Html
import Html.App
import Element exposing (..)
import Collage exposing (..)
import Text exposing (..)
import Color exposing (..)

import Task
import Random
import Window
import Time exposing (Time, second)
import AnimationFrame exposing (times)
import Mouse exposing (Position)

import Debug exposing (log)


main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Dot =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , color : Color
  }
    
type alias Model =
  { winSize : Window.Size
  , dots : List Dot
  }

initModel : Model
initModel =
  { winSize = Window.Size 0 0
  , dots = []
  }


init : (Model, Cmd Msg)
init =
  ( initModel
  , Task.perform GetWindowSizeFail GetWindowSize Window.size
  )


type Msg
  = NoOp
  | GetWindowSizeFail String
  | GetWindowSize Window.Size
  | GetRandom (List Dot)
  | Tick Time


initDots : Window.Size -> Cmd Msg
initDots size =
  let
    hw =
      toFloat size.width / 2
    hh =
      toFloat size.height / 2
    len =
      50
    vmax =
      3
  in
    Random.generate GetRandom
      <| Random.list len
      <| Random.map5 Dot
           (Random.float (negate hw) hw)
           (Random.float (negate hh) hh)
           (Random.float (negate vmax) vmax)
           (Random.float (negate vmax) vmax)
           (Random.map (\h -> hsl (degrees h) 0.8 0.5 ) (Random.float 0 360))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetWindowSizeFail _ ->
      (model, Cmd.none)
    GetWindowSize size ->
      ( { model | winSize = size }
      , initDots size
      )
    GetRandom data ->
      ( { model | dots = data }
      , Cmd.none
      )
    MouseMove pos ->
      
    Tick t ->
      let
        w =
          toFloat model.winSize.width / 2
        h =
          toFloat model.winSize.height / 2
        ndots =
          List.map
            (\d ->
               let
                 vx' =
                   if d.x > w - 2 || d.x < -w - 2 then -d.vx else d.vx
                 vy' =
                   if d.y > h - 2 || d.y < -h - 2 then -d.vy else d.vy
               in
                 { d |
                   vx = vx'
                 , vy = vy'
                 , x = d.x + vx'
                 , y = d.y + vy'
                 }
            )
            model.dots
      in
        ( { model | dots = ndots }, Cmd.none)
    _ ->
      (model, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
       [ times Tick
       , Mouse.moves MouseMove
       ]


view : Model -> Html.Html Msg
view model =
  let
    rdots =
      List.map
        (\d -> move (d.x, d.y) <| filled d.color <| circle 2)
        model.dots

    lineStyle color =
      { defaultLine |
         color = color
      ,  width = 0.3
      }
    lines =
      List.map
        (\d ->
           List.map
             (\m ->
                traced (lineStyle d.color) <| segment (d.x, d.y) (m.x, m.y)
             )
             <| List.filter (\n -> n /= d) model.dots
        )
        model.dots
  in
    toHtml <| collage model.winSize.width model.winSize.height
             <| rdots ++ List.concat lines
