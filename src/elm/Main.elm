module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing (Posix)
import Setters
import Update
import Json.Decode as Decode
import List exposing (indexedMap,length)

-- CONSTANT
boardSize : Int
boardSize = 40

type alias Position =
  { x : Int
  , y : Int
  }

type alias Offset =
  { dx : Int
  , dy : Int
  }

{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }
type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , coloredSquare : Int
  , snake : {
        cases : List (Int, Int)
      , direction : Direction
      , head : (Int, Int)
      , tail : List (Int, Int)
    }
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time 0 {cases = [(0, 0)], direction = Up, head = (0,0), tail = [(0,0)]}
  |> Update.none

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyDown Key

{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct 
 -|   Cmds. -}
updateSnake : Model -> Model
updateSnake model =
  let 
      casesPrint = Debug.log "snake " snake

      newBody = List.take ((List.length snake.cases) - 1) model.snake.cases

      removedPart = List.drop ((List.length snake.cases) - 1) model.snake.cases

      updateCases = model.snake.cases ++ model.snake.tail

      snake = model.snake
      newSnake = {
          snake | cases = List.map(\(x,y) -> 
            if snake.direction == Up then 
              (x,modBy boardSize (y - 1))
            else
            if snake.direction == Right then
              (modBy boardSize (x+1),y)
            else 
            if snake.direction == Down then
              (x,modBy boardSize (y + 1))
            else
              (modBy boardSize (x - 1),y)
        
          ) snake.cases ++ nextTail

        , head =  currentHead
        
        , tail = nextTail
           
        }
      -- nextHead = adjustPosition model.snake.head model.direction
      currentHead =
            model.snake.cases
                |> List.head
                |> Maybe.withDefault ( 0, 0 )

      nextTail =  model.snake.tail

      
  in
    
    {
       model | snake = newSnake
    }

type Direction = Up | Down | Left | Right

directionToOffset: Direction -> Offset
directionToOffset direction =
  case direction of 
    Up -> Offset 0 -1
    Down -> Offset 0 1
    Left -> Offset -1 0
    Right -> Offset 1 0

applyOffset : Position -> Offset -> Position
applyOffset pos offset = Position (pos.x + offset.dx) (pos.y + offset.dy)

adjustPosition: Position -> Direction -> Position
adjustPosition pos dir = applyOffset pos (directionToOffset dir)


toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
  not gameStarted
  |> Setters.setGameStartedIn model
  |> Update.none

keyDown : Key -> Model -> ( Model, Cmd Msg )
keyDown key model =
  let snake = model.snake
      newSnake = 
        case key of 
          ArrowUp -> 
            { snake | direction = Up }
          ArrowDown -> 
            { snake | direction = Down }
          ArrowLeft -> 
            { snake | direction = Left }
          ArrowRight -> 
            { snake | direction = Right }
          Space -> 
            snake
  in
    ({ model | snake = newSnake }, Cmd.none)

  
nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
  let time_ = Time.posixToMillis time in
  if time_ - model.lastUpdate >= 1000 then
    updateSnake model
    |> Setters.setTime time_
    |> Setters.setLastUpdate time_
    |> Update.none
  else
    time_
    |> Setters.setTimeIn model
    |> Update.none

{-| Main update function, mainly used as a router for subfunctions -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleGameLoop -> toggleGameLoop model
    KeyDown key -> keyDown key model
    NextFrame time -> nextFrame time model

{-| Manage all your view functions here. -}
cell : Bool -> Html msg
cell active =
  let class = if active then "cell active" else "cell" in
  Html.div [ Attributes.class class ] []
  

movingSquare : Model -> Html msg
movingSquare model =
  Html.div [ Attributes.class "grid" ]
    (List.concat 
      (List.indexedMap (\y elem ->
        List.indexedMap (\x _ ->
          (cell (List.member (x, y) model.snake.cases))
        )
        elem
       ) 
        (List.repeat boardSize (List.repeat boardSize 0))
      )
    )

actualTime : Model -> Html msg
actualTime { time } =
  Html.div [ Attributes.class "actual-time" ]
    [ Html.text "Actual time"
    , time
      |> String.fromInt
      |> Html.text
      |> List.singleton
      |> Html.code []
    ]

explanations : Model -> Html Msg
explanations ({ gameStarted } as model) =
  let word = if gameStarted then "Stop" else "Start" in
  Html.div [ Attributes.class "separator" ]
    [ Html.h1 []
      [ Html.text "Welcome to the snake project!" ]
    , actualTime model
    , Html.button
      [ Events.onClick ToggleGameLoop, Attributes.class "btn" ]
      [ Html.text (String.join " " [word, "game loop"]) ]
    ]

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Html.main_ []
    [ Html.img [ Attributes.src "/logo.svg" ] []
    , explanations model
    , movingSquare model
    ]

{-| Parts for the runtime. Get key presses and subscribe to
 -|   requestAnimationFrame for the game loop. You don't have to bother with
 -|   this. -}
decodeArrow : String -> Decode.Decoder Key
decodeArrow value =
  case value of
    "ArrowUp" -> Decode.succeed ArrowUp
    "ArrowLeft" -> Decode.succeed ArrowLeft
    "ArrowRight" -> Decode.succeed ArrowRight
    "ArrowDown" -> Decode.succeed ArrowDown
    " " -> Decode.succeed Space
    _ -> Decode.fail "Not an arrow"

decodeKey : Decode.Decoder Msg
decodeKey =
  Decode.field "key" Decode.string
  |> Decode.andThen decodeArrow
  |> Decode.map KeyDown

subscriptions : Model -> Sub Msg
subscriptions { gameStarted } =
  let aF = Browser.Events.onAnimationFrame NextFrame
      base = Browser.Events.onKeyDown decodeKey :: [] in
    Sub.batch (if gameStarted then aF :: base else base)

{-| Entrypoint of your program -}
main : Program Flags Model Msg
main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    }
