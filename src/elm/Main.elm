module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html,h1)
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing (Posix)
import Setters
import Update
import Json.Decode as Decode
import Random

-- CONSTANT
boardSize : Int
boardSize = 40

type alias Position =
  { x : Int
  , y : Int
  }

type Direction = Up | Down | Left | Right

{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }
type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , coloredSquare : Int
  , snake : {
        cases : List Position
      , direction : Direction
      , head :  Position
    }
  , apple : Position
  , cherry : Position
  , cooldownCherry : Int
  , tempsApparitionCherry : Int
  , gameOver : Bool
  , wallOn : Bool
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time 0 {cases = [{x= boardSize//2 , y = boardSize//2}], direction = Up, head = {x= 0 , y = 0}} {x= 5 , y = 5} {x= 15 , y = 30} 0 0 False False
  |> Update.none

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyDown Key
  | NewApplePosition ( Int, Int )
  | NewCherryPosition ( Int, Int )

{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct 
 -|   Cmds. -}

hitTheWall : Model -> Bool
hitTheWall model = 
  case model.snake.cases of 
    h :: t ->
      if h.x < 1 || h.y < 1 || h.x == boardSize-1 || h.y == boardSize-1 then
       True
      else 
        False 
    _ ->
      False

updateSnake : Model -> Model
updateSnake model =
  let
      casesPrint = Debug.log "snake " snake
      snake = model.snake
      newSnake = updateSnakeBody model
  in
    {
      model | snake = newSnake.snake --newSnake
    }

updateSnakeBody: Model -> Model
updateSnakeBody model = 
  let 
      currentHead =
            model.snake.cases
                |> List.head
                |> Maybe.withDefault  {x= 0 , y = 0}
      newHeadPosition = getNewHeadPosition currentHead model.snake.cases model.snake.direction
      cases = movePositions model.snake.cases newHeadPosition
      newCases = if isSnakeEatApple model then cases else stripLast cases
  in
    {model |snake = {cases = newCases, head= currentHead, direction = model.snake.direction}}

isSnakeEatApple : Model -> Bool
isSnakeEatApple model = model.snake.head == model.apple

isSnakeEatCherry : Model -> Bool
isSnakeEatCherry model = model.snake.head == model.cherry

collisionAvecLuiMeme : Model -> Bool
collisionAvecLuiMeme model = False
    -- List.member model.snake.head (List.drop 1 model.snake.cases)

collisionWithRandomWall : Model -> Bool
collisionWithRandomWall model = False

randomPosition : Random.Generator ( Int, Int )
randomPosition =
    Random.pair (Random.int 0 (boardSize - 1)) (Random.int 0 (boardSize - 1))

movePositions : List Position -> Position -> List Position
movePositions positions newFirstPosition =
    newFirstPosition :: positions

getNewHeadPosition : Position -> List Position -> Direction -> Position
getNewHeadPosition currentHead positions direction =
    let
        maybeSnakeHead =
            positions |> List.head
    in
    case maybeSnakeHead of
        Nothing ->
            currentHead

        Just position ->
            case direction of
                Up ->
                    { position | y = modBy boardSize (position.y - 1) }

                Right ->
                    { position | x = modBy boardSize (position.x + 1) }

                Down ->
                    { position | y = modBy boardSize (position.y + 1) }

                Left ->
                    { position | x = modBy boardSize (position.x - 1) }

stripLast : List a -> List a
stripLast list = List.take ((List.length list) - 1) list    

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
  let
    time_ = Time.posixToMillis time
  in
    if time_ - model.lastUpdate >= 100 then

      if model.gameOver then
        (model, Cmd.none)
      else if hitTheWall model || hitTheWall model then 
        ({ model | gameOver = True }, Cmd.none)
      else if isSnakeEatApple model then 
        (updateSnake model, Random.generate NewApplePosition randomPosition )
      else if isSnakeEatCherry model then 
        ({model | snake = (updateSnake model).snake, cherry = {x= -2 , y = -2}, tempsApparitionCherry = 500}, Cmd.none)
      else
        updateSnake model
          |> Setters.setTime time_
          |> Setters.setLastUpdate time_
          |> Update.none

    else
      if model.cooldownCherry == 6000 then

        if model.cherry == {x= -1 , y = -1} then 
          (model, Random.generate NewCherryPosition randomPosition)
        else 
          if model.tempsApparitionCherry /= 500 then
            ({model | tempsApparitionCherry = model.tempsApparitionCherry + 1}, Cmd.none)
          else
            ({model | tempsApparitionCherry = 0, cooldownCherry = 0, cherry = {x= -1 , y = -1} }, Cmd.none)
      else 
        if model.cooldownCherry /= 6000 then
          ({model | cooldownCherry = model.cooldownCherry + 1}, Cmd.none )
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
    NewApplePosition ( x, y ) ->
      ( { model | apple = { x = x, y = y } }, Cmd.none )
    NewCherryPosition ( x, y ) ->
      ( { model | cherry = { x = x, y = y } }, Cmd.none )

{-| Manage all your view functions here. -}
cell : Bool -> Model-> Int -> Int -> Html msg
cell active model x y = 
  let 
    class = if active then "cell active" 
            else if model.apple.x == x && model.apple.y == y then "apple" 
            else if  y == 0 || x ==0 || y == boardSize-1 || x ==boardSize-1 then "wall"
            else if model.cherry.x == x && model.cherry.y == y then "cherry"  else "cell" 
  in
  Html.div [ Attributes.class class ] []

wallSquares : Bool -> Int -> Int -> Html msg
wallSquares active x y = 
  let
      class = if active then "cell active"
              else if x == 0 || y == 0 || x == boardSize || y == boardSize then "cell"
              else ""
  in
    Html.div [ Attributes.class class ] []
  
  --Html.div [ Attributes.class "grid" ]
  --++ (
    --case index of 
    --_ -> (cell (List.member {x=0, y=index}))
    --Loop.while ((<) boardSize 0 ((+) 1))
  --)


movingSquare : Model -> Html msg
movingSquare model =
  Html.div [ Attributes.class "grid" ]
    (List.concat 
      (List.indexedMap (\y elem ->
        List.indexedMap (\x _ ->
          (cell (List.member {x=x, y=y} model.snake.cases)) model x y
        )
        elem
       ) 
        (List.repeat boardSize (List.repeat boardSize 0))
      )
    )

actualTime : Model -> Html msg
actualTime model =
  Html.div [ Attributes.class "actual-time" ]
    [ 
      Html.text "Actual time"
        , model.time
        |> String.fromInt
        |> Html.text
        |> List.singleton
        |> Html.code []
      , Html.text "temps avant apparition du cherry : "
        , (6000 - model.cooldownCherry)
        |> String.fromInt
        |> Html.text
        |> List.singleton
        |> Html.code []
      , Html.text "temps restant avant reinitialisation du cherry : "
        , (500 - model.tempsApparitionCherry)
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
      [ Html.text "Projet Snake Gabriel-Matthias-Nathalia" ]
    , actualTime model
    , Html.button
      [ Events.onClick ToggleGameLoop, Attributes.class "btn" ]
      [ Html.text (String.join " " [word, "game loop"]) ]
    , if model.gameOver then Html.h2 [] [ Html.text "Game Over!" ] else Html.h2 [] [ Html.text "score : 0"  ]
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
