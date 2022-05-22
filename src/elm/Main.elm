module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html,h1)
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing (Posix)
import Html.Events exposing (onClick)
import Setters
import Update
import Json.Decode as Decode
import Random

-- CONSTANT
defaultBoardSize : Int
defaultBoardSize = 40
type OtherBoard = Board20 | Board10

type alias Position =
  { x : Int
  , y : Int
  }

type alias BoardSize =
  { size : Int
  }

type Direction = Up | Down | Left | Right

{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }
type alias Model =
  { 
  boardSize : Int
  , gameStarted : Bool
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
  , score : Int
  , tempsAvantAugmentationScore : Int
  , wallOn : Bool
  , randomWallOn : Bool
  , randomWall : List Position
  }


init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model defaultBoardSize False time time 0 {cases = [{x= defaultBoardSize//2 , y = defaultBoardSize//2}], direction = Up, head = {x= 0 , y = 0}} {x= 5 , y = 5} {x= 15 , y = 15} 0 0 False 0 0 False False [{x=-1,y=-1}] --{x= 15 , y = 30}
  |> Update.none

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyDown Key
  | NewApplePosition ( Int, Int )
  | NewCherryPosition ( Int, Int )
  | WallIsChecked Bool
  | RandomWallIsChecked Bool 
  | ExtraWallUpdate ( Int, Int )
  | UpdateBoardSize Int

{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct 
 -|   Cmds. -}

hitTheWall : Model -> Bool
hitTheWall model = 
  case model.snake.cases of 
    h :: t ->
      if h.x < 1 || h.y < 1 || h.x == model.boardSize-1 || h.y == model.boardSize-1 then
       True
      else 
        False 
    _ ->
      False

updateSnake : Model -> Model
updateSnake model =
  let
      snake = model.snake
      newSnake = updateSnakeBody model
      newScore = setScore model
  in
    {
      model | snake = newSnake.snake, score = newScore.score
    }

collisionWithWallUpdateSnake : List Position -> List Position
collisionWithWallUpdateSnake positions = 
    positions
      |> List.reverse
      |> List.drop 2
      |> List.reverse
  

updateSnakeBody: Model -> Model
updateSnakeBody model = 
  let 
      currentHead =
            model.snake.cases
                |> List.head
                |> Maybe.withDefault  {x= 0 , y = 0}
      newHeadPosition = getNewHeadPosition model currentHead model.snake.cases model.snake.direction
      cases = movePositions model.snake.cases newHeadPosition

      newCases = 
        if isSnakeEatApple model then cases 
        else if collisionWithRandomWall model then collisionWithWallUpdateSnake cases
        else stripLast cases
      
  in
    {model |snake = {cases = newCases, head= newHeadPosition, direction = model.snake.direction}}

setScore : Model -> Model
setScore model = 
    if isSnakeEatApple model then
      {model | score = model.score + 100}
    else
      if isSnakeEatCherry model then
        {model | score = model.score + 100}
      else
        model

isSnakeEatApple : Model -> Bool
isSnakeEatApple model = model.snake.head == model.apple

isSnakeEatCherry : Model -> Bool
isSnakeEatCherry model = model.snake.head == model.cherry

collisionAvecLuiMeme : Model -> Bool
collisionAvecLuiMeme model = False
    -- List.member model.snake.head (List.drop 1 model.snake.cases)

collisionWithRandomWall : Model -> Bool
collisionWithRandomWall model =  List.member model.snake.head model.randomWall
    

randomPosition : Model -> Random.Generator ( Int, Int )
randomPosition model =
    if model.wallOn then 
      Random.pair (Random.int 1 (model.boardSize - 2)) (Random.int 1 (model.boardSize - 2))
    else 
      Random.pair (Random.int 0 (model.boardSize - 1)) (Random.int 0 (model.boardSize - 1))

movePositions : List Position -> Position -> List Position
movePositions positions newFirstPosition =
    newFirstPosition :: positions

getNewHeadPosition : Model -> Position -> List Position -> Direction -> Position
getNewHeadPosition model currentHead positions direction =
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
                    { position | y = modBy model.boardSize (position.y - 1) }

                Right ->
                    { position | x = modBy model.boardSize (position.x + 1) }

                Down ->
                    { position | y = modBy model.boardSize (position.y + 1) }

                Left ->
                    { position | x = modBy model.boardSize (position.x - 1) }

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
    index = List.length model.randomWall
  in
    if index < 10 then 
      ( model, Random.generate ExtraWallUpdate (randomPosition model)) 
      

    else if time_ - model.lastUpdate >= 100 then

      if model.gameOver then
        (model, Cmd.none)
      else if collisionWithRandomWall model && List.length model.snake.cases < 2 then 
        ({ model | gameOver = True }, Cmd.none)      
      else if model.wallOn && (hitTheWall model || hitTheWall model) then 
        ({ model | gameOver = True }, Cmd.none)
      else if isSnakeEatApple model then 
        (updateSnake model, Random.generate NewApplePosition (randomPosition model))
      else if isSnakeEatCherry model then 
        ({model | snake = (updateSnake model).snake, cherry = {x= -2 , y = -2}, tempsApparitionCherry = 500}, Cmd.none)
      else
        updateSnake model
          |> Setters.setTime time_
          |> Setters.setLastUpdate time_
          |> Update.none

      else if model.cooldownCherry == 6000 then
        if model.cherry == {x= -1 , y = -1} then 
          (model, Random.generate NewCherryPosition (randomPosition model))
        else
          if model.tempsAvantAugmentationScore == 50 then
            if model.tempsApparitionCherry /= 500 then
              ({model | tempsApparitionCherry = model.tempsApparitionCherry + 1, tempsAvantAugmentationScore = 0, score = model.score + 10}, Cmd.none)
            else
              ({model | tempsApparitionCherry = 0, cooldownCherry = 0, cherry = {x= -1 , y = -1}, tempsAvantAugmentationScore = 0, score = model.score + 10}, Cmd.none)
          else
            if model.tempsApparitionCherry /= 500 then
              ({model | tempsApparitionCherry = model.tempsApparitionCherry + 1, tempsAvantAugmentationScore = model.tempsAvantAugmentationScore + 1}, Cmd.none)
            else
              ({model | tempsApparitionCherry = 0, cooldownCherry = 0, cherry = {x= -1 , y = -1}, tempsAvantAugmentationScore = model.tempsAvantAugmentationScore + 1}, Cmd.none)


      else if model.cooldownCherry /= 6000 then
        if model.tempsAvantAugmentationScore /= 50 then
          ({model | cooldownCherry = model.cooldownCherry + 1, tempsAvantAugmentationScore = model.tempsAvantAugmentationScore + 1}, Cmd.none)
        else if model.tempsAvantAugmentationScore == 50 then
          ({model | cooldownCherry = model.cooldownCherry + 1, tempsAvantAugmentationScore = 0, score = model.score + 10}, Cmd.none)
        else
          ({model | cooldownCherry = model.cooldownCherry + 1}, Cmd.none)        
    
    else
        time_
        |> Setters.setTimeIn model
        |> Update.none 

{-| Main update function, mainly used as a router for subfunctions -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateBoardSize boardSize ->
        ({ model | boardSize = boardSize, snake = { cases = [{x=boardSize//2,y=boardSize//2}] , direction = model.snake.direction, head = model.snake.head } }, Cmd.none)
    ToggleGameLoop -> toggleGameLoop model
    KeyDown key -> keyDown key model
    NextFrame time -> nextFrame time model
    NewApplePosition ( x, y ) ->
      if List.member {x=x,y=y} model.randomWall then
      (updateSnake model, Random.generate NewApplePosition (randomPosition model))
      else
      ( { model | apple = { x = x, y = y } }, Cmd.none )
    NewCherryPosition ( x, y ) ->
      if List.member {x=x,y=y} model.randomWall then
        (model, Random.generate NewCherryPosition (randomPosition model) )
      else
        ( { model | cherry = { x = x, y = y } }, Cmd.none )
    WallIsChecked  isChecked ->
      if isChecked == True then 
        ({ model | wallOn = False }, Cmd.none )
      else 
        ({ model | wallOn = True }, Cmd.none )
    RandomWallIsChecked  isChecked ->
      if isChecked == True then 
        ({ model | randomWallOn = False }, Cmd.none )
      else 
        ({ model | randomWallOn = True }, Cmd.none )
    ExtraWallUpdate (x,y) -> --dans le cas ou un utilisateur décide de générer un mur pendant le jeu
      if List.member {x=model.apple.x,y=model.apple.y} model.randomWall || List.member {x=model.cherry.x,y=model.cherry.y} model.randomWall then
        ( { model | randomWall = model.randomWall  }, Cmd.none )
      else 
        ( { model | randomWall = model.randomWall ++ [{ x = x, y = y }]  }, Cmd.none )

{-| Manage all your view functions here. -}
cell : Bool -> Model-> Int -> Int -> Html msg
cell active model x y = 
  
  let class = if active then "cell active" 
            else if model.apple.x == x && model.apple.y == y then "apple" 
            else if model.wallOn && (y == 0 || x ==0 || y == model.boardSize-1 || x ==model.boardSize-1) then "wall"
            else if model.randomWallOn && List.member {x=x,y=y} model.randomWall then "wall"
            else if model.cherry.x == x && model.cherry.y == y then "cherry"  else "cell" 
  in
  Html.div [ Attributes.class class ] []

movingSquare : Model -> Html msg
movingSquare model =
  Html.div [  if model.boardSize == 40 then Attributes.class "grid" else Attributes.class "grid20"]
    (List.concat 
      (List.indexedMap (\y elem ->
        List.indexedMap (\x _ ->
          (cell (List.member {x=x, y=y} model.snake.cases)) model x y
        )
        elem
      ) 
        (List.repeat model.boardSize (List.repeat model.boardSize 0))
      )
    )


actualTime : Model -> Html msg
actualTime model =
  Html.div [ Attributes.class "actual-time" ]
    [ 
      Html.text "temps actuel : "
        , model.time
        |> String.fromInt
        |> Html.text
        |> List.singleton
        |> Html.code []
      , Html.text "temps avant apparition de la cerise : "
        , (6000 - model.cooldownCherry)
        |> String.fromInt
        |> Html.text
        |> List.singleton
        |> Html.code []
      , Html.text "temps restant avant réinitialisation de la cerise : "
        , (500 - model.tempsApparitionCherry)
        |> String.fromInt
        |> Html.text
        |> List.singleton
        |> Html.code []
      , Html.text "temps restant avant augmentation du score : "
        , (50 - model.tempsAvantAugmentationScore)
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
    , if model.gameOver then h1 [] [ Html.text "Game Over!" ] else h1 [] [ Html.text (String.append "score : " (String.fromInt model.score))]
    ]

updateWallOptions : Model -> Bool -> Model
updateWallOptions model isChecked= 
  {model | wallOn = isChecked}

gameOptions : Model -> Html Msg
gameOptions model =
    Html.div[]
    [
      if  model.gameStarted then 
        Html.div [] []
      else
        Html.div []
        [
          Html.label [] [Html.text "Wall on"]
          ,Html.input [ Attributes.type_ "checkbox", Attributes.checked model.wallOn, onClick(WallIsChecked model.wallOn)  ] []
        ],
        Html.div []
        [
          Html.label [] [Html.text "Random wall on"]
          ,Html.input [ Attributes.type_ "checkbox", Attributes.checked model.randomWallOn, onClick(RandomWallIsChecked model.randomWallOn)  ] []
        ]
        , 
      if List.length model.snake.cases > 2  then 
        Html.div [] []
      else if  model.gameStarted then 
        Html.div [] []
      else 
        Html.div []
      [
          Html.div []
          [
            Html.p [] [Html.text "Grid size: "]
            ,Html.button [ onClick(UpdateBoardSize 40)  ] [ Html.text "40"]
            ,Html.button [ onClick(UpdateBoardSize 20) ] [Html.text "20"]
          ]

      ]
    ]

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Html.main_ []
    [ 
      Html.img [ Attributes.src "/logo.svg" ] []
    , explanations model
    , gameOptions model
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
