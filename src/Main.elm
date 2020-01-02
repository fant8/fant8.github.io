module Golems exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.App exposing (..)
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import String
import List exposing (..)
import Url
import Random
import Dict exposing (..)
import Tuple exposing (..)

main =
  Browser.element { init = init, update = update, subscriptions = \_ -> Sub.none, view = view }

-- initialize model with empty guess and generate code, number of guesses = 10

init () =
  ({ userGuess = []
   , numGuess = 10
   , hint = (0,0)
   , gameOver = False
   , seed = 20
   , code = codeGenerator 2 }
  , Cmd.none
  )


-- -- MODEL
type alias Code = List Element

type alias Model = {userGuess: Code, code : Code, numGuess: Int, gameOver: Bool, hint: (Int,Int), seed : Int}

type Element = Fire | Air | Water | Earth

type Msg = Input Element | Submit | Start | GotRandom Int

type alias Position = Int

listElements = [Fire, Air, Water, Earth]

-- function to randomly generate secret code
codeGenerator : Int -> List Element
codeGenerator seed =
  let
    randTuple = Random.step (Random.list 4 (Random.int 1 4)) (Random.initialSeed seed)
    randInt = Tuple.first randTuple
  in List.map (intToElement) randInt

-- maps 1-4 to element
intToElement : Int -> Element
intToElement n =
  case n of
    1 -> Fire
    2 -> Water
    3 -> Earth
    _ -> Air


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    -- if user clicks element button add elemenet to userGuess if userGuess < 4 elements
    Input element ->

      (if (length (model.userGuess) < 4) then
        { model | userGuess = model.userGuess ++ [element]}
        else model
      , Cmd.none
      )

    -- if user clicks submit
    -- generate hint and decrease number of guesses remaining
    -- change gameOver to True if userGuess is correct or no guesses remaining
    Submit ->
      ( if (model.gameOver == False) then
          if (model.code /= model.userGuess) then
            if (length (model.userGuess) == 4) then
              if (model.numGuess > 1) then
                { model | numGuess = model.numGuess - 1, hint = renderHint model.userGuess model.code, userGuess = [] }
                else { model | gameOver = True}
            else model
          else { model | gameOver = True}
        else model
      , Cmd.none
      )

    -- reset model
    Start ->
      ({model | code = codeGenerator model.seed, userGuess = [], numGuess = 10, hint = (0,0), gameOver = False}, Random.generate GotRandom (Random.int 1 1000000))

    -- give model random seed
    GotRandom n ->
      ({model | seed = n}, Cmd.none)


-- generate 2 ints for number of blues and number of reds
renderHint : Code -> Code -> (Int, Int)
renderHint guess answer =
  let

-- count number of elements in correct position
    numBlue =
            List.map2 (==) guess answer
                |> List.filter identity
                |> List.length

-- algorithm to count number of correct elements in wrong position
-- create two lists of the incorrect guesses from guess and answer
-- check if each element in the wrong guesses is a member of the answer without the correctly guessed elemenets
-- if it is, +1 and remove the first instance of the element from the answer without the correctly guessed elements, recurse using new answer list
-- if not, continue recursing through the rest of the guess 

    -- remove all elements in the right position from guess
    wrongGuesses : Code -> Code -> List Element
    wrongGuesses guesses answers =
      case (guesses, answers) of
        (g::gs, n::ns) ->
          if g /= n
            then g :: wrongGuesses gs ns
            else wrongGuesses gs ns
        _ ->
          []

    -- remove all elements correctly guesses from answer
    wrongAnswers guesses answers =
      case (guesses, answers) of
        (g::gs, n::ns) ->
          if g /= n
            then n :: wrongAnswers gs ns
            else wrongAnswers gs ns
        _ ->
          []

    -- remove first instance of element from a list
    remove a list =
      case list of
        x::xs ->
          if a == x
            then xs
            else x :: remove a xs
        [] ->
          []

   -- count number of reds
    numRed wrongs answers =
      case wrongs of
        w::ws ->
          if List.member w answers
            then 1 + numRed ws (remove w answers)
            else numRed ws answers
        [] ->
          0


  in (numBlue, numRed (wrongGuesses guess answer) <| wrongAnswers guess answer)

-- display hint in html message
showHint : (Int,Int) -> Html Msg
showHint (blue,red) =
     div []
       [ p [style "color" "blue", style "font-weight" "bold"] [Html.text ("Blue: " ++ String.fromInt (blue))]
       , br [] []
       , p [style "color" "red", style "font-weight" "bold"] [Html.text ("Red: " ++ String.fromInt (red))]
       ]

-- display number of guesses remaining
showNumGuesses : Int -> Html Msg
showNumGuesses a =
  div []
    [ Html.text ("Guesses Remaining: " ++ String.fromInt(a))]

-- show message if game is over
displayGameOver : Model -> Html Msg
displayGameOver model =
  let
    message =
      if (model.gameOver == True) then
        if (model.userGuess == model.code) then
          "WIN"
        else "GAME OVER"
      else ""
  in div []
      [ p [style "font-size" "30px", style "font-weight" ""] [Html.text (message)]]


-- VIEW
myStyle = [ style "text-align" "center"
          ,  style "font-family" "Courier"
          ]

view : Model -> Html Msg
view model =

  div myStyle
  -- header
    [ h1  [ style "font-weight" "bold"]   [ Html.text "Mastermind: Avatar Edition" ]
    , Html.img [src "src/aang.png"] []
    , br [] []

    -- game instructions
    , p [style "font-weight" "bold"] [Html.text ("HOW TO PLAY: ")]
    , p [] [Html.text ("Try to guess the secret code within 10 attempts. You'll get a hint of blues (number of elements in the correct position) and reds (number of correct elements in the wrong positon). Good luck!")]
    , br [] []

    -- buttons
    , button [ onClick (Start)] [Html.text "New Game"]
    , br [] []

    --element buttons
    , button [ onClick (Input Fire) ] [ Html.img [src "src/Fire.jpg", height 50, width 50] [] ]
    , button [ onClick (Input Air) ] [ Html.img [src "src/Air.jpg" , height 50, width 50] [] ]
    , button [ onClick (Input Water) ] [ Html.img [src "src/water.jpg", height 50, width 50] [] ]
    , button [ onClick (Input Earth) ] [ Html.img [src "src/earth.jpg", height 50, width 50 ] [] ]
    , br [] []
    , button [ onClick (Submit)] [Html.text "Submit"]
    , br [] []
    , br [] []
    , showHint(model.hint)
    , showNumGuesses (model.numGuess)
    , displayGameOver model
    -- display user guess
    , Html.text ("YOUR GUESS: " ++ Debug.toString model.userGuess)
    ]
