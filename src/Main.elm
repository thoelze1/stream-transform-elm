port module Main exposing (main)

{-|  A simple Platform.worker program with
a simple command-line interface:

   `$ sh make.sh `                      -- (1)
   `$ chmod u+x cli; alias cli='./cli'` -- (2)
   `$ cli 77`                           -- (3)
     `232`

1) Compile Main.elm to `./run/main.js` and
copy `src/cli.js` to `./run/cli.js`

2) Make `cli` executable and make an alias for it
to avoid awkward typing.

3) Try it out.  The program `cli.js` communicates
with runtime for the `Platform.worker` program.
The worker accepts input, computes some output,
and send the output back through ports.

To do something more interesting, replace
the `transform` function in `Main.elm`.

-}

import Platform exposing (Program)


type alias InputType = Int
type alias OutputType = Maybe Int

port get : (InputType -> msg) -> Sub msg

port put : OutputType -> Cmd msg

port loopback : OutputType -> Cmd msg

main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { layers : List (Int -> Bool -> (Maybe Int,Bool) , Bool)
    }


type Msg
    = Input Int


type alias Flags =
    ()

baseMap : Int -> Bool -> (Maybe Int,Bool)
baseMap i _ =
    case i of
        9 -> (Just 8,False)
        _ -> (Just i,False)

-- would be cooler if there was pttern matching on functions...
-- shiftLayer 6 b = (Nothing,not b)
-- and
-- shiftLayer i True = (Just 3,b)
-- and
-- shiftLayer i False = (Just i,b)
-- elm really is dumb haskell
shiftLayer : Int -> Bool -> (Maybe Int,Bool)
shiftLayer i b =
    if i == 6 then
        (Nothing,not b)
      else if b then
        (Just 3,b)
      else
        (Just i,b)

init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { layers = [ (baseMap , False)
                , (shiftLayer , False)
                ]
      }
      , Cmd.none )

doOps : Maybe Int -> Model -> (Model, Maybe Int)
doOps i m =
  case m.layers of
    [] -> ({ layers = [] },i)
    ((l,s)::rest) -> case i of
                       Nothing -> (m,Nothing)
                       Just x -> let
                                   (output,updatedState) = (l x s)
                                   (updatedRest,finalOutput) = doOps output
                                                                     { layers = rest }
                                 in
                                 ({ layers = (l,updatedState) :: updatedRest.layers }
                                  ,finalOutput)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Input i ->
      let
          (newModel,output) = doOps (Just i) model
      in
      (newModel, put output)

subscriptions : Model -> Sub Msg
subscriptions _ =
    get Input
