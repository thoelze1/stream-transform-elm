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


-- type alias InputType = Int
-- type alias OutputType = Maybe Int

port get : (Int -> msg) -> Sub msg

port put : Int -> Cmd msg

port loopback : Int -> Cmd msg

main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

-- before I can implement tap-next-release, I need to "catch" events
-- in a layer's state. that means first storing events as state and
-- second emitting multiple events somehow (either at once as a List
-- Int instead of Maybe Int, or in a "hacky" way by emitting control
-- events back to the layer with the stored values)

-- Ideas
-- 1 Construct an algebra of state operations and allow the layer to
--   emit a list of state operations
-- 2 Pass the entire state to the layer and allow direct manipulation
-- 3 Allow the layer to "replace itself." I think this didn't get me anywhere:
--   Event -> State -> (List Event, (Event -> State -> (List Event))

type alias LState = (Bool, List Int)
type alias Model = List (Int -> LState -> (Maybe Int,LState) , LState)

type alias Msg
    = Int


type alias Flags =
    ()

baseMap : Int -> LState -> (Maybe Int,LState)
baseMap i _ =
    case i of
        9 -> (Just 8,(False,[]))
        _ -> (Just i,(False,[]))

-- would be cooler if there was pttern matching on functions...
-- shiftLayer 6 b = (Nothing,not b)
-- and
-- shiftLayer i True = (Just 3,b)
-- and
-- shiftLayer i False = (Just i,b)
-- elm really is dumb haskell
shiftLayer : Int -> LState -> (Maybe Int,LState)
shiftLayer i (b,_) =
    if i == 6 then
        (Nothing,(not b,[]))
      else if b then
        (Just 3,(b,[]))
      else
        (Just i,(b,[]))

catchLayer : Int -> LState -> (Maybe Int,LState)
catchLayer i (b,xs) =
    if b then
        if i == 5 then
            (Just (List.foldl (+) 0 xs),(False,[]))
          else
            (Nothing,(True,i::xs))
      else
        if i == 5 then
            (Nothing,(True,[]))
          else
            (Just i,(False,[]))

-- tnr : Int -> Model
-- tnr i =
--   [ \x b -> case x of
--                i -> ]

init : Flags -> ( Model, Cmd Msg )
init _ =
    ( [ (baseMap , (False,[]))
      , (shiftLayer , (False,[]))
      , (catchLayer , (False,[]))
      ] , Cmd.none )

doOps : Maybe Int -> Model -> (Model, Maybe Int)
doOps i m =
  case m of
    [] -> ([],i)
    ((l,s)::rest) -> case i of
                       Nothing -> (m,Nothing)
                       Just x -> let
                                   (output,updatedState) = (l x s)
                                   (updatedRest,finalOutput) = doOps output rest
                                 in
                                 ((l,updatedState)::updatedRest,finalOutput)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        (newModel,output) = doOps (Just msg) model
    in
    case output of
      Nothing -> (newModel , Cmd.none)
      Just x ->  (newModel , put x)

subscriptions : Model -> Sub Msg
subscriptions _ =
    get identity
