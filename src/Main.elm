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
import String

-- type alias InputType = Int
-- type alias OutputType = Maybe Int

port get : (Int -> msg) -> Sub msg

port put : Maybe Int -> Cmd msg

port loopback : Maybe Int -> Cmd msg

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

-- Now we can emit lists, but still it is good to loop events back,
-- which is already trivial to implement by adding a wrapper around
-- the event types

-- Ideas
-- 1 Construct an algebra of state operations and allow the layer to
--   emit a list of state operations
-- 2 Pass the entire state to the layer and allow direct manipulation
-- 3 Allow the layer to "replace itself." I think this didn't get me anywhere:
--   Event -> State -> (List Event, (Event -> State -> (List Event))

type alias LState = (Bool, List Ctrl)
type alias Model = List (Ctrl -> LState -> (List Ctrl,LState) , LState)

type alias Msg
    = Int

const : a -> b -> a
const x _ = x

type Ctrl
    = Emit Int
    | Loop Int
    | Blah Int

cget : Ctrl -> Int
cget c =
    case c of
        Emit i -> i
        Loop i -> i
        Blah i -> i

map : (Int -> Int) -> Ctrl -> Ctrl
map f c =
    case c of
        Emit i -> Emit (f i)
        Loop i -> Loop (f i)
        Blah i -> Blah (f i)

type alias Flags =
    ()

simpleMap : Int -> Int
simpleMap i =
    case i of
        8 -> 9
        _ -> i

baseMap : Ctrl -> LState -> (List Ctrl,LState)
baseMap c _ = ([map simpleMap c],(False,[]))

loopMap : Ctrl -> LState -> (List Ctrl,LState)
loopMap c s =
    case cget c of
        7 -> ([Loop 8],s)
        _ -> ([c],s)

-- would be cooler if there was pttern matching on functions...
-- shiftLayer 6 b = (Nothing,not b)
-- and
-- shiftLayer i True = (Just 3,b)
-- and
-- shiftLayer i False = (Just i,b)
-- elm really is dumb haskell
shiftLayer : Ctrl -> LState -> (List Ctrl,LState)
shiftLayer c (b,_) =
    if (cget c) == 6 then
        ([],(not b,[]))
      else if b then
        ([map (const 3) c],(b,[]))
      else
        ([c],(b,[]))

catchLayer : Ctrl -> LState -> (List Ctrl,LState)
catchLayer c (b,xs) =
    if b then
        if cget c == 5 then
            (xs,(False,[]))
          else
            ([],(True,c::xs))
      else
        if cget c == 5 then
            ([],(True,[]))
          else
            ([c],(False,[]))

-- tnr : Int -> Model
-- tnr i =
--   [ \x b -> case x of
--                i -> ]

init : Flags -> ( Model, Cmd Msg )
init _ =
    ( [ (baseMap , (False,[]))
      , (shiftLayer , (False,[]))
      , (catchLayer , (False,[]))
      , (loopMap , (False,[]))
      ] , Cmd.none )

doOps : List Ctrl -> Model -> (Model, List Ctrl)
doOps i m =
  case m of
    [] -> ([],i)
    ((l,s)::rest) ->
      case i of
        [] -> (m,[])
        (x::xs) ->
          let
            (output,newLayerState) = (l x s)
            (newModelState,finalOutput1) = doOps xs ((l,newLayerState)::rest)
            (newRestState,finalOutput2) = case newModelState of
                                            [] -> (newModelState,finalOutput1)
                                            (y::ys) -> doOps output ys
            finalLayerState = case newModelState of
                                [] -> s
                                ((ya,yb)::ys) -> yb
          in
          ((l,finalLayerState)::newRestState,
          List.append finalOutput1 finalOutput2)

cdispatch : Ctrl -> Cmd Msg
cdispatch c =
    case c of
        Emit i -> put (Just i)
        Blah i -> put (Just i)
        Loop i -> loopback (Just i)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        (newModel,output) = doOps [Blah msg] model
    in
    case output of
      [] -> (newModel , put Nothing)
      _ -> (newModel , List.map cdispatch output |> Cmd.batch)

subscriptions : Model -> Sub Msg
subscriptions _ =
    get identity
