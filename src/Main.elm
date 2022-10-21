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
import Task
import Process
import Char
import Tuple
import Dict

-- type alias InputType = Int
-- type alias OutputType = Maybe Int

port get : (Int -> msg) -> Sub msg

port put : Maybe Int -> Cmd msg

port loopback : Maybe Int -> Cmd msg

port wait : Maybe Int -> Cmd msg

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
    = Ctrl

const : a -> b -> a
const x _ = x

type Dest = Forward | Out | In
type alias Wait = Int

-- wait time is orthogonal to destination, so maybe structure
-- should be different here...
type Ctrl = Ctrl Dest Wait Int

getInt : Ctrl -> Int
getInt (Ctrl d w i) = i

map : (Int -> Int) -> Ctrl -> Ctrl
map f (Ctrl d w i) = Ctrl d w (f i)

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
    case getInt c of
        7 -> ([Ctrl In 0 8],s)
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
    if getInt c == 6 then
        ([],(not b,[]))
      else if b then
        ([map (const 3) c],(b,[]))
      else
        ([c],(b,[]))

catchLayer : Ctrl -> LState -> (List Ctrl,LState)
catchLayer c (b,xs) =
    if b then
        if getInt c == 5 then
            (xs,(False,[]))
          else
            ([],(True,c::xs))
      else
        if getInt c == 5 then
            ([],(True,[]))
          else
            ([c],(False,[]))

-- this too should simply be a map from ctrl value to list of control values,
-- with another function that turns that map into a full layer
waitMap : Ctrl -> LState -> (List Ctrl, LState)
waitMap (Ctrl d w i) s =
  case i of
    9 -> ([Ctrl d 1000 i],s)
    _ -> ([Ctrl d w i],s)

-- next we've got to make this more general: mkTapNextRelease, isPress, isRelease, etc

isPress : Ctrl -> Bool
isPress c = modBy 2 (getInt c) == 0

isDown : LState -> Bool
isDown (_,xs) = List.member 0 (List.map getInt xs)

codes : List Int
codes = List.range 65 90

chars : List Char
chars = List.map Char.fromCode codes

zip : List a -> List b -> List (a,b)
zip xs ys = List.map2 Tuple.pair xs ys

codesByChar = Dict.fromList (zip chars codes)
charsByCode = Dict.fromList (zip codes chars)

charToCode : Char -> Maybe Int
charToCode c = Dict.get c codesByChar

codeToChar : Int -> Maybe Char
codeToChar i = Dict.get i charsByCode

-- i need to rewrite LState to be algebraic and then customize the tnr datatype
-- to include a second boolean for whether or not the key has been pressed yet

-- let's say that even integers x represent key presses, and each x+1 represents
-- release of the key of which x represents a press. Let's say 0 and 1 represent
-- press and release of key A
tapNextReleaseA : Ctrl -> LState -> (List Ctrl, LState)
tapNextReleaseA c (isHold,xs) =
-- b corresponds to whether or not map is active
  if not (isDown (isHold,xs)) then
      if getInt c == 0 then
          ([],(False,[c]))
        else
          ([c],(False,[]))
    else
      if not isHold then
          if isPress c then
              ([],(False,c::xs))
            else
              if getInt c == 1 then
                  (c::xs,(False,[]))
                else
                  if List.member ((getInt c)-1) (List.map getInt xs) then
                      let (zero,rest) = List.partition (\x -> getInt x == 0) (c::xs) in
                      (List.append (List.map (\z -> map (\q -> 3*q) z) rest)
                                   zero
                      ,(True,zero))
                    else
                      ([c],(False,xs))
        else
          if getInt c == 1 then
              ([c],(False,[]))
            else
              ([map (\z -> 3*z) c],(True,xs))

--  if isPress c then
--      if getInt c == 0 then
--          ([],(False,[c]))
--        else
--          if isHold then
--              ([map (\z -> 3*z) c],(isHold,xs))
--            else
--              ([],(False,c::xs))
--    else
--      if isDown (isHold,down) then
--          if List.member ((getInt c)-1) (map getInt xs) then
--              (c::xs,(True,[]))
--            else
--              ([c],(False,[]))

-- b corresponds to whether or not map is active
--  if isDown (b,xs) then
--      if isPress c then
--          if isHold then ([map (\z -> 3*z) c],(True,xs)) else ([],(False,c::xs))
--        else
--          if List.member ((getInt c)-1) (map getInt xs) then (c::xs,(True,[]))
--    else if getInt c == 0
--      ([],(False,[c]))
--    else
--      ([c],(False,[]))

type alias Layer = Ctrl -> LState -> (List Ctrl, LState)

myMap : List Layer
myMap = [ tapNextReleaseA ]
        
init : Flags -> ( Model, Cmd Msg )
init _ =
    ( List.map (\l -> (l , (False,[]))) myMap, Cmd.none )


-- i think doOps should manage destination and waiting of events;
-- each layer therefore should only care about the actual event it
-- has recevied, because it should have been provided the event at the
-- correct time (and should be the correct recipient) of the event
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

-- there are actually 3 ways to do loopback:
-- 1 using ports with a JS listener
-- 2 through the Elm runtime with a Cmd Msg
-- 3 recursion
-- 4 through the Elm runtime using the model

-- i think I can remove the loopback port...
-- the following map belongs as data (a list of tuples?), not as code
destMap : Dest -> (Maybe Int -> Cmd msg)
destMap d =
    case d of
        Forward -> put
        Out -> put
        In -> loopback

-- Ctrl should actually have two value constructors, one with time and one without
cdispatch : Ctrl -> Cmd Ctrl
cdispatch (Ctrl d w i) =
  if w > 0 then
      delay w i
    else
      destMap d <| Just i

delay : Int -> Int -> Cmd Msg
delay i msg =
  Process.sleep (toFloat i)
  |> Task.perform (\_ -> (Ctrl Out 0 msg))
  
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Ctrl Out w i -> (model,cdispatch msg)
    Ctrl In w i -> (model,cdispatch msg)
    Ctrl Forward w i ->
      let
        (newModel,output) = doOps [msg] model
      in
      case output of
        [] -> (newModel , put Nothing)
        _ -> (newModel , List.map cdispatch output |> Cmd.batch)

subscriptions : Model -> Sub Msg
subscriptions _ =
    get <| Ctrl Forward 0
