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

port error : String -> Cmd msg

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


-- could theoretically create 3-state algebraic type for TNR to replace the 2 bools
type LState
    = None
    | Toggle Bool
    | Pool (List Ctrl)
    | Catch Bool (List Ctrl)
    | TNR Bool (List Ctrl)

type alias Model = List (Layer , LState)

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

-- todo: use maybe to signal errors

baseMap : Ctrl -> LState -> Maybe (List Ctrl,LState)
baseMap c s =
    case s of
        None -> Just ([map simpleMap c],None)
        _ -> Nothing

loopMap : Ctrl -> LState -> Maybe (List Ctrl,LState)
loopMap c s =
  case s of
    None -> case getInt c of
              7 -> Just ([Ctrl In 0 8],None)
              _ -> Just ([c],None)
    _ -> Nothing

shift : Ctrl -> Bool -> (List Ctrl,Bool)
shift c b =
  if getInt c == 6 then
      ([],b)
    else if b then
      ([map (const 3) c],b)
    else
      ([c],b)

toLayer : (Ctrl -> a -> (List Ctrl,a)) -> (LState -> Maybe a) -> (a -> Maybe LState) ->
          Layer
toLayer l f g =
  \c s -> case f s of
              Just x -> let (xs,ss) = (l c x) in
                        case g ss of
                            Just ls -> Just (xs,ls)
                            Nothing -> Nothing
              Nothing -> Nothing

shiftLayer : Ctrl -> LState -> Maybe (List Ctrl,LState)
shiftLayer = toLayer shift (\s -> case s of
                                     Toggle b -> Just b
                                     _ -> Nothing)
                           (\b -> Just (Toggle b))

-- would be cooler if there was pttern matching on functions...
-- shiftLayer 6 b = (Nothing,not b)
-- and
-- shiftLayer i True = (Just 3,b)
-- and
-- shiftLayer i False = (Just i,b)
-- elm really is dumb haskell
-- shiftLayer : Ctrl -> LState -> Maybe (List Ctrl,LState)
-- shiftLayer c s =
-- case s of
--    Toggle b -> if getInt c == 6 then
--                    Just ([],Toggle (not b))
--                  else if b then
--                    Just ([map (const 3) c],Toggle b)
--                  else
--                    Just ([c],Toggle b)
--    _ -> ([],None)

catchLayer : Ctrl -> LState -> Maybe (List Ctrl,LState)
catchLayer c s =
 case s of
  Catch b xs ->
    if b then
        if getInt c == 5 then
            Just (xs, Catch False [])
          else
            Just ([], Catch True (c::xs))
      else
        if getInt c == 5 then
            Just ([], Catch True [])
          else
            Just ([c], Catch False [])
  _ -> Nothing

-- this too should simply be a map from ctrl value to list of control values,
-- with another function that turns that map into a full layer
waitMap : Ctrl -> LState -> Maybe (List Ctrl, LState)
waitMap (Ctrl d w i) s =
 case s of
  None ->
   case i of
     9 -> Just ([Ctrl d 1000 i],None)
     _ -> Just ([Ctrl d w i],None)
  _ -> Nothing

-- next we've got to make this more general: mkTapNextRelease, isPress, isRelease, etc

isPress : Ctrl -> Bool
isPress c = modBy 2 (getInt c) == 0

isDown : LState -> Bool
isDown s =
 case s of
  TNR _ xs -> List.member 0 (List.map getInt xs)
  _ -> False

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
tapNextReleaseA : Ctrl -> LState -> Maybe (List Ctrl, LState)
tapNextReleaseA c s =
 case s of
  TNR isHold xs ->
-- b corresponds to whether or not map is active
   if not (isDown (TNR isHold xs)) then
       if getInt c == 0 then
           Just ([],TNR False [c])
         else
           Just ([c],TNR False [])
     else
       if not isHold then
           if isPress c then
               Just ([],TNR False (c::xs))
             else
               if getInt c == 1 then
                   Just (c::xs,TNR False [])
                 else
                   if List.member ((getInt c)-1) (List.map getInt xs) then
                       let (zero,rest) = List.partition (\x -> getInt x == 0) (c::xs) in
                       Just (List.append (List.map (\z -> map (\q -> 3*q) z) rest)
                                         zero
                       ,TNR True zero)
                     else
                       Just ([c],TNR False xs)
         else
           if getInt c == 1 then
               Just ([c],TNR False [])
             else
               Just ([map (\z -> 3*z) c],TNR True xs)
  _ -> Just ([],None)

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

type alias Layer = Ctrl -> LState -> Maybe (List Ctrl, LState)

-- myMap : List Layer
-- myMap = [ tapNextReleaseA ]

init : Flags -> ( Model, Cmd Msg )
init _ =
    ( [ (tapNextReleaseA , TNR False []) ] , Cmd.none )


-- replace Maybe with Either plus meaningful error message

-- i think doOps should manage destination and waiting of events;
-- each layer therefore should only care about the actual event it
-- has recevied, because it should have been provided the event at the
-- correct time (and should be the correct recipient) of the event

-- doOps : List Ctrl -> State Model List Ctrl
doOps : List Ctrl -> Model -> Maybe (Model, List Ctrl)
doOps i m =
  case m of
    [] -> Just ([],i)
    ((l,s)::rest) ->
      case i of
        [] -> Just (m,[])
        (x::xs) ->
         case (l x s) of
           Nothing -> Nothing
           Just (output,newLayerState) ->
            case doOps xs ((l,newLayerState)::rest) of
             Nothing -> Nothing
             Just (newModelState,finalOutput1) ->
              case newModelState of
               [] -> Nothing --unique error
               ((ya,yb)::ys) ->
                case doOps output ys of
                 Nothing -> Nothing
                 Just (newRestState,finalOutput2) ->
                  Just ((l,yb)::newRestState,List.append finalOutput1
                                                         finalOutput2)

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
      case doOps [msg] model of
        Nothing -> (model,error "something went wrong")
        Just (newModel,output) ->
         case output of
          [] -> (newModel , put Nothing)
          _ -> (newModel , List.map cdispatch output |> Cmd.batch)

subscriptions : Model -> Sub Msg
subscriptions _ =
    get <| Ctrl Forward 0
