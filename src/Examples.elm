module Examples exposing (base,loop,shift,wait,catch)

{- Example layers that exhibit simple behaviors such as "shift",
sending events back through the layer stack, sending delayed events,
"catching" events and emitting them as a group, etc -}

import Layer exposing (..)
import Event exposing (Ctrl(..),Dest(..),intToCtrl)
import Error exposing (Error(..))
import Utils exposing (const,zip)
import Dict

base : Layer
base =
  let simple : Int -> Int
      simple i = if i == 8 then 9 else i
  in mkSimple simple

loop : Layer
loop =
  let loop_ : Int -> List Ctrl
      loop_ c = if c == 7 then [Ctrl In 0 8] else [intToCtrl c]
  in toLayer (\i s -> (loop_ i,s)) nonePrism

shift : Layer
shift =
  let shift_ : Int -> Bool -> (List Ctrl,Bool)
      shift_ i b =
        case (i,b) of
          (6,_) -> ([],not b)
          (_,True) -> ([intToCtrl 3],b)
          (_,False) -> ([intToCtrl i],b)
  in toLayer shift_ togglePrism

catch : Layer
catch =
  let catch_ : Int -> (Bool,List Ctrl) -> (List Ctrl,(Bool,List Ctrl))
      catch_ i (b,cs) =
        case (b,i) of
          (True,5) -> (cs, (False,[]))
          (True,_) -> ([], (True,((intToCtrl i)::cs)))
          (False,5) -> ([], (True,[]))
          (False,_) -> ([intToCtrl i],(False,[]))
  in toLayer catch_ catchPrism

wait : Layer
wait =
  let wait_ : Int -> List Ctrl
      wait_ i = if i == 9 then [Ctrl Forward 1000 i] else [intToCtrl i]
 in toLayer (\i s -> (wait_ i,s)) nonePrism

isPress : Int -> Bool
isPress i = modBy 2 i == 0

codes : List Int
codes = List.range 65 90

chars : List Char
chars = List.map Char.fromCode codes

codesByChar = Dict.fromList (zip chars codes)
charsByCode = Dict.fromList (zip codes chars)

charToCode : Char -> Maybe Int
charToCode c = Dict.get c codesByChar

codeToChar : Int -> Maybe Char
codeToChar i = Dict.get i charsByCode

tapNextReleaseA : Int -> (Bool,List Int) -> (List Ctrl, (Bool,List Int))
tapNextReleaseA i (isHold,xs) =
-- b corresponds to whether or not map is active
  if not (List.member 0 xs) then
      if i == 0 then
          ([],(False,[i]))
        else
          ([intToCtrl i],(False,[]))
    else
      if not isHold then
          if isPress i then
              ([],(False,i::xs))
            else
              if i == 1 then
                  (List.map intToCtrl (i::xs),(False,[]))
                else
                  if List.member (i-1) xs then
                      let (zero,rest) = List.partition ((==) 0) (i::xs) in
                      (List.map intToCtrl (List.append (List.map ((*) 3) rest) zero)
                      ,(True,zero))
                    else
                      ([intToCtrl i],(False,xs))
        else
          if i == 1 then
              ([intToCtrl i],(False,[]))
            else
              ([intToCtrl <| ((*) 3) i],(True,xs))

tnrA = toLayer tapNextReleaseA tnrPrism

{-
-- todo mkTapNextRelease

-- next we've got to make this more general: mkTapNextRelease, isPress, isRelease, etc

-- let's say that even integers x represent key presses, and each x+1 represents
-- release of the key of which x represents a press. Let's say 0 and 1 represent
-- press and release of key A

isPress : Ctrl -> Bool
isPress c = modBy 2 (getInt c) == 0

asciis : List Int
asciis = List.range 65 74

chars : List Char
chars = List.map Char.fromCode codes

codes : List Int
codes = List.map (\i -> i - 65) asciis

zip : List a -> List b -> List (a,b)
zip xs ys = List.map2 Tuple.pair xs ys

codesByChar = Dict.fromList (zip chars codes)
charsByCode = Dict.fromList (zip codes chars)

charToCode : Char -> Maybe Int
charToCode c = Dict.get c codesByChar

codeToChar : Int -> Maybe Char
codeToChar i = Dict.get i charsByCode

type Event = Press Char | Release Char

intToEvent : Int -> Maybe Event
intToEvent i =
  -- Maybe.map (\k -> if isPress i then Press c else Release c) (codeToChar i//2)
    case codeToChar i//2 of
        Just c -> if modBy 2 i == 0 then Press c else Release c
        Nothing -> Nothing 

eventToInt : Event -> Maybe Int
eventToInt e =
  case charToCode e of
-- the explicit syntax of Tuples feels brittle to refactoring. I
-- should choose to make use of the state monad or type alias the
-- tuple and use a value constructor instead of the parentheses and
-- comma

type TNRActive = On | Off
type TNRAction = Hold | Unsure

boolToTNRActive : Bool -> TNR Active
boolToTNRActive b = if b then On else Off

boolToTNRAction : Bool -> TNR Active
boolToTNRAction b = if b then Hold else Unsure

-- the reason I have 8 cases to kmonad's 4 is that I manually track
-- whether or not the layer is active or not! if it's a button, it can
-- match its own release and it doesn't need Off/On--if we're
-- receiving events, we assume we're on.

-- the layer-state abstraction isn't quite right for TNR!
tapNextReleaseA : Layer
tapNextReleaseA c s =
  let myMap : Event -> Event
      myMap e = Event Press 'Z'
      tnr : Event -> (TNRActive,TNRAction,List Ctrl) -> (List Event, (TNRActive,TNRAction,List Ctrl)
      tnr e (active?,action,es) =
       case (active?,action,e) ofn
        (Off,_,Event Press 'A') ->       ([],TNR On Unsure [])
        (Off,_,_)                ->      ([e],TNR Off Unsure [])
        (On,Hold,Event Release 'A') ->   ([e],TNR Off Unsure [])
        (On,Hold,_) ->                   ([myMap e],TNR On Hold [])
        (On,Unsure,Event Press _) ->     ([],TNR False (e::es))
        (On,Unsure,Event Release 'A') -> (List.map myMap es,TNR On Hold [])
        (On,Unsure,Event Release _) ->   
          if List.member (Event Release c) es then (es,TNR Off Unsure [])
          else ([e],TNR On Unsure [])
  in
  toLayer (ctrlToEvent >> tnr >> (\(es,s) -> (map eventToInt es,s))) tnrPrism
-}
