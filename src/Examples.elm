module Examples exposing (base,loop,shift,wait,catch)

{- Example layers that exhibit simple behaviors such as "shift",
sending events back through the layer stack, sending delayed events,
"catching" events and emitting them as a group, etc -}

import Layer exposing (..)
import Event exposing (Ctrl(..),Dest(..),ctrlToInt,ctrlMap,intToCtrl)
import Error exposing (Error(..))
import Utils exposing (const)

simple : Int -> Int
simple i = if i == 8 then 9 else i

base : Layer
base = mkSimple simple

loop_ : Int -> List Ctrl
loop_ c = if c == 7 then [Ctrl In 0 8] else [intToCtrl c]

loop : Layer
loop = toLayer (\i s -> (loop_ i,s)) nonePrism

shift_ : Int -> Bool -> (List Ctrl,Bool)
shift_ i b =
  case (i,b) of
    (6,_) -> ([],not b)
    (_,True) -> ([intToCtrl 3],b)
    (_,False) -> ([intToCtrl i],b)

shift : Layer
shift = toLayer shift_ togglePrism

catch_ : Int -> (Bool,List Ctrl) -> (List Ctrl,(Bool,List Ctrl))
catch_ i (b,cs) =
  case (b,i) of
    (True,5) -> (cs, (False,[]))
    (True,_) -> ([], (True,((intToCtrl i)::cs)))
    (False,5) -> ([], (True,[]))
    (False,_) -> ([intToCtrl i],(False,[]))

catch : Layer
catch = toLayer catch_ catchPrism

wait_ : Int -> List Ctrl
wait_ i = if i == 9 then [Ctrl Forward 1000 i] else [intToCtrl i]

wait : Layer
wait = toLayer (\i s -> (wait_ i,s)) nonePrism
