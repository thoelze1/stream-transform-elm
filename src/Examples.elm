module Examples exposing (base,loop,shift,wait,catch)

import Layer exposing (Layer,LState(..),mkSimple,toLayer,togglePrism)
import Either exposing (Either(..))
import Event exposing (Ctrl(..),Dest(..),ctrlToInt,ctrlMap,intToCtrl)
import Error exposing (Error(..))
import Utils exposing (const)

simple : Int -> Int
simple i =
    case i of
        8 -> 9
        _ -> i

base : Layer
base = mkSimple simple

loop : Ctrl -> LState -> Either.Either Error (List Ctrl,LState)
loop c s =
  case s of
    None -> case ctrlToInt c of
              7 -> Either.Right ([Ctrl In 0 8],None)
              _ -> Either.Right ([c],None)
    _ -> Either.Left UnexpectedLState

shift_ : Int -> Bool -> (List Ctrl,Bool)
shift_ i b =
  if i == 6 then
      ([],not b)
    else if b then
      ([intToCtrl 3],b)
    else
      ([intToCtrl i],b)


shift : Int -> LState -> Either.Either Error (List Ctrl,LState)
shift = toLayer shift_ togglePrism

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

catch : Int -> LState -> Either.Either Error (List Ctrl,LState)
catch i s =
 case s of
  Catch b xs ->
    if b then
        if i == 5 then
            Either.Right (xs, Catch False [])
          else
            Either.Right ([], Catch True ((intToCtrl i)::xs))
      else
        if i == 5 then
            Either.Right ([], Catch True [])
          else
            Either.Right ([intToCtrl i], Catch False [])
  _ -> Either.Left UnexpectedLState

-- this too should simply be a map from ctrl value to list of control values,
-- with another function that turns that map into a full layer
wait : Int -> LState -> Either.Either Error (List Ctrl, LState)
wait i s =
 case s of
  None ->
   case i of
     9 -> Either.Right ([Ctrl Forward 1000 i],None)
     _ -> Either.Right ([intToCtrl i],None)
  _ -> Either.Left UnexpectedLState


-- next we've got to make this more general: mkTapNextRelease, isPress, isRelease, etc

-- let's say that even integers x represent key presses, and each x+1 represents
-- release of the key of which x represents a press. Let's say 0 and 1 represent
-- press and release of key A
