module Layer exposing (LState(..),Layer,mkSimple,toLayer,nonePrism,togglePrism)

import Either
import Event exposing (Ctrl,intToCtrl)
import Monocle.Prism exposing (Prism)
import Utils exposing (const)
import Error exposing (Error)

type LState
    = None
    | Toggle Bool
    | Pool (List Ctrl)
    | Catch Bool (List Ctrl)
    | TNR Bool (List Ctrl)

-- NOTE: each instance here could be made cleaner if Elm supported
-- templating; moreover, we might even be able to replace all of these
-- definitions somehow...
nonePrism : Prism LState ()
nonePrism = Prism (\x -> case x of
                              None -> Just ()
                              _ -> Nothing)
                  (const None)

togglePrism : Prism LState Bool
togglePrism = Prism (\x -> case x of
                              Toggle b -> Just b
                              _ -> Nothing)
                    Toggle

-- If we implement Layer with an StateEither monad, would it be an
-- instance of an Applicative? A function (Int -> List Ctrl) could be
-- lifted to type Layer
type alias Layer = Int -> LState -> Either.Either Error (List Ctrl, LState)

lift1 : (Int -> Int) -> (Int -> List Ctrl)
lift1 f = f >> intToCtrl >> List.singleton

lift2 : (Int -> List Ctrl) -> (Int -> a -> (List Ctrl,a))
lift2 f = \i s -> (f i,s)

-- below... should be using >>= ??????
-- nope, it gets complicated:
--   \c s -> f s |> Either.andThen (\x -> let (xs,ss) = (l c x) in  ......
toLayer : (Int -> a -> (List Ctrl,a)) -> Monocle.Prism.Prism LState a ->
          (Int -> LState -> Either.Either Error (List Ctrl,LState))
toLayer l p =
  \c s -> case p.getOption s of
              -- Just x -> Either.Right (Tuple.mapSecond p.reverseGet (l c x))
              Just x -> let (xs,ss) = (l c x) in Either.Right (xs,p.reverseGet ss)
              Nothing -> Either.Left Error.UnexpectedLState

mkSimple : (Int -> Int) -> Layer
mkSimple f = toLayer (lift1 f |> lift2) nonePrism


{-
-- todo mkTapNextRelease

-- next we've got to make this more general: mkTapNextRelease, isPress, isRelease, etc

-- let's say that even integers x represent key presses, and each x+1 represents
-- release of the key of which x represents a press. Let's say 0 and 1 represent
-- press and release of key A

isPress : Ctrl -> Bool
isPress c = modBy 2 (getInt c) == 0

isDown : LState -> Bool
isDown s =
 case s of
  TNR _ xs -> List.member 0 (List.map getInt xs)
  _ -> False

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
tapNextReleaseA : Ctrl -> LState -> Either.Either Error (List Ctrl, LState)
tapNextReleaseA c s =
 case s of
  TNR isHold xs -> Either.Right 
   (case (boolToTNRActive (isDown (TNR isHold xs)),
         boolToTNRAction isHold,
         intToEvent (getInt c)) of
       (Off,_,Event Press 'A') ->               ([],TNR False [c])
       (Off,_,_)                ->              ([c],TNR False [])
       (On,Hold,Event Release 'A') ->           ([c],TNR False [])
       (On,Hold,_) ->                           ([map (\z -> 3*z) c],TNR True xs)
       (On,Unsure,Event Press _) ->             ([],TNR False (c::xs))
       (On,Unsure,Event Release 'A') ->         (c::xs,TNR False [])
       (On,Unsure,_) ->        ([c],TNR False [])
         if List.member ((getInt c)-1) (List.map getInt xs) then
             let (zero,rest) = List.partition (\x -> getInt x == 0) (c::xs) in
             (List.append (List.map (\z -> map (\q -> 3*q) z) rest)
                                         zero
                          ,TNR True zero)
           else
             ([c],TNR False xs))
-}
