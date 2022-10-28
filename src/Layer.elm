module Layer exposing (LState(..)
                      ,Layer
                      ,mkSimple
                      ,toLayer
                      ,nonePrism
                      ,togglePrism
                      ,catchPrism
                      ,tnrPrism)

import Event exposing (Ctrl,intToCtrl)
import Monocle.Prism exposing (Prism)
import Utils exposing (const)
import Error exposing (Error)

type LState
    = None
    | Toggle Bool
    | Pool (List Ctrl)
    | Catch Bool (List Ctrl)
    | TNR Bool (List Int)

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

catchPrism : Prism LState (Bool,List Ctrl)
catchPrism = Prism (\x -> case x of
                              Catch b cs -> Just (b,cs)
                              _ -> Nothing)
                   (\(b,cs) -> Catch b cs)

tnrPrism : Prism LState (Bool,List Int)
tnrPrism = Prism (\x -> case x of
                            TNR a b -> Just (a,b)
                            _ -> Nothing)
                 (\(a,b) -> TNR a b)

-- If we implement Layer with an StateEither monad, would it be an
-- instance of an Applicative? A function (Int -> List Ctrl) could be
-- lifted to type Layer
type alias Layer = Int -> LState -> Result Error (List Ctrl, LState)

lift1 : (Int -> Int) -> (Int -> List Ctrl)
lift1 f = f >> intToCtrl >> List.singleton

lift2 : (Int -> List Ctrl) -> (Int -> a -> (List Ctrl,a))
lift2 f = \i s -> (f i,s)

-- below... should be using >>= ??????
-- nope, it gets complicated:
--   \c s -> f s |> Either.andThen (\x -> let (xs,ss) = (l c x) in  ......
toLayer : (Int -> a -> (List Ctrl,a)) -> Monocle.Prism.Prism LState a ->
          (Int -> LState -> Result Error (List Ctrl,LState))
toLayer l p =
  \c s -> case p.getOption s of
              -- Just x -> Either.Right (Tuple.mapSecond p.reverseGet (l c x))
              Just x -> let (xs,ss) = (l c x) in Ok (xs,p.reverseGet ss)
              Nothing -> Err Error.UnexpectedLState

mkSimple : (Int -> Int) -> Layer
mkSimple f = toLayer (lift1 f |> lift2) nonePrism
