module Event exposing  (Ctrl(..),Dest(..),dispatch,ctrlToInt,intToCtrl,ctrlMap)

import Process
import Task
import Ports exposing (put,loopback)

-- TODO Should Ctrl have two value constructors; one w/ time and one w/out?
type Ctrl = Ctrl Dest Wait Int
type Dest = Forward | Out | In
type alias Wait = Int

-- TODO implement this as lookup from a list (data, not code)
destMap : Dest -> (Maybe Int -> Cmd msg)
destMap d =
    case d of
        Forward -> put
        Out -> put
        In -> loopback

dispatch : Ctrl -> Cmd Ctrl
dispatch (Ctrl d w i) =
  if w > 0 then
      delay w i
    else
      destMap d <| Just i

delay : Int -> Int -> Cmd Ctrl
delay i msg =
  Process.sleep (toFloat i)
  |> Task.perform (\_ -> (Ctrl Out 0 msg))

ctrlToInt : Ctrl -> Int
ctrlToInt (Ctrl d w i) = i

intToCtrl : Int -> Ctrl
intToCtrl i = Ctrl Forward 0 i

ctrlMap : (Int -> Int) -> Ctrl -> Ctrl
ctrlMap f (Ctrl d w i) = Ctrl d w (f i)
