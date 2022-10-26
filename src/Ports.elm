port module Ports exposing (get,put,loopback,wait,error)

port get : (Int -> msg) -> Sub msg
port put : Maybe Int -> Cmd msg
port loopback : Maybe Int -> Cmd msg
port wait : Maybe Int -> Cmd msg
port error : String -> Cmd msg
