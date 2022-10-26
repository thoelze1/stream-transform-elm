module Main exposing (main)

import Platform exposing (Program)
import Either exposing (Either(..))

import Ports exposing (get,put,loopback,wait,error)
import Layer exposing (Layer,LState)
import Event exposing (Ctrl(..),Dest(..),dispatch)
import Error exposing (Error(..),errorToString)
import Examples

type alias Flags = ()
type alias Model = List (Layer , LState)
type alias Msg = Ctrl

main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

init : Flags -> ( Model, Cmd Msg )
init _ = ( [] , Cmd.none )

-- TODO use monad operators to simplify complex state expressions
-- TODO use state monad to simplify type of doOps?
doOps : List Ctrl -> Model -> Either.Either Error (Model, List Ctrl)
doOps i m =
  case m of
    [] -> Either.Right ([],i)
    ((l,s)::rest) ->
      case i of
        [] -> Either.Right (m,[])
        ((Ctrl d w x)::xs) ->
         case (l x s) of
           Either.Left e -> Either.Left e
           Either.Right (output,newLayerState) ->
            case doOps xs ((l,newLayerState)::rest) of
             Either.Left e -> Either.Left e
             Either.Right (newModelState,finalOutput1) ->
              case newModelState of
               [] -> Either.Left BadModel
               ((ya,yb)::ys) ->
                case doOps output ys of
                 Either.Left e -> Either.Left e
                 Either.Right (newRestState,finalOutput2) ->
                  Either.Right ((l,yb)::newRestState,List.append finalOutput1
                                                         finalOutput2)
  
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Ctrl Out w i -> (model,dispatch msg)
    Ctrl In w i -> (model,dispatch msg)
    Ctrl Forward w i ->
      case doOps [msg] model of
        Either.Left e -> (model,error (errorToString e))
        Either.Right (newModel,output) ->
         case output of
          [] -> (newModel , put Nothing)
          _ -> (newModel , List.map dispatch output |> Cmd.batch)

subscriptions : Model -> Sub Msg
subscriptions _ =
    get <| Ctrl Forward 0
