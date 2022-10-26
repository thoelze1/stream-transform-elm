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

-- I tried reimplementing doOps using Either.andThen to capture some
-- of the repetitive Left checks, but these checks are mixed in with
-- other logic which gets in the way of chaining Either
-- computations. Even with StateEither, I'm not sure the code can be
-- simplified much.
doOps : List Ctrl -> Model -> Either.Either Error (Model, List Ctrl)
doOps i m =
  case m of
    [] -> Right ([],i)
    ((l,s)::rest) ->
      case i of
        [] -> Right (m,[])
        ((Ctrl d w x)::xs) ->
         case (l x s) of
           Left e -> Left e
           Right (output,newLayerState) ->
            case doOps xs ((l,newLayerState)::rest) of
             Left e -> Left e
             Right (newModelState,finalOutput1) ->
              case newModelState of
               [] -> Left BadModel
               ((ya,yb)::ys) ->
                case doOps output ys of
                 Either.Left e -> Either.Left e
                 Either.Right (newRestState,finalOutput2) ->
                  Either.Right ((l,yb)::newRestState,finalOutput1++ finalOutput2)
  
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
