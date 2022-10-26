module Error exposing (Error(..),errorToString)

type Error
    = BadModel
    | UnexpectedLState

-- TODO this should be data not code
errorToString : Error -> String
errorToString e =
    case e of
        BadModel -> "something is wrong with the model"
        UnexpectedLState -> "unexpected type of state received by layer"
