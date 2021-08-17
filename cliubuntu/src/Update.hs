module Update (update, Msg(..), Model(..), initModel, Cmd(..)) where

data Cmd
    = GetArgs
    | SetupDb
    | Cmds [Cmd]
    deriving (Eq, Show)


data Model
    = Model
    deriving (Eq, Show)


data Msg
    = Start


update :: Model -> Msg -> (Model, Cmd)
update model Start =
    (model, Cmds [GetArgs, SetupDb])


initModel :: Model
initModel =
    Model
