module Lang (compile, Msg(..), Cmd(..)) where


import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Map as M


data Msg
    = Start


data Cmd
    = Print T.Text
    deriving (Eq, Show)


type Model
    = M.Map T.Text B.ByteString


compile
    :: B.ByteString
    -> Either T.Text (Msg -> Model -> (Model, Cmd))
compile raw =
    case parse raw of
    Left err ->
        Left err

    Right parsed ->
        
