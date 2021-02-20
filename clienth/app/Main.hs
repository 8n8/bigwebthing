{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.STM as Stm
import System.Environment (getArgs)
import qualified System.IO as Sio
import Control.Exception (try)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import qualified Control.Concurrent.STM.TQueue as Q
import Crypto.Noise.DH.Curve25519 (Curve25519)
import qualified Crypto.Noise.DH as Dh
import qualified Data.ByteString as B
import qualified Data.ByteArray as Ba


data Mode
        = Update
        | DontKnow


data State = State
        { mode :: Mode
        , staticKeys :: Maybe (Dh.KeyPair Curve25519)
        }


initState :: State
initState =
        State
        { mode = DontKnow
        }


data Input
        = Args [String]
        | RawStaticKeys (Either IOError B.ByteString)
        | StaticKeys (Dh.KeyPair Curve25519)
        | Default


data Output
        = GetArgs
        | ReadStaticKeysFile
        | Print T.Text
        | MakeStaticKeys
        | SaveStaticKeys B.ByteString
        | Sequence [Output]
        | CloseHandle Sio.Handle
        | End
        | GetServerConn


update :: State -> Input -> (State, Output)
update state input =
        case input of
        Args ["bwt", "update"] ->
                (state, ReadStaticKeysFile)

        Args _ ->
                (state, Print "bad arguments")

        RawStaticKeys (Left _) ->
                (state, MakeStaticKeys)

        RawStaticKeys (Right raw) ->
                case parseStaticKeys raw of
                Nothing ->
                        ( state
                        , Sequence [Print "internal error", End]
                        )

                Just keys ->
                        (state { staticKeys = Just keys }
                        , case mode state of
                                DontKnow ->
                                        panic

                                Update ->
                                        GetServerConn
                        )

        StaticKeys keys ->
                (state, SaveStaticKeys $ encodeKeys keys)


panic :: Output
panic =
        Sequence [Print "internal error", End]


parseStaticKeys :: B.ByteString -> Maybe (Dh.KeyPair Curve25519)
parseStaticKeys =
        Dh.dhBytesToPair . Ba.convert


encodeKeys :: Dh.KeyPair Curve25519 -> B.ByteString
encodeKeys (secret, _) =
        Ba.convert $ Dh.dhSecToBytes secret


staticKeysFile :: String
staticKeysFile =
        "staticKeys"


run :: Q.TQueue Input -> Output -> IO ()
run inQ out =
        case out of
        End ->
                return ()

        GetArgs ->
                do
                args <- getArgs
                writeQ inQ $ Args args

        ReadStaticKeysFile ->
                do
                contents <- try $ B.readFile staticKeysFile
                writeQ inQ $ RawStaticKeys contents

        Print message ->
                Tio.putStr message

        MakeStaticKeys ->
                do
                keys <- Dh.dhGenKey
                writeQ inQ $ StaticKeys keys

        Sequence outputs ->
                mapM_ (run inQ) outputs

        CloseHandle handle ->
                Sio.hClose handle

        SaveStaticKeys keys ->
                B.writeFile staticKeysFile keys

        GetServerConn ->
                


writeQ :: Q.TQueue a -> a -> IO ()
writeQ q x =
        Stm.atomically $ Q.writeTQueue q x


main :: IO ()
main =
        do
        inQ <- Stm.atomically $ Q.newTQueue
        Stm.atomically $ Q.writeTQueue inQ Default
        mainHelp initState inQ


mainHelp :: State -> Q.TQueue Input -> IO ()
mainHelp state inQ =
        do
        newInput <- Stm.atomically $ Q.readTQueue inQ
        case update state newInput of
                (_, End) ->
                        return ()

                (newState, output) ->
                        do
                        run inQ output
                        mainHelp newState inQ
