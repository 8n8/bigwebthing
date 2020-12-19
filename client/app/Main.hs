{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main (main) where

import Update
    ( update
    , Msg(..)
    , State(..)
    , Output(..)
    , Init(EmptyI)
    , Drg(..)
    )
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.STM as Stm
import System.Environment (getArgs)
import qualified Data.Text.IO as Tio
import qualified Network.Simple.TCP as Tcp
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Crypto.PubKey.Ed25519 as Ed
import Crypto.Random (getSystemDRG)
import Debug.Trace (trace)


main :: IO ()
main = do
    model <- TVar.newTVarIO (InitS EmptyI)
    updateIo model StartM


serverUrl :: Tcp.HostName
serverUrl =
    "localhost"


serverPort :: Tcp.ServiceName
serverPort =
    "11453"


keysPath :: FilePath
keysPath =
    "bigwebthingSECRET"


updateIo :: TVar.TVar State -> Msg -> IO ()
updateIo mainState msg = do
    output <- Stm.atomically $ do
        model <- TVar.readTVar mainState
        let (output, newModel) = update model msg
        TVar.writeTVar mainState newModel
        return output
    io mainState output


showb :: B.ByteString -> String
showb bs =
    show $ B.unpack bs


io :: TVar.TVar State -> Output -> IO ()
io mainState output =
    case output of
    GetRandomGenO -> do
        drg <- getSystemDRG
        updateIo mainState $ RandomGenM $ Drg drg

    TcpListenO socket len -> do
        result <- E.try $ Tcp.recv socket len
        updateIo mainState $ FromServerM result

    TcpSendO socket msg ->
        trace ("sending: " <> showb msg) $ do
        eitherError <- E.try $ Tcp.send socket msg
        updateIo mainState $ TcpSendResultM eitherError

    WriteKeyToFileO key -> do
        B.writeFile keysPath key

    PrintO msg ->
        Tio.putStr msg

    ReadMessageFromStdInO -> do
        stdin <- B.getContents
        updateIo mainState $ StdInM stdin

    GenerateSecretKeyO -> do
        key <- Ed.generateSecretKey
        updateIo mainState $ NewSecretKeyM key

    GetArgsO -> do
        args <- getArgs
        updateIo mainState $ ArgsM args

    ReadSecretKeyO -> do
        raw <- E.try $ B.readFile keysPath
        updateIo mainState $ SecretKeyFileM raw

    DoNothingO ->
        return ()

    BatchO outputs -> trace ("OUTPUTS: " <> show outputs) $ do
        mapM_ (\o -> print ("hi" :: String) >> io mainState o) outputs

    MakeTcpConnO ->
        do
        err <- E.try $ Tcp.connect serverUrl serverPort $ \(conn, _) ->
            updateIo mainState $ TcpConnM conn
        updateIo mainState $ TcpConnErrM err
