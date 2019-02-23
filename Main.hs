{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as De
import qualified Web.Scotty as Scot
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = Scot.scotty 3000 $
    Scot.post "/" $ do
        body <- Scot.body
        case De.decodeUtf8' body of
            Left err -> liftIO $ print err
            Right txt ->
                Scot.text $ T.concat [txt, " world!"]
