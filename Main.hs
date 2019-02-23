{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as De
import qualified Web.Scotty as Scot

main :: IO ()
main =
  Scot.scotty 3000 $ do
    Scot.get "/" $ Scot.file "index.html"
    Scot.post "/" $ do
      body <- Scot.body
      case De.decodeUtf8' body of
        Left err -> liftIO $ print err
        Right txt -> Scot.text $ T.concat [txt, " world!"]
