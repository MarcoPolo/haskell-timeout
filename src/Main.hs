{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.ByteString.Char8


main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [("delay/:time", mSetTimeout)] <|>
    dir "static" (serveDirectory ".")

parseTimeout :: String -> Int
parseTimeout = (1000 *) . read

mSetTimeout :: Snap ()
mSetTimeout = do
    param <- getParam "time"
    maybe (writeBS "must specify echo/param in URL")
          (\x -> liftIO (threadDelay . parseTimeout . unpack $ x) >> writeBS "pwn")
          param
