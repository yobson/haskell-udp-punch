{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Run.UDP
import Network.Socket
import Network.Socket.ByteString
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Control.Concurrent
-- import Control.Concurrent.MVar
import Control.Monad
import Data.Functor
-- import Data.List
import Control.Monad.Fix


data Message = REQ Integer
             | REG

request, register, message :: Parser Message
request  = "REQUEST "  *> (REQ <$> decimal)
register = "REGISTER" $> REG
message  = register <|> request

parseMsg :: ByteString -> Either String Message
parseMsg = parseOnly message

main :: IO ()
main = do
  clients <- newMVar []
  ids     <- newMVar 0
  fix $ \loop -> do
    runUDPServer (Just "0.0.0.0") "12345" (handle ids clients)
    loop

handle :: MVar Integer -> MVar [(Integer, SockAddr)] -> Socket -> IO ()
handle ids clients sock = do
  (msg, addr) <- recvFrom sock 4096
  putStr $ show addr <> ": "
  putStrLn $ BS.unpack msg
  case parseMsg msg of
    Right (REG) -> do
      myId <- takeMVar ids
      putMVar ids (myId + 1)
      lst <- takeMVar clients
      putMVar clients ((myId,addr):lst)
      void $ sendTo sock (BS.pack $ unwords [show myId, show addr]) addr
      putStrLn "Replied"
    Right (REQ rid) -> do
      lst <- readMVar clients
      case lookup rid lst of
        Just ip -> void $ sendTo sock (BS.pack $ show ip) addr
        Nothing -> void $ sendTo sock "NOT_FOUND" addr
    Left _ -> do
      putStrLn "Parse Failed"
      return ()
