{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main where

import Network.Run.UDP
import Network.Socket
import Network.Socket.ByteString
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Control.Concurrent
import Control.Monad
import Data.Functor
import Control.Monad.Fix
import System.IO

server :: String
server = "127.0.0.1"
port :: String
port = "12345"

data Message = REQ Integer
             | REG

request, register, message :: Parser Message
request  = "REQUEST "  *> (REQ <$> decimal)
register = "REGISTER" $> REG
message  = register <|> request

parseMsg :: ByteString -> Either String Message
parseMsg = parseOnly message

readAddr :: String -> SockAddr
readAddr s = SockAddrInet (read port) $ tupleToHostAddress (read one, read two, read thre, read four)
  where (host, _:port) = span (/= ':') s
        [one,two,thre,four] = words $ map swap host
        swap '.' = ' '
        swap x   = x

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runUDPClient server port handle

handle :: Socket -> SockAddr -> IO ()
handle sock addr = do
  void $ sendTo sock "REGISTER" addr
  [idS, addrS] <- words . BS.unpack <$> recv sock 1024
  putStrLn $ unwords ["My ID:", idS, "My Addr: ", addrS]
  putStr "Enter other client ID: "
  friend <- getLine
  void $ sendTo sock ("REQUEST " <> BS.pack friend) addr
  resp <- BS.unpack <$> recv sock 1024
  case resp of
    "NOT_FOUND" -> putStrLn "That client doesn't exist"
    (readAddr -> friendAddr) -> handleChat sock friendAddr

handleChat :: Socket -> SockAddr -> IO ()
handleChat sock addr = do
  void $ forkIO $ fix $ \loop -> do
    (msg, _) <- recvFrom sock 1024
    putStrLn $ BS.unpack msg
    loop

  fix $ \loop -> do
    msg <- BS.pack <$> getLine
    void $ sendTo sock msg addr
    loop
  return ()
