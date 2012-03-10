{-# LANGUAGE BangPatterns #-}
import Network
import System.IO
import qualified Data.Map as Map
import Data.String.Utils
import qualified Data.ByteString as BS
import Data.ByteString.Internal(c2w, w2c)
import Text.XML.Light
import Text.XML.Light.Lexer
import Control.Exception

acceptLoop :: Socket -> IO ()
acceptLoop s = do
    print $ "acceptloop"
    (handle, hostname, _) <- accept s
    hSetBuffering handle NoBuffering
    !req <- BS.hGetSome handle 1000
    print $ BS.append req (BS.pack $ map c2w "Request\n\n")
    !req2 <- BS.hGetSome handle 1000
    print req2
    BS.hPut handle $ createResponse req2
    hClose handle

createResponse req = "response"

main :: IO ()
main = do
  s <- listenOn (PortNumber 8001)
  sequence_ $ repeat $ acceptLoop s
