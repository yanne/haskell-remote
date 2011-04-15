{-# LANGUAGE BangPatterns #-}
module RobotRemote
    (requestTypeAndURI,
     payload, methodName, methodParams, keywords) where

import Network
import System.IO
import qualified Data.Map as Map
import Data.String.Utils
import qualified Data.ByteString as BS
import Data.ByteString.Internal(c2w, w2c)
import Text.XML.Light
import Text.XML.Light.Lexer
import XMLRPC
import Control.Exception

acceptLoop :: Socket -> IO ()
acceptLoop s = do
    print $ "acceptloop"
    (handle, hostname, _) <- accept s
    !req <- BS.hGetContents handle
    BS.hPut handle $ createResponse req
    hClose handle
    acceptLoop s

createResponse :: BS.ByteString -> BS.ByteString
createResponse req =
    case action of
        "get_keyword_names" -> BS.pack $ map c2w exres2
        "run_keyword" -> BS.pack $ map c2w exres2
    where action = methodName $ payload $ map w2c $ BS.unpack req
{-        then runKeyword kwName kwArgs
        else ""
          kwName = show $ head $ methodParams $ payload req
          kwArgs = tail $ methodParams $ payload req
-}

responseHeader = join "\n" parts
    where parts = ["HTTP/1.1 200 OK",
                   "Connection: close",
                   "Content-Type: text/xml",
                   "", ""]

keywords :: Map.Map String (Params -> String)
keywords = Map.fromList [("argKeyword", \_ -> ""),
                         ("returnsGivenString", \_ -> "kukkanen")
                         ]

runKeyword :: String -> Params -> String
runKeyword name args = case Map.lookup name keywords of
                (Just kw) -> kwResponse "PASS" $ kw args
                Nothing -> "Bloo"

exres2 = responseHeader ++ "<?xml version='1.0' ?> <methodResponse> <params> <param> <value> <array> <data> <value> <string>argKeyword</string> </value> <value> <string>returnsGivenString</string> </value> </data></array></value></param></params></methodResponse>"
exres = responseHeader ++ xml_header ++
    (showElement $ arrayResponse $ Map.keys keywords) ++ "\n"

kwResponse :: String -> String -> String
kwResponse status kwRetVal = responseHeader ++ xml_header ++
    (showElement $ structResponse [structValue "status" status,
                                   structValue "return" kwRetVal])

structResponse :: [Element] -> Element
structResponse elems = methodResponse $ nestedElem "struct" elems

structValue :: String -> String -> Element
structValue name value =
    nestedElem "member"
        [textElem "name" name, textElem "value" value]

arrayResponse :: [String] -> Element
arrayResponse vals = methodResponse $ arrElemen vals

methodResponse :: Element -> Element
methodResponse e = nestedElem "methodResponse" [nestedElem "params" [
    nestedElem "param" [nestedElem "value" [e]]]]

arrElemen :: [String] -> Element
arrElemen cs = nestedElem "array" $ [nestedElem "data" [dataElem c | c <- cs]]

dataElem :: String -> Element
dataElem content = nestedElem "value" $ [textElem "string" content]

textElem :: String -> String -> Element
textElem name text = Element (unqual name) [] [content] Nothing
    where content = Text $ CData CDataText text Nothing

nestedElem :: String -> [Element] -> Element
nestedElem name es = Element (unqual name) [] [Elem e | e <- es] Nothing

requestTypeAndURI :: String -> (String, String)
requestTypeAndURI req = (parts !! 0, parts !! 1)
    where parts = splitWs $ firstLine
          firstLine = head $ lines req

payload :: String -> String
payload req = strip $ unlines $ tail $ splitFromEmptyLine
    where splitFromEmptyLine = dropWhile (\l -> strip l /="") $ lines req

methodCall :: String -> Element
methodCall paylod = head $ tail $ onlyElems $ parseXML paylod

methodName :: String -> String
methodName payload = case findElement methodName $ methodCall payload of
    (Just e) -> strContent e
    Nothing -> ""
    where methodName = unqual "methodName"

methodParams :: String -> Params
methodParams payload = extractValuesFrom $ paramsElem
    where paramsElem = findElement (unqual "params") $ methodCall payload

extractValuesFrom :: Maybe Element -> Params
extractValuesFrom (Just e) = [value ch | ch <- concat $ [elChildren el | el <- findElements (unqual "value") e]]
    where value elem@(Element (QName name _ _) _ content _) = _val elem name content
          _val elem name content
                | name ==  "string" = RPCString $ showContent $ head content
                | name == "array" =  RPCArray $ extractValuesFrom (Just elem)

main :: IO ()
main = do
  s <- listenOn (PortNumber 8001)
  acceptLoop s
