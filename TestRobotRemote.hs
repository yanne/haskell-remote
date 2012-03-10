module TestRobotRemote where

import Test.HUnit
import RobotRemote
import XMLRPC

testParsing = TestList [
    parseTypeAndURIFromRequest,
    parsePayload,
    parseMethodName,
    parseStringParam,
    parseArrayParam
    ]

exampleRequest = "POST /RPC2 HTTP/1.0\r\nHost: localhost:8001\r\n" ++
    "User-Agent: xmlrpclib.py/1.0.1 (by www.pythonware.com)\r\n" ++
    "Content-Type: text/xml\r\nContent-Length: 128\r\n" ++
    "\r\n" ++
    exampleXMLRPC

xmlHeader = "<?xml version='1.0' encoding='UTF-8'?>\n"
exampleXMLRPC =  xmlHeader ++ methodCall exampleMethodNameTag

methodCall :: String -> String
methodCall content = "<methodCall>" ++ content ++ "</methodCall>"

someMethodName = "someMethodName"
exampleMethodNameTag = "<methodName>" ++ someMethodName ++ "</methodName>"

exampleXMLRPCWithStringParam = xmlHeader ++ methodCall stringParams
stringParams = "<params><param><value>" ++
               "<string>kwname</string>" ++
               "</value></param><param>"

exampleXMLRPCWithArrayParam = makeXml $ methodCall arrayParams
arrayParams = "<params><param><value><array>" ++
                "<data><value><string>argument</string></value></data>" ++
              "</array></value></param></params>"

makeXml :: String -> String
makeXml content = xmlHeader ++ content

parseTypeAndURIFromRequest = 
    (requestTypeAndURI exampleRequest) ~?= ("POST", "/RPC2")

parsePayload = (payload exampleRequest) ~?= exampleXMLRPC

parseMethodName = (methodName $ exampleXMLRPC) ~?= someMethodName

parseStringParam = param ~?= [RPCString "kwname"]
        where param = methodParams $ exampleXMLRPCWithStringParam

parseArrayParam = param ~?= [RPCArray [RPCString "argument"]]
        where param = methodParams $ exampleXMLRPCWithArrayParam

main = do runTestTT testParsing

