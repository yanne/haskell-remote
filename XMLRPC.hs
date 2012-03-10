module XMLRPC where

data RPCType = RPCString String | RPCInt Int | RPCArray [RPCType] deriving Eq
instance Show RPCType where
    show (RPCInt i) = show i
    show (RPCString s) = s
    show (RPCArray a) = show a

type Params = [RPCType]
