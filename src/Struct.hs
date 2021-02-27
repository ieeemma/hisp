module Struct where

import Common
import Eval

import Data.IORef
import Data.Traversable (for)
import Control.Monad.IO.Class (liftIO)

-- Consruct a structure constructor given a name and a list of
-- argument names. The result is a function that, when given some
-- arguments, will produce a structure value.
makeStruct :: Symbol -> [Symbol] -> [Value] -> Lisp Value
makeStruct n fs xs = do
  vals <- zip fs <$> (liftIO . newIORef) `traverse` xs
  pure $ Struct n vals

-- Get the IORef stored within a structure under a given field name,
-- or throw an error if it does not exist
getIORef :: Symbol
         -> [(Symbol, IORef Value)]
         -> Symbol
         -> Lisp (IORef Value)
getIORef sn vs n = case lookup n vs of
    Just ref -> pure ref
    Nothing -> lispError NameError
             $ "no struct field " <> n <> " in " <> sn


-- Get the value of a field in a struct
getField :: Symbol
         -> [(Symbol, IORef Value)]
         -> Symbol
         -> Lisp Value
getField sn vs n = getIORef sn vs n >>= liftIO . readIORef

-- Set the value of a field in a struct
setField :: Symbol
         -> [(Symbol, IORef Value)]
         -> Symbol
         -> Value
         -> Lisp ()
setField sn vs n x =
    getIORef sn vs n >>= liftIO . (flip writeIORef x)

-- Given a struct definition of the form `(define-struct field1 field2 ...)`
-- make functions to create this struct, and to get and set each
-- field, then insert these functions into the environment. If the
-- provided arguments are not symbols, raise an error.
defineStruct :: Symbol -> [Value] -> Lisp ()
defineStruct name fields = do
    let extract = \case
            Symbol x -> pure x
            _ -> lispError FormError
                     "define-struct expected symbol field name"
    fields' <- extract `traverse` fields
    let makeN = "make-" <> name
        makeF = makeProc makeN (== (length fields))
              $ makeStruct name fields'
    define makeN makeF
    for fields' $ \f -> do
        let getN = name <> "-" <> f
            getF = makeProc getN (== 1) $ \[st] -> case st of
                Struct n xs | n == name -> getField n xs f
                _ -> lispError TypeError
                         (getN <> " expected " <> name <> " argument")
            setN = name <> "-" <> f <> "-set!"
            setF = makeProc getN (== 2) $ \[st, x] -> case st of
                Struct n xs | n == name -> setField n xs f x
                                        *> pure Null
                _ -> lispError TypeError
                         (setN <> " expected " <> name <> " argument")
        define getN getF
        define setN setF
    pure ()
