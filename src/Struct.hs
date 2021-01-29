module Struct where

import Common

import Data.IORef
import Control.Monad.IO.Class (liftIO)

makeStruct :: Symbol -> [Symbol] -> [Value] -> Lisp Value
makeStruct n fs xs = do
  vals <- zip fs <$> (liftIO . newIORef) `traverse` xs
  pure $ Struct n vals

getIORef :: Symbol -> [(Symbol, IORef Value)] -> Symbol -> Lisp (IORef Value)
getIORef sn vs n = case lookup n vs of
    Just ref -> pure ref
    Nothing -> lispError NameError $ "no struct field " <> n <> " in " <> sn

getField :: Symbol -> [(Symbol, IORef Value)] -> Symbol -> Lisp Value
getField sn vs n = getIORef sn vs n >>= liftIO . readIORef

setField :: Symbol -> [(Symbol, IORef Value)] -> Symbol -> Value -> Lisp ()
setField sn vs n x = getIORef sn vs n >>= liftIO . (flip writeIORef x)
