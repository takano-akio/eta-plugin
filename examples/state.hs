{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test where
import Control.Monad.State
import Eta.Plugin (EtaAnnotation(..))

{-# ANN type Proc (EtaArity 1) #-}

newtype Proc a = Proc (State Int a)
    deriving (Monad, MonadState Int)

test :: [Int] -> Proc ()
test xs = do
    x <- get
    put $! x + sum xs
