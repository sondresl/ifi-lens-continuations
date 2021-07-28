{-# LANGUAGE InstanceSigs #-}

module Actual where

import Data.Map (Map)
import qualified Data.Map as Map

-- Idea & usage {{{
add :: Int -> Int -> (Int -> a) -> a
add x y cont = cont (x + y)

sub :: Num a => a -> a -> (a -> b) -> b
sub x y cont = cont (x - y)

square :: Num a => a -> (a -> b) -> b
square x cont = cont (x ^ 2)

evalCont :: ((a -> a) -> a) -> a
evalCont cont = cont id

-- }}}

-- Isomorphism with Identity {{{

idToCont :: a -> ((a -> a) -> a)
idToCont = undefined

contToId :: ((a -> a) -> a) -> a
contToId = undefined

-- }}}
--
-- Callbacks {{{{{{
type DB = Map String String

db :: DB
db = Map.fromList
  [ ("Gaute", "Berge")
  , ("Eirik", "Sæther")
  , ("Ida", "Motzfeldt")
  , ("Morten", "Kolstad")
  , ("Petter", "Moen")
  , ("Sondre", "Lunde")
  ]

fetchSurname :: DB -> String -> (Maybe String -> String) -> String
fetchSurname db firstname handler = 
  handler $ Map.lookup firstname db

handleMissing :: Maybe String -> String
handleMissing (Just v) = v
handleMissing Nothing = "Name missing"

fetchFromNameDB :: String -> (Maybe String -> String) -> String
fetchFromNameDB = fetchSurname db

-- }}}}}}

-- Definition and Instances {{{
newtype Cont r a = Cont {runCont :: (a -> r) -> r}

-- Some basic usage

-- Instances
instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f cc = undefined

instance Applicative (Cont r) where
  -- pure = Cont . flip ($)
  pure = Cont . flip ($)
  cf <*> cx = undefined

instance Monad (Cont r) where
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  cx >>= f = Cont $ \k ->
    runCont cx $ \x ->
      runCont (f x) k

-- }}}

-- CallCC {{{
trivialDo :: Cont r Int
trivialDo = callCC $ \ret -> do
  ret 5
  pure 25


trivial :: Cont r Int
trivial = callCC $ \ret ->
  ret 5 >>= \_x ->
    pure 25

-- Show idea!

-- }}}

-- Implementation {{{
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC = undefined

-- }}}

-- Using callcc {{{
divExcpt :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcpt x y handler =
  callCC $ \ok ->
    if y /= 0
      then ok (x `div` y)
      else handler "Denominator 0"

-- }}}

main :: IO ()
main = do
  print $ runCont trivial id

  -- print $ (\k -> add 5 6) square id
  print 1

-- Resources
-- https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style#Example:_a_complicated_control_structure
--
