{-# LANGUAGE OverloadedStrings #-}

module Test.Lib (
      Test
    , mkTest
    , failWith
    , succeedWith
    , run
    , runAll
    -- export System.Exit.exitFailure
    , exitFailure
    -- export Control.Monad.Writer.tell
    , tell
    ) where

import           Control.Monad        (unless)
import           Control.Monad.Writer (Writer (..), runWriter, tell)
import           Data.Foldable        (fold)
import           Data.Monoid          (All (..))
import           Data.Text            (Text)
import           Data.Text.Color
import           Data.Text.Utils
import           Data.Traversable     (sequence)
import           System.Exit          (exitFailure)

testColors :: Decoration Color8 ()
testColors = Decoration (Just Blue) Nothing

failColors :: Decoration Color8 ()
failColors = Decoration (Just Red) Nothing

succeedColors :: Decoration Color8 ()
succeedColors = Decoration (Just Green) Nothing

data Test a = Test { test :: Writer Text a
                   , name :: Text
                   }

mkTest :: Text -> Writer Text a -> Test a
mkTest name test = Test test $ colorize testColors name

failWith t = do putText t
                putTextLn $ colorize failColors "Failure"

succeedWith t = do putText t
                   putTextLn $ colorize succeedColors "Success"

run :: Test Bool -> IO All
run testcase = do let (r, w) = runWriter $ test testcase
                      label  = name testcase
                  unless r $ failWith (label <> eol <> w) >> exitFailure
                  succeedWith $ label <> eol
                  return $ All r

runAll :: Traversable t => t (Test Bool) -> IO All
runAll ts = mapM run ts >>= return . fold

--

