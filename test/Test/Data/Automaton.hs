{-# LANGUAGE OverloadedStrings #-}
module Automaton where

import           Distribution.TestSuite

import           Control.Monad          ((>=>))
import           Control.Monad.Writer   (runWriter, tell)

import           Data.Monoid            (All (..))
import           Data.Text              (Text)
import           Data.Text.Utils

import           Data.Pad

import           Data.Automaton

import           System.Exit            (exitFailure)

data Test m = Test { run  :: WriterT Text m Bool
                   , name :: Text
                   }

tests = [test1, test2, test3, test4]

main = if getAll runAllTests then return () else exitFailure
    where runAllTests = fold $ mapM runTest tests
          runTest (Test test name) = let (r, w) = runWriter test
                                     in if r then succeed w else failed w
          result w = putTextLn $ w +++ diag r >> return (All . r)
          diag r = if r then "succeed" else "failed"

-----------------------
-- TESTS
--

data InArgs t = IN t (Either PAD PASS)
    deriving (Eq, Show)

data OutArgs = OutPASS (Maybe PASS)
             | OutBool Bool
             | OutUnit
    deriving (Eq, Show)

logSyAnOk t = "syn. analysis from " +++ t +++ eol
logSyAnFail t =  "failure with " +++ t +++ " : no PAD arg." +++ eol
logStAnOk t = "sta. analysis from " +++ t +++ eol
logStAnFail t = "failure with " +++ t +++ " : no PASS arg." +++ eol
logStart t = "Start with " +++ t +++ eol
logFinish t = "End with " +++ t +++ eol

-- Data.Automaton.syntacticAnalysis Wrap
actionSyAn :: InArgs Text -> Diagnostic OutArgs
actionSyAn (IN t (Left pad)) = syntacticAnalysis message pad >>= return . OutPASS
    where message = logSyAnOk t
actionSyAn (IN t _)          = tell message >> return (OutPASS Nothing)
    where message = logSyAnFail t

-- Data.Automaton.staticAnalysis Wrap
actionStAn :: InArgs Text -> Diagnostic OutArgs
actionStAn (IN t (Right pass)) = staticAnalysis message pass >>= return . OutBool
    where message = logStAnOk t
actionStAn (IN t _)          = tell message >> return (OutBool False)
    where message = logStAnFail t

actionStart :: InArgs Text -> Diagnostic OutArgs
actionStart (IN t _) = tell message >> return OutUnit
    where message = logStart t

actionFinish :: InArgs Text -> Diagnostic OutArgs
actionFinish (IN t _) = tell message >> return OutUnit
    where message = logFinish t

type StateDD = AutomatonState (InArgs Text) (Diagnostic OutArgs)

s0 :: StateDD
s0 = AutomatonState "s0" actionStart

s1 :: StateDD
s1 = AutomatonState "s1" actionSyAn

s2 :: StateDD
s2 = AutomatonState "s2" actionStAn

s3 :: StateDD
s3 = AutomatonState "s3" actionFinish

type TransitionDD = AutomatonTransition (InArgs Text) (Diagnostic OutArgs)

-- valid
iarg1 :: Text -> InArgs Text
iarg1 label = IN label (Left (PAD label))
iarg2 :: Text -> InArgs Text
iarg2 label = IN label (Right (PASS label))
iarg3 :: Text -> InArgs Text
iarg3 label = IN label (Right (PASS label))

t1 :: Text -> TransitionDD
t1 label = AutomatonTransition (iarg1 label) s0 s1
t2 :: Text -> TransitionDD
t2 label = AutomatonTransition (iarg2 label) s1 s2
t3 :: Text -> TransitionDD
t3 label = AutomatonTransition (iarg3 label) s2 s3

-- invalid
iarg1' :: Text -> InArgs Text
iarg1' label = IN label (Right (PASS label))
iarg2' :: Text -> InArgs Text
iarg2' label = IN label (Left (PAD label))

t1' :: Text -> TransitionDD
t1' label = AutomatonTransition (iarg1' label) s0 s1 -- s1 action will failed
t2' :: Text -> TransitionDD
t2' label = AutomatonTransition (iarg2' label) s1 s2 -- s2 action will failed

type AutomatonDD = Automaton (InArgs Text) (Diagnostic OutArgs)

ts label = [t1,t2,t3] <*> [label]

aut0 label = Automaton s0 (ts label)
aut1 label = Automaton s1 (ts label)
aut2 label = Automaton s2 (ts label)
aut3 label = Automaton s3 (ts label)

aex1 :: Text -> AutomatonExecution (InArgs Text) b (Maybe b)
aex1 label = evaluate (iarg1 label)

aex2 :: Text -> AutomatonExecution (InArgs Text) b (Maybe b)
aex2 label = evaluate (iarg2 label)

aex3 :: Text -> AutomatonExecution (InArgs Text) b (Maybe b)
aex3 label = evaluate (iarg3 label)

aex1' :: Text -> AutomatonExecution (InArgs Text) b (Maybe b)
aex1' label = evaluate (iarg1' label)

--

test1 = Test testDD1 "DD 1 state s0 -> s1"

testDD1 = Writer Text Bool
testDD1 = do tell $ "-- " +++ eol
             let (out, aut') = runAutomaton (aex1 label) (aut0 label)
                 automatonCheck = aut' == (aut1 label)
                 diagnosticCheck = maybe False (test . runWriter) out
             return $ automatonCheck && diagnosticCheck
    where label = "test1"
          expectedLog = "read PAD : " +++ label +++ eol
                      +++ logSyAnOk label
          expectedOut = OutPASS $ Just $ PASS $ "pass(" +++ label +++ ")"
          test (r, w) = (w == expectedLog) && (r == expectedOut)

test2 = Test testDD2 "DD 1 state s0 -> s1"

testDD2 = Writer Text Bool
testDD2 = do tell "-- "
             let (out, aut') = runAutomaton (aex2 label) (aut1 label)
                 automatonCheck = aut' == (aut2 label)
                 diagnosticCheck = maybe False (test . runWriter) out
             return $ automatonCheck && diagnosticCheck
    where label = "test2"
          expectedLog = "read PASS : " +++ label +++ eol
                      +++ logStAnOk label
          expectedOut = OutBool True
          test (r, w) = (w == expectedLog) && (r == expectedOut)

test3 = Test testDD3 "DD 1 state s0 -> s1"

testDD3 = Writer Text Bool
testDD3 = do tell "-- "
             let (out, aut') = runAutomaton (aex3 label) (aut2 label)
                 automatonCheck = aut' == (aut3 label)
                 diagnosticCheck = maybe False (test . runWriter) out
             return $ automatonCheck && diagnosticCheck
    where label = "test3"
          expectedLog = logFinish label
          expectedOut = OutUnit
          test (r, w) = (w == expectedLog) && (r == expectedOut)

test4 = Test testDD4 "DD 4 state s0 -> s0"

testDD4 = Writer Text Bool
testDD4 = do tell "-- "
             let (out, aut') = runAutomaton (aex1' label) (aut0 label)
                 automatonCheck = aut' == (aut0 label)
                 diagnosticCheck = out == Nothing
             return $ automatonCheck && diagnosticCheck
    where label = "test4"

