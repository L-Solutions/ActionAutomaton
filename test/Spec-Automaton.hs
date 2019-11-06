{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.State  (get)
import           Control.Monad.Writer
import           Data.Maybe           (isNothing)
import           Data.Text.Utils
import           Test.Lib

import           Data.Automaton

tests = [ testRun0
        , testRun1
        , testRun2
        , testPause0
        , testPause1
        , testPause2
        , testReset0
        , testReset1
        , testReset2
        , testFail0
        , testFail1
        , testFail2
        ]

main :: IO ()
main = do result <- runAll tests
          unless (getAll result) exitFailure
          return ()

-- Test
--

data Label = Run | Reset | Pause | Fail
    deriving (Eq, Show)

data Effect = Ok | Ko
    deriving (Eq, Show)

type Input = (Label, Text)
type Output = Writer Text Effect

type AutomatonIO = Automaton Input Output
type AActionIO = Input -> Output
type AStateIO = AutomatonState Input Output
type ATransitionIO = AutomatonTransition Input Output
type AExecutionIO p = AutomatonExecution Input Output p

logMSG :: Show a => Text -> a -> Text
logMSG m n = m <> (pack $ show n)

logError n = logMSG "Error " n
logRun n = logMSG "Run from " n
logReset n = logMSG "Reset from " n
logPause n = logMSG "Pause from " n
logFail n = logMSG "ERROR => Fail from " n

--
-- States
--
-- Idle
actionS0 :: AActionIO
actionS0 (Run, args) = do tell $ logRun args
                          return $ Ko
actionS0 (Reset, args) = do tell $ logReset args
                            return $ if args == "Waiting" then Ko else Ok
actionS0 (Pause, args) = do tell $ logPause args
                            return $ Ko
actionS0 (Fail, args) = do tell $ logFail args
                           return $ Ko

-- Run
actionS1 :: AActionIO
actionS1 (Run, args) = do tell $ logRun args
                          return $ if args == "Running" then Ko else Ok
actionS1 (Reset, args) = do tell $ logReset args
                            return $ Ko
actionS1 (Pause, args) = do tell $ logPause args
                            return $ Ko
actionS1 (Fail, args) = do tell $ logFail args
                           return $ Ko

-- Pause
actionS2 :: AActionIO
actionS2 (Run, args) = do tell $ logRun args
                          return $ Ko
actionS2 (Reset, args) = do tell $ logReset args
                            return $ Ko
actionS2 (Pause, args) = do tell $ logPause args
                            return $ if args == "Pausing" then Ko else Ok
actionS2 (Fail, args) = do tell $ logFail args
                           return $ Ko
s0 :: AStateIO
s0 = AutomatonState "Waiting" actionS0

s1 :: AStateIO
s1 = AutomatonState "Running" actionS1

s2 :: AStateIO
s2 = AutomatonState "Pausing" actionS2

--
-- Transition
--

transitions :: [ATransitionIO]
transitions = [ tRun_01
              , tRun_21
              , tPause_12
              , tReset_00
              , tReset_20
              ]

tRun_01 = AutomatonTransition "Run" s0 s1 (\p -> fst p == Run)
tRun_21 = AutomatonTransition "Run" s2 s1 (\p -> fst p == Run)
tPause_12 = AutomatonTransition "Pause" s1 s2 (\p -> fst p == Pause)
tReset_20 = AutomatonTransition "Reset" s2 s0 (\p -> fst p == Reset)
tReset_00 = AutomatonTransition "Reset" s0 s0 (\p -> fst p == Reset)


--
-- Automaton
--

aut0 = Automaton s0 transitions
aut1 = Automaton s1 transitions
aut2 = Automaton s2 transitions

aex :: Label -> AExecutionIO (Outcome Output)
aex l = get >>= \s -> evaluateProgress (l, getName s)
aexRun = aex Run
aexReset = aex Reset
aexPause = aex Pause
aexFail = aex Fail

--
-- Test runAutomaton
--

runTest :: AExecutionIO o
        -> AutomatonIO
        -> (AutomatonIO -> Bool)
        -> (AutomatonIO -> Writer Text ())
        -> (o -> Bool)
        -> (o -> Writer Text ())
        -> Writer Text Bool
runTest exec start testAutomaton logAutomaton testOutcome logOutcome =
    do let (out, aut') = runAutomaton exec start
           automatonCheck = testAutomaton aut'
           diagnosticCheck = testOutcome out
       tell $ "Automaton  : " <> pack (show automatonCheck) <> eol
       logAutomaton aut'
       tell $ "Diagnostic : " <> pack (show diagnosticCheck) <> eol
       logOutcome out
       return $ automatonCheck && diagnosticCheck

runSucceedsWith :: AExecutionIO (Outcome Output)
                -> AutomatonIO
                -> AutomatonIO
                -> Text
                -> Effect
                -> Writer Text Bool
runSucceedsWith exec start target expectedLog expectedResult =
    runTest exec start (== target) logAut testOutcome logOut
      where testOutcome (Produce w) = let (r, log) = runWriter w
                                      in (log == expectedLog) && (r == expectedResult)
            testOutcome _           = False
            logAut aut' = do tell $ "             aut' = " <> pack (show aut') <> eol
                             tell $ "         expected = " <> pack (show target) <> eol
            logOut out  = do tell $ "              out = " <> pack (show out) <> eol
                             tell $ "  expected result = " <> pack (show expectedResult) <> eol
                             tell $ "     expected log = " <> pack (show expectedLog) <> eol

runSucceeds :: AExecutionIO (Outcome Output)
            -> AutomatonIO
            -> Writer Text Bool
runSucceeds exec start = runTest exec start (== start) logAut (== Succeed) logOut
      where logAut aut' = do tell $ "             aut' = " <> pack (show aut') <> eol
                             tell $ "         expected = " <> pack (show start) <> eol
            logOut out  = do tell $ "              out = " <> pack (show out) <> eol
                             tell $ "     expected out = " <> pack (show (Succeed::Outcome ())) <> eol

runFails :: AExecutionIO (Outcome Output)
         -> AutomatonIO
         -> Writer Text Bool
runFails exec start = runTest exec start (== start) logAut (== Failed) logOut
      where logAut aut' = do tell $ "             aut' = " <> pack (show aut') <> eol
                             tell $ "         expected = " <> pack (show start) <> eol
            logOut out  = do tell $ "              out = " <> pack (show out) <> eol
                             tell $ "     expected out = " <> pack (show (Failed::Outcome ())) <> eol

------------------------------------------------------------------------------------------
-- TEST CASE
--
testRun0 = mkTest "Test : s0 -Run-> succeeds to s1 with Ok" test
    where test = runSucceedsWith aexRun aut0 aut1 (logRun "Waiting") Ok

testRun1 = mkTest "Test : s1 -Run-> fails" test
    where test = runFails aexRun aut1

testRun2 = mkTest "Test : s2 -Run-> succeeds to s1 with Ok" test
    where test = runSucceedsWith aexRun aut2 aut1 (logRun "Pausing") Ok

testPause0 = mkTest "Test : s0 -Pause-> fails" test
    where test = runFails aexPause aut0

testPause1 = mkTest "Test : s1 -Pause-> fails" test
    where test = runSucceedsWith aexPause aut1 aut2 (logPause "Running") Ok

testPause2 = mkTest "Test : s2 -Pause-> Fails" test
    where test = runFails aexPause aut2

testReset0 = mkTest "Test : s0 -Reset-> succeeds" test
    where test = runSucceeds aexReset aut0

testReset1 = mkTest "Test : s1 -Reset-> fails" test
    where test = runFails aexReset aut1

testReset2 = mkTest "Test : s2 -Reset-> succeeds to s0 with Ok" test
    where test = runSucceedsWith aexReset aut2 aut0 (logReset "Pausing") Ok

testFail0 = mkTest "Test : s0 -Fail-> fails" test
    where test = runFails aexFail aut0

testFail1 = mkTest "Test : s1 -Fail-> fails" test
    where test = runFails aexFail aut1

testFail2 = mkTest "Test : s2 -Fail-> fails" test
    where test = runFails aexFail aut2

