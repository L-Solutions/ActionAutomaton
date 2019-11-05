{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad        (unless)
import           Control.Monad.Writer (Writer (..), runWriter)

import           Data.Monoid          (getAll)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Color
import           Data.Text.Utils

import           Data.Automaton
import           Data.Pad

import           Test.Lib

tests = [ test1
        , test2
        , test3
        , test4
        ]

main :: IO ()
main = do result <- runAll tests
          unless (getAll result) exitFailure
          return ()

-----------------------
-- TESTS
--

data InArgs t = IN t (Either PAD PASS)
    deriving (Eq, Show)

data OutArgs = OutPASS (Maybe PASS)
             | OutBool Bool
             | OutUnit
    deriving (Eq, Show)

logSyAnOk t = "syn. analysis from " <> t <> eol
logSyAnFail t =  "SyAn. failure with " <> t <> " : no PAD arg." <> eol
logStAnOk t = "sta. analysis from " <> t <> eol
logStAnFail t = "StAn. failure with " <> t <> " : no PASS arg." <> eol
logStart t = "Start with " <> t <> eol
logFinish t = "End with " <> t <> eol

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
t1 label = AutomatonTransition (label <> " PAD") s0 s1 $ mkGuardWithPAD label
t2 :: Text -> TransitionDD
t2 label = AutomatonTransition (label <> " PASS") s1 s2 $ mkGuardWithPASS label
t3 :: Text -> TransitionDD
t3 label = AutomatonTransition label s2 s3 $ mkGuard label

mkGuardWithPAD :: Text -> InArgs Text -> Bool
mkGuardWithPAD _ (IN _ (Right _))  = False
mkGuardWithPAD t1 (IN t2 (Left _)) = t1 == t2

mkGuardWithPASS :: Text -> InArgs Text -> Bool
mkGuardWithPASS _ (IN _ (Left _))    = False
mkGuardWithPASS t1 (IN t2 (Right _)) = t1 == t2

mkGuard :: Text -> InArgs Text -> Bool
mkGuard t1 (IN t2 _) = t1 == t2

-- invalid
iarg1' :: Text -> InArgs Text
iarg1' label = IN label (Right (PASS label))
iarg2' :: Text -> InArgs Text
iarg2' label = IN label (Left (PAD label))

t1' :: Text -> TransitionDD
t1' label = AutomatonTransition label s0 s1 (mkGuard label) -- s1 action will failed
t2' :: Text -> TransitionDD
t2' label = AutomatonTransition label s1 s2 (mkGuard label) -- s2 action will failed

type AutomatonDD = Automaton (InArgs Text) (Diagnostic OutArgs)

ts label = [t1,t2,t3] <*> [label]

aut0 label = Automaton s0 (ts label)
aut1 label = Automaton s1 (ts label)
aut2 label = Automaton s2 (ts label)
aut3 label = Automaton s3 (ts label)

aex1 :: Text -> AutomatonExecution (InArgs Text) b (Outcome b)
aex1 label = evaluate (iarg1 label)

aex2 :: Text -> AutomatonExecution (InArgs Text) b (Outcome b)
aex2 label = evaluate (iarg2 label)

aex3 :: Text -> AutomatonExecution (InArgs Text) b (Outcome b)
aex3 label = evaluate (iarg3 label)

aex1' :: Text -> AutomatonExecution (InArgs Text) b (Outcome b)
aex1' label = evaluate (iarg1' label)

--

test1 = mkTest "DD 1 state s0 -> s1" testDD1
testDD1 = do let (out, aut') = runAutomaton (aex1 label) (aut0 label)
                 automatonCheck = aut' == (aut1 label)
                 diagnosticCheck = test out
             tell $ withFG Blue "|" <> "   Automaton : " <> T.pack (show automatonCheck) <> eol
             tell $ withFG Blue "|" <> "       aut = " <> pack (show aut') <> eol
             tell $ withFG Blue "|" <> "   Diagnostic : " <> T.pack (show diagnosticCheck) <> eol
             tell $ withFG Blue "|" <> "       out = " <> pack (show out) <> eol
             return $ automatonCheck && diagnosticCheck
    where label = "test1"
          expectedLog = "read PAD : " <> label <> eol
                      <> logSyAnOk label
          expectedOut = OutPASS $ Just $ PASS $ "pass(" <> label <> ")"
          test (Produce w) = let (r, l) = runWriter w
                             in (l == expectedLog) && (r == expectedOut)
          test _ = False

test2 = mkTest "DD 2 state s1 -> s2" testDD2
testDD2 = do let (out, aut') = runAutomaton (aex2 label) (aut1 label)
                 automatonCheck = aut' == (aut2 label)
                 diagnosticCheck = test out
             tell $ withFG Blue "|" <> "   Automaton : " <> T.pack (show automatonCheck) <> eol
             tell $ withFG Blue "|" <> "       aut = " <> pack (show aut') <> eol
             tell $ withFG Blue "|" <> "   Diagnostic : " <> T.pack (show diagnosticCheck) <> eol
             tell $ withFG Blue "|" <> "       out = " <> pack (show out) <> eol
             return $ automatonCheck && diagnosticCheck
    where label = "test2"
          expectedLog = "read PASS : " <> label <> eol
                      <> logStAnOk label
          expectedOut = OutBool True
          test (Produce w) = let (r, l) = runWriter w
                             in (l == expectedLog) && (r == expectedOut)
          test _ = False

test3 = mkTest "DD 3 state s2 -> s3" testDD3
testDD3 = do let (out, aut') = runAutomaton (aex3 label) (aut2 label)
                 automatonCheck = aut' == (aut3 label)
                 diagnosticCheck = test out
             tell $ withFG Blue "|" <> "   Automaton : " <> T.pack (show automatonCheck) <> eol
             tell $ withFG Blue "|" <> "       aut = " <> pack (show aut') <> eol
             tell $ withFG Blue "|" <> "   Diagnostic : " <> T.pack (show diagnosticCheck) <> eol
             tell $ withFG Blue "|" <> "       out = " <> pack (show out) <> eol
             return $ automatonCheck && diagnosticCheck
    where label = "test3"
          expectedLog = logFinish label
          expectedOut = OutUnit
          test (Produce w) = let (r, l) = runWriter w
                             in (l == expectedLog) && (r == expectedOut)
          test _ = False

test4 = mkTest "DD 4 state s0 -> s0" testDD4
testDD4 = do let (out, aut') = runAutomaton (aex1' label) (aut0 label)
                 automatonCheck = aut' == (aut0 label)
                 diagnosticCheck = out == Failed
             tell $ withFG Blue "|" <> "   Automaton : " <> T.pack (show automatonCheck) <> eol
             tell $ withFG Blue "|" <> "       aut = " <> pack (show aut') <> eol
             tell $ withFG Blue "|" <> "   Diagnostic : " <> T.pack (show diagnosticCheck) <> eol
             tell $ withFG Blue "|" <> "       out = " <> pack (show out) <> eol
             return $ automatonCheck && diagnosticCheck
    where label = "test4"

