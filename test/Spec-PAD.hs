{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad   ((>=>))

import           Data.Text.Utils

import           Data.Pad

import           Test.Lib

main :: IO ()
main = run test1 >> return ()

-----------------------
-- TESTS
--

-- TEST 1

test1 = mkTest "PAD 1 load -> validate" testPAD1

testPAD1 = do let motPAD = "pad"
                  motPASS = "pass(" <> motPAD <> ")"
                  messageLoad = "loading" <> eol
                  messageValidate = "validating" <> eol
                  expectedDiagnostic = "load PAD : " <> motPAD <> eol
                                     <> messageLoad
                                     <> messageValidate
                                     <> "read PAD : " <> motPAD <> eol
                                     <> "read PASS : " <> motPASS <> eol
              tell $ "load and validate"
              let pad = PAD motPAD -- creating the PAD
                  -- processing and acquiring
                  diagnostic = getDiagnostic pad $ load messageLoad >=> validate messageValidate
              return $ diagnostic == expectedDiagnostic

