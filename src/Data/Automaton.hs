{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | A simple state machine where each state can be activate to produce a result
--
module Data.Automaton where

import           Control.Monad
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Maybe
import           Data.Monoid            (First (..), getFirst)
import           Data.Text              (Text, unpack)

-- | An simple automaton defined by its transitions and its current state
--
--
data Automaton i o = Automaton { getCurrentAutomatonState :: AutomatonState i o
                               , getTransitions           :: [AutomatonTransition i o]
                               }
                   deriving (Eq, Show)

newAutomaton s ts = Automaton s ts

-- | An automaton transition labelled with a value of type @a@ and that links two
--   states of type @'AutomatonState' a b@.
--

data AutomatonTransition i o = AutomatonTransition { getLabel    :: Text
                                                   , isFrom   :: AutomatonState i o
                                                   , goTo     :: AutomatonState i o
                                                   , getGuard :: i -> Bool
                                                   }

instance Eq (AutomatonTransition i o) where
    t0 == t1 =  isFrom t0 == isFrom t1
             && goTo t0 == goTo t1
             && getLabel t0 == getLabel t1

instance Show i => Show (AutomatonTransition i o) where
    show t = from ++ " --" ++ label ++ "--> " ++ to
      where from = show $ isFrom t
            label = show $ getLabel t
            to = show $ goTo t

-- | An automaton state identified by its name and that can produce a result through
--   its action (represented as a function @a -> b@.
--
data AutomatonState i o = AutomatonState { getName   :: Text
                                         , getAction :: i -> o
                                         }

instance Eq (AutomatonState a b) where
    s1 == s2 = getName s1 == getName s2

instance Show (AutomatonState a b) where
    show = unpack . getName

-- | 'selectAutomatonState'
--
selectAutomatonState :: (Eq i, Traversable t) => t (AutomatonTransition i o)
                                              -> AutomatonState i o
                                              -> i
                                              -> Maybe (AutomatonState i o)
selectAutomatonState transitions currentAutomatonState label = state
    where -- candidate :: First ( Maybe (AutomatonState a b ))
          candidate = foldMap (First . destination currentAutomatonState label) transitions
          -- newAutomatonState :: Maybe (AutomatonState a b)
          state = getFirst candidate

-- | 'destination'
--
destination :: (Eq i) => AutomatonState i o
                      -> i
                      -> AutomatonTransition i o
                      -> Maybe (AutomatonState i o)
destination currentAutomatonState input transition
    | (getGuard transition) input && isFrom transition == currentAutomatonState = Just $ goTo transition
    | otherwise                                                                 = Nothing

-- | 'runCurrentAutomatonStateWith'
--
runCurrentAutomatonStateWith :: Automaton i o
                             -> i
                             -> o
runCurrentAutomatonStateWith aut x = getAction (getCurrentAutomatonState aut) x

-- | AutomatonExecutionT
--
type AutomatonExecutionT i o m p = ReaderT [AutomatonTransition i o]
                                           (StateT (AutomatonState i o) m)
                                           p

runAutomatonT :: Monad m => AutomatonExecutionT i o m p
              -> Automaton i o
              -> m (p, Automaton i o)
runAutomatonT e a = run >>= return . \ (x,state) -> (x,Automaton state transitions)
    where run = runStateT (runReaderT e transitions) current
          current = getCurrentAutomatonState a
          transitions = getTransitions a

evalAutomatonT :: Monad m => AutomatonExecutionT i o m p
                          -> Automaton i o
                          -> m p
evalAutomatonT e = liftM fst . runAutomatonT e

execAutomatonT :: Monad m => AutomatonExecutionT i o m p
                          -> Automaton i o
                          -> m (Automaton i o)
execAutomatonT e = liftM snd . runAutomatonT e

-- | AutomatonExecution
--
type AutomatonExecution i o p = AutomatonExecutionT i o Identity p

runAutomaton :: AutomatonExecution i o p
             -> Automaton i o
             -> (p, Automaton i o)
runAutomaton e a = runIdentity $ runAutomatonT e a

evalAutomaton :: AutomatonExecution i o p
              -> Automaton i o
              -> p
evalAutomaton e a = runIdentity $ evalAutomatonT e a

execAutomaton :: AutomatonExecution i o p
              -> Automaton i o
              -> Automaton i o
execAutomaton e a = runIdentity $ execAutomatonT e a

-----------------------
-- Evaluation
--

-- | Evaluation Output
--

data Outcome o = Failed
               | Succeed
               | Produce o
    deriving (Eq, Show)

-- | 'transit'
--
transit :: Eq i
        => i
        -> AutomatonExecution i o (Bool, AutomatonState i o)
transit l = do transitions <- ask
               fromState <- get
               let newStateCandidate = selectAutomatonState transitions fromState l
                   haveTransited = isJust newStateCandidate
               when haveTransited $ put $ fromJust newStateCandidate
               return (haveTransited, fromState)

-- | 'activate'
--
activate :: i
         -> AutomatonExecution i o o
activate l = do current <- get
                return $ getAction current l

-- | 'evaluateProgress'
--
evaluateProgress :: Eq i
                 => i
                 -> AutomatonExecution i o (Outcome o)
evaluateProgress l = do result <- transit l
                        current <- get
                        activateIfNewState current result
    where activateIfNewState current (True, from)
            | current == from = return Succeed
            | otherwise       = activate l >>= return . Produce
          activateIfNewState _ (False, _) = return Failed

-- | 'evaluate'
--
evaluate :: Eq i
         => i
         -> AutomatonExecution i o (Outcome o)
evaluate l = do result <- transit l
                if fst result then activate l >>= return . Produce
                          else return Failed

