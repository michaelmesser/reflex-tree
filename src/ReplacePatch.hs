{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedLists, TypeFamilies, GADTs, FlexibleContexts, UndecidableInstances, StandaloneDeriving, TupleSections #-}
module ReplacePatch where

import Reflex
import Control.Lens
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Data.GADT.Compare.TH
import Data.GADT.Show.TH


data ReplacePatch a = ReplacePatch_New (PatchTarget a) | ReplacePatch_Patch a
deriving instance (Show p, Show (PatchTarget p)) => Show (ReplacePatch p)
deriving instance (Eq p, Eq (PatchTarget p)) => Eq (ReplacePatch p)

instance Patch p => Patch (ReplacePatch p) where
    type PatchTarget (ReplacePatch p) = PatchTarget p
    apply change old = case change of
        ReplacePatch_New n -> Just n
        ReplacePatch_Patch p -> apply p old

class Patch p => Diffable p where
    diff :: PatchTarget p -> PatchTarget p -> Maybe p

diffReplacePatch :: (Reflex t, MonadHold t m, Diffable p) => PatchTarget p -> Event t (ReplacePatch p) -> m (PatchTarget p, Event t p)
diffReplacePatch initial events = do
    incremental <- holdIncremental initial events
    return (initial, push (\case
        ReplacePatch_New new -> do
            old <- sample $ currentIncremental incremental
            return $ diff old new
        ReplacePatch_Patch patch -> return $ Just patch
        ) events)
    
flattenToReplacePatch :: (Patch p, MonadHold t m, Reflex t) => (PatchTarget p, Event t p) -> Event t (PatchTarget p, Event t p) -> m (PatchTarget p, Event t (ReplacePatch p))
flattenToReplacePatch initial events = do
    x <- switchPromptly (snd initial) (snd <$> events)
    return (fst initial, leftmost [ReplacePatch_New <$> (fst <$> events), ReplacePatch_Patch <$> x])
