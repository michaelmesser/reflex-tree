{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedLists, TypeFamilies, GADTs, FlexibleContexts, UndecidableInstances, StandaloneDeriving #-}
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

getIncremental :: (MonadHold t m, Reflex t, Patch p) => PatchTarget p -> Event t (ReplacePatch p) -> m (Incremental t p)
getIncremental value events = do
    x <- events <&> (\case (ReplacePatch_Patch patch) -> Just patch; _ -> Nothing) & stopOnNothing
    holdIncremental value x

replacePatchToDyn :: (MonadHold t m, Reflex t, Patch a) => Incremental t (ReplacePatch a) -> m (Dynamic t (Incremental t a))
replacePatchToDyn i = do
    initial <- sample $ currentIncremental i
    let events = updatedIncremental i
    newInitial <- getIncremental initial events
    let newEvents = events & push (\case
            ReplacePatch_New value -> Just <$> do
                e <- tailE events
                getIncremental value e
            ReplacePatch_Patch _ -> return Nothing)
    holdDyn newInitial newEvents

--switchPromptly or switchPromptOnly to go the other way

stopOnNothing :: (Reflex t, MonadHold t m) => Event t (Maybe a) -> m (Event t a)
stopOnNothing e = switchPromptly (fmapMaybe id e) (never <$ ffilter isNothing e)
