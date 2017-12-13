{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedLists, TypeFamilies, GADTs, StandaloneDeriving, FlexibleContexts, UndecidableInstances, TupleSections #-}
module DeepPatchMap where

import Reflex
import Control.Lens
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Control.Monad.Fix

data DeepPatchMapRowChange p = DeepPatchMapRowChange_Delete | DeepPatchMapRowChange_Patch p | DeepPatchMapRowChange_New (PatchTarget p)
deriving instance (Show p, Show (PatchTarget p)) => Show (DeepPatchMapRowChange p)
deriving instance (Eq p, Eq (PatchTarget p)) => Eq (DeepPatchMapRowChange p)

newtype DeepPatchMap k p = DeepPatchMap { unDeepPatchMap :: Map k (DeepPatchMapRowChange p) }
deriving instance (Show k, Show p, Show (PatchTarget p)) => Show (DeepPatchMap k p)
deriving instance (Eq k, Eq p, Eq (PatchTarget p)) => Eq (DeepPatchMap k p)

instance (Ord k, Patch p) => Patch (DeepPatchMap k p) where
    type PatchTarget (DeepPatchMap k p) = Map k (PatchTarget p)
    apply (DeepPatchMap p) old = Just $! s3
        where s1 = old `M.difference` deletions
              s2 = insertions `M.union` s1
              s3 = M.differenceWith (flip apply) s2 modifications
              insertions = M.mapMaybeWithKey (const $ \case (DeepPatchMapRowChange_New a) -> Just a; _ -> Nothing) p
              deletions = M.mapMaybeWithKey (const $ \case DeepPatchMapRowChange_Delete -> Just (); _ -> Nothing) p
              modifications = M.mapMaybeWithKey (const $ \case (DeepPatchMapRowChange_Patch a) -> Just a; _ -> Nothing) p

qw :: (Ord k, Patch p, MonadFix m, MonadHold t m, Reflex t) => Event t (DeepPatchMap k p) -> k -> DeepPatchMapRowChange p -> Maybe (m (Maybe (PatchTarget p, Event t p)))
qw events key = \case
    DeepPatchMapRowChange_New value -> Just $ Just <$> do
        es <- tailE events
        (value,) <$> getUpdatesForKey key es
    DeepPatchMapRowChange_Delete -> Just $ return Nothing
    DeepPatchMapRowChange_Patch _ -> Nothing

convert :: (Ord k, Patch p, Reflex t, MonadHold t m) => Map k (PatchTarget p) -> Event t (DeepPatchMap k p) -> m (Map k (PatchTarget p, Event t p), Event t (PatchMap k (PatchTarget p, Event t p)))
convert initial events = do
    newInitial <- sequence $ M.mapWithKey (\key value -> (value,) <$> getUpdatesForKey key events) initial
    let newEvents = push (fmap (fmap PatchMap) . fmap (\m -> if M.null m then Nothing else Just m) . sequence . M.mapMaybeWithKey (qw events) . unDeepPatchMap) events
    return (newInitial, newEvents)

getUpdatesForKey :: (MonadHold t m, Reflex t, Ord k) => k -> Event t (DeepPatchMap k p) -> m (Event t p) 
getUpdatesForKey key events = events <&> unDeepPatchMap & fmapMaybe (M.lookup key) <&> (\case (DeepPatchMapRowChange_Patch patch) -> Just patch; _ -> Nothing) & stopOnNothing

stopOnNothing :: (Reflex t, MonadHold t m) => Event t (Maybe a) -> m (Event t a)
stopOnNothing e = switchPromptly (fmapMaybe id e) (never <$ ffilter isNothing e)