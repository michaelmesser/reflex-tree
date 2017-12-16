{-# LANGUAGE ScopedTypeVariables, LambdaCase, OverloadedLists, TypeFamilies, GADTs, StandaloneDeriving, FlexibleContexts, UndecidableInstances, TupleSections #-}

module Util where

import Control.Lens
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Dependent.Map as DM
import Data.Dependent.Map (DMap)
import Data.Maybe

mNullToNothing :: Map k v -> Maybe (Map k v)
mNullToNothing x = if M.null x then Nothing else Just x

dmNullToNothing :: DMap k v -> Maybe (DMap k v)
dmNullToNothing x = if DM.null x then Nothing else Just x

