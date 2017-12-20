{-# LANGUAGE RecursiveDo, ScopedTypeVariables, OverloadedStrings, LambdaCase, TupleSections, OverloadedLists, TemplateHaskell, TypeFamilies, GADTs, PartialTypeSignatures, RankNTypes, StandaloneDeriving, MultiParamTypeClasses, FlexibleContexts, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
import Reflex.Dom
import Control.Lens
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Dependent.Map as DM
import Data.Dependent.Map (DMap)
import ReplacePatch
import DeepPatchMap
import Data.Dependent.Sum
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Util

type MapKey = Int

newtype SectionedItemsPatch =  SectionedItemsPatch { unSectionedItemsPatch :: DMap SectionedItemsTag Identity } deriving Show

data SectionedItemsTag a where
    SectionedItems_NameTag :: SectionedItemsTag Text
    SectionedItems_ItemsTag :: SectionedItemsTag (PatchMap MapKey Text)
    SectionedItems_SubsectionsTag :: SectionedItemsTag (DeepPatchMap MapKey SectionedItemsPatch)

instance ShowTag SectionedItemsTag Identity where
    showTaggedPrec SectionedItems_NameTag = showsPrec
    showTaggedPrec SectionedItems_ItemsTag = showsPrec
    showTaggedPrec SectionedItems_SubsectionsTag = showsPrec

data SectionedItems = SectionedItems
    { _sectionedItemsName :: Text
    , _sectionedItemsItems :: Map MapKey Text
    , _sectionedItemsSubsections :: Map MapKey SectionedItems
    } deriving (Eq, Show)

deriveGEq ''SectionedItemsTag
deriveGCompare ''SectionedItemsTag
deriveGShow ''SectionedItemsTag
makeFields ''SectionedItems

instance Patch SectionedItemsPatch where
    type PatchTarget  SectionedItemsPatch = SectionedItems
    apply (SectionedItemsPatch mods) orig =
        if DM.null mods
        then Nothing
        else Just . foldl (&) orig . catMaybes $
            [ (name .~) <$> (runIdentity <$> DM.lookup SectionedItems_NameTag mods)
            , (\change -> items %~ (\x -> fromMaybe x $ apply change x)) . runIdentity <$> DM.lookup SectionedItems_ItemsTag mods
            , (\change -> subsections %~ (\x -> fromMaybe x $ apply change x)) . runIdentity <$> DM.lookup SectionedItems_SubsectionsTag mods
            ]

instance Diffable SectionedItemsPatch where
    diff old new = fmap SectionedItemsPatch . dmNullToNothing . DM.fromList . catMaybes $
        [ (SectionedItems_NameTag :=>) . Identity <$> if old^.name == new^.name then Nothing else Just (new^.name)
        , (SectionedItems_ItemsTag :=>) . Identity <$> diff (old^.items) (new^.items)
        , (SectionedItems_SubsectionsTag :=>) . Identity <$> diff (old^.subsections) (new^.subsections)
        ]

instance (Ord k, Eq v) => Diffable (PatchMap k v) where
    diff old new = fmap PatchMap . mNullToNothing $ diffMap old new

data SectionedItemsDynamic t = SectionedItemsDynamic
    { _sectionedItemsDynamicName :: Dynamic t Text
    , _sectionedItemsDynamicItems :: Dynamic t (Incremental t (PatchMap MapKey Text))
    , _sectionedItemsDynamicSubsections :: Dynamic t (Incremental t (DeepPatchMap MapKey SectionedItemsPatch))
    }

makeFields ''SectionedItemsDynamic

data SectionedItemsEvent t = SectionedItemsEvent
    { _sectionedItemsEventName :: Event t Text
    , _sectionedItemsEventItems :: Event t (PatchMap MapKey Text)
    , _sectionedItemsEventSubsections :: Event t (DeepPatchMap MapKey SectionedItemsPatch)
    }

makeFields ''SectionedItemsEvent

mergeSectionedItemsEvents :: Reflex t => SectionedItemsEvent t -> Event t SectionedItemsPatch
mergeSectionedItemsEvents r = SectionedItemsPatch <$> merge (DM.fromList
        [ SectionedItems_NameTag :=> _sectionedItemsEventName r 
        , SectionedItems_ItemsTag :=> _sectionedItemsEventItems r
        , SectionedItems_SubsectionsTag :=> _sectionedItemsEventSubsections r
        ])

fanSectionedItemsEvents :: Reflex t => Event t SectionedItemsPatch -> SectionedItemsEvent t
fanSectionedItemsEvents e = SectionedItemsEvent
    { _sectionedItemsEventName = select x SectionedItems_NameTag
    , _sectionedItemsEventItems = select x SectionedItems_ItemsTag
    , _sectionedItemsEventSubsections = select x SectionedItems_SubsectionsTag
    } where
        x = fan (fmap unSectionedItemsPatch e)

sectionItemsWidget :: MonadWidget t m => SectionedItems -> Event t SectionedItemsPatch -> m (Event t (Maybe SectionedItemsPatch))
sectionItemsWidget initial events = elAttr "div" [("style", "border: 1px solid black; margin: 10px")] $ do
    let fe = fanSectionedItemsEvents events
    (isi, esi) <- convert (initial^.subsections) (fe^.subsections)
    itemsIncremental <- holdIncremental (initial^.items) (fe^.items) -- Maybe find be way to make the next id
    subsectionsIncremental <- holdIncremental (initial^.subsections) (fe^.subsections)
    nameEvents <- nameWidget (initial^.name) (fe^.name)
    (itemsEvents, addItemEvent, subsectionsEvents, addSectionEvent) <- elAttr "div" [("style", "margin-left: 1cm")] $ do
        itemsEvents <- listWithKeyShallowDiff (initial^.items) (unPatchMap <$> (fe^.items)) (const itemWidget)
        addItemEvent <- itemAdderWidget
        subsectionsEvents <- listWithKeyShallowDiff isi (unPatchMap <$> esi) (\k i e -> do
            n <- uncurry diffReplacePatch =<< flattenToReplacePatch i e
            uncurry sectionItemsWidget n
            )
        addSectionEvent <- sectionAdderWidget
        return (itemsEvents, addItemEvent, subsectionsEvents, addSectionEvent)
    return $ leftmost
        [ Just <$> mergeSectionedItemsEvents SectionedItemsEvent
            { _sectionedItemsEventName = nameEvents & fmapMaybe id
            , _sectionedItemsEventItems = leftmost
                [ switch . current . fmap (fmap PatchMap . mergeMap) $ itemsEvents
                , pushAlways (\x -> do
                    cMap <- sample $ currentIncremental itemsIncremental
                    return . PatchMap . M.singleton (maybe 0 (succ . fst . fst) . M.maxViewWithKey $ cMap) . Just $ x
                  ) addItemEvent
                ]
            , _sectionedItemsEventSubsections = leftmost
                [ switch . current . fmap (fmap (DeepPatchMap . fmap (fmap ReplacePatch_Patch)) . mergeMap) $ subsectionsEvents
                , pushAlways (\x -> do
                    cMap <- sample $ currentIncremental subsectionsIncremental
                    return . DeepPatchMap . M.singleton (maybe 0 (succ . fst . fst) . M.maxViewWithKey $ cMap) . Just . ReplacePatch_New $ SectionedItems { _sectionedItemsName = x, _sectionedItemsItems = [], _sectionedItemsSubsections = []}
                  ) addSectionEvent
                ]
            }
        , fmapMaybe (\case Just x -> Nothing; Nothing -> Just Nothing) nameEvents
        ]

nameWidget :: MonadWidget t m => Text -> Event t Text -> m (Event t (Maybe Text))
nameWidget initial events = el "div" $ do
    updates <- elAttr "span" [("style", "font-weight: bold")] $ editableTextWidget initial events
    delete <- button "Delete Section"
    return $ leftmost [Nothing <$ delete, Just <$> updates]

itemWidget :: MonadWidget t m => Text -> Event t Text -> m (Event t (Maybe Text))
itemWidget initial events = el "div" $ do
    updates <- editableTextWidget initial events
    delete <- button "Delete Item"
    return $ leftmost [Nothing <$ delete, Just <$> updates]

editableTextWidget :: MonadWidget t m => Text -> Event t Text -> m (Event t Text)
editableTextWidget initial events = el "span" $ do
    el "span" $ dynText =<< holdDyn initial events
    rec i <- textInput $ def & textInputConfig_setValue .~ ("" <$ b)
        b <- button "Save"
    return $ tag (current . _textInput_value $ i) b

itemAdderWidget :: MonadWidget t m => m (Event t Text)
itemAdderWidget = el "div" $ do
    rec i <- textInput $ def & textInputConfig_setValue .~ ("" <$ b)
        b <- button "Add Item"
    return $ tag (current . _textInput_value $ i) b
    
sectionAdderWidget :: MonadWidget t m => m (Event t Text)
sectionAdderWidget = el "div" $ do
    rec i <- textInput $ def & textInputConfig_setValue .~ ("" <$ b)
        b <- button "Add Section"
    return $ tag (current . _textInput_value $ i) b

initial :: SectionedItems
initial = SectionedItems
    { _sectionedItemsName = "Root 0"
    , _sectionedItemsItems =
        [ (0, "Item 0")
        , (1, "Item 1")
        ]
    , _sectionedItemsSubsections =
        [ (0, SectionedItems
            { _sectionedItemsName = "Root 1"
            , _sectionedItemsItems =
                [ (0, "Item 2")
                , (1, "Item 3")
                ]
            , _sectionedItemsSubsections = []
            })
        , (1, SectionedItems
            { _sectionedItemsName = "Root 2"
            , _sectionedItemsItems =
                [ (0, "Item 4")
                , (1, "Item 5")
                ]
            , _sectionedItemsSubsections = []
            })
        ]
    }

main = mainWidget $ do
    rec
        let ea = fmapMaybe id $ leftmost [e1,e2]
        e1 <- sectionItemsWidget initial ea
        e2 <- sectionItemsWidget initial ea
    return ()