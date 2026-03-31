-- | Chapter 6: The Meta Fold
-- |
-- | The finale: an interpreter that produces HATS from HATS.
-- | The metatree they've been seeing all along is itself a HATS tree.
module Chapters.Chapter6
  ( diagramTree
  , metaTree
  ) where

import Prelude

import Hylograph.HATS (Tree)
import Chapters.Chapter3 as Ch3
import Examples.MetaHATS as Meta

-- | The "nice diagram" — the completed function diagram from Chapter 3
diagramTree :: Tree
diagramTree = Ch3.stageTree Ch3.Stage4

-- | The meta interpreter output — a HATS tree describing the structure
metaTree :: Tree
metaTree = Meta.interpretMeta Meta.defaultMetaConfig diagramTree
