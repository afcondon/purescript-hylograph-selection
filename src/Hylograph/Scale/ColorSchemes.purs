-- | Hylograph.Scale.ColorSchemes — Categorical color palettes
-- |
-- | Pure PureScript color scheme arrays. No D3 dependency.
-- | Colors sourced from D3's d3-scale-chromatic (Apache 2.0 licensed).
module Hylograph.Scale.ColorSchemes
  ( -- * Categorical Schemes
    schemeCategory10
  , schemeTableau10
  , schemePaired
  , schemeSet1
  , schemeSet2
  , schemeSet3
  , schemeAccent
  , schemeDark2
  , schemePastel1
  , schemePastel2
  -- * Index-based Access
  , schemeCategory10At
  , schemeTableau10At
  , schemePairedAt
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)

-- =============================================================================
-- Categorical Color Schemes
-- =============================================================================
-- Colors from d3-scale-chromatic (Apache 2.0)

-- | Tableau 10 — the default categorical palette
-- | Designed by Tableau for data visualization
schemeCategory10 :: Array String
schemeCategory10 =
  [ "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"
  , "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  ]

-- | Tableau 10 — the modern Tableau palette
-- | Slightly softer than Category10
schemeTableau10 :: Array String
schemeTableau10 =
  [ "#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f"
  , "#edc949", "#af7aa1", "#ff9da7", "#9c755f", "#bab0ab"
  ]

-- | Paired — 12 colors in 6 pairs (light/dark)
schemePaired :: Array String
schemePaired =
  [ "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c"
  , "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928"
  ]

-- | Set1 — 9 bold colors
schemeSet1 :: Array String
schemeSet1 =
  [ "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"
  , "#ffff33", "#a65628", "#f781bf", "#999999"
  ]

-- | Set2 — 8 pastel colors
schemeSet2 :: Array String
schemeSet2 =
  [ "#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"
  , "#ffd92f", "#e5c494", "#b3b3b3"
  ]

-- | Set3 — 12 light colors
schemeSet3 :: Array String
schemeSet3 =
  [ "#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462"
  , "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f"
  ]

-- | Accent — 8 accent colors
schemeAccent :: Array String
schemeAccent =
  [ "#7fc97f", "#beaed4", "#fdc086", "#ffff99"
  , "#386cb0", "#f0027f", "#bf5b17", "#666666"
  ]

-- | Dark2 — 8 dark colors
schemeDark2 :: Array String
schemeDark2 =
  [ "#1b9e77", "#d95f02", "#7570b3", "#e7298a"
  , "#66a61e", "#e6ab02", "#a6761d", "#666666"
  ]

-- | Pastel1 — 9 light pastel colors
schemePastel1 :: Array String
schemePastel1 =
  [ "#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6"
  , "#ffffcc", "#e5d8bd", "#fddaec", "#f2f2f2"
  ]

-- | Pastel2 — 8 light pastel colors
schemePastel2 :: Array String
schemePastel2 =
  [ "#b3e2cd", "#fdcdac", "#cbd5e8", "#f4cae4"
  , "#e6f5c9", "#fff2ae", "#f1e2cc", "#cccccc"
  ]

-- =============================================================================
-- Index-based Access (wrapping with modulo)
-- =============================================================================

schemeCategory10At :: Int -> String
schemeCategory10At i = indexScheme schemeCategory10 i

schemeTableau10At :: Int -> String
schemeTableau10At i = indexScheme schemeTableau10 i

schemePairedAt :: Int -> String
schemePairedAt i = indexScheme schemePaired i

indexScheme :: Array String -> Int -> String
indexScheme scheme i =
  let len = Array.length scheme
      idx = if len == 0 then 0 else i `mod` len
  in fromMaybe "#999999" (Array.index scheme idx)
