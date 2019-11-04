module Utils.System where

import Apecs (Has, Storage, cfoldM)
import Apecs.Core (ExplGet, ExplMembers)

import World (World, SystemW)

cfor_
  :: ( Has World IO c
     , ExplMembers IO (Storage c)
     , ExplGet IO (Storage c)
     , Monoid a
     )
  => (c -> SystemW a) -> SystemW a
cfor_ proc = cfoldM (const proc) mempty
