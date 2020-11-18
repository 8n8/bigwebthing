{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydrogen where

import qualified Language.C.Inline as C
import qualified Foreign.C.Types as Ty

C.include "hydrogen.h"

init :: IO Ty.CInt
init = do
    [C.exp| int{ hydro_init() } |]
