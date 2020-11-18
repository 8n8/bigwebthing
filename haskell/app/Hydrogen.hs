{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydrogen (init_) where

import qualified Language.C.Inline as C
import qualified Foreign.C.Types as Ty

C.include "hydrogen.h"

init_ :: IO Ty.CInt
init_ =
    [C.exp| int{ hydro_init() } |]
