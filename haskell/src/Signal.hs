{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Signal where

import qualified Language.C.Inline as C

C.include "crypto_provider_openssl.h"

installInit =

normalInit =
