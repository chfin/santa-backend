{-# LANGUAGE OverloadedStrings #-}
module WriteAPI where

import Wish (writeJS)
import System.Environment
import qualified Data.Text as T
import Data.Maybe (listToMaybe)

main = do
  args <- getArgs
  writeJS "static/api.js" $ maybe "/santa" T.pack (listToMaybe args)
