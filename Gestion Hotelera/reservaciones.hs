module Reservaciones where

import Control.Monad (when, unless)
import Data.Char (isSpace, toLower)
import Data.List (intercalate)
import Data.Time (getZonedTime, formatTime, defaultTimeLocale)
import Numeric (showFFloat)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)