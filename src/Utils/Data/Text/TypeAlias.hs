{- |
Module : Utils.Data.Text.TypeAlias
Description : Text-based typed alias
Copyright : (c) Luca Borghi, 2023
License : GPL-3
Stability : experimental

Typed aliases based @Text@ data type.
-}

module Utils.Data.Text.TypeAlias
    (
    -- * Aliases
      Reason
    , Usage
    , Description
    , ProgName
    -- * Constructors
    , reason
    , usage
    , description
    , progName
) where

import Data.Text (Text)
import Utils.TypeAlias (Typing (..))

{- |
A `Reason` is just a brief explanation of something.
-}
data Reason_
type Reason = Reason_ `Typing` Text

reason :: Text -> Reason
reason = Typing

{- |
A `Usage` is just an explanation of what something has to be used.
-}
data Usage_
type Usage = Usage_ `Typing` Text

usage :: Text -> Usage
usage = Typing

{- |
A `Description` is literally a description.
-}
data Description_
type Description = Description_ `Typing` Text

description :: Text -> Description
description = Typing

{- |
A `ProgName` is the name of a program.
-}
data ProgName_
type ProgName = ProgName_ `Typing` Text

progName :: Text -> ProgName
progName = Typing
