module Utils.Data.Text.TypeAlias
    ( Reason
    , Usage
    , Description
    , ProgName
) where

import Data.Text (Text)
import Utils.TypeAlias (Typing (..))

{- A `Reason` is just a brief explanation of something. -}
data Reason_
type Reason = Reason_ `Typing` Text

{- A `Usage` is just an explanation of what something has to be used. -}
data Usage_
type Usage = Usage_ `Typing` Text

{- A `Description` is literally a description. -}
data Description_
type Description = Description_ `Typing` Text

{- A `ProgName` is the name of a program. -}
data ProgName_
type ProgName = ProgName_ `Typing` Text
