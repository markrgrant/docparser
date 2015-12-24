module Doc where

data Doc         = Doc       [Paragraph]           deriving (Eq, Show)
data Paragraph   = Paragraph [Sentence]            deriving (Eq, Show)
data Sentence    = Sentence  [Word] Punctuation    deriving (Eq, Show)
data Word        = Word      String                deriving (Eq, Show)
data Punctuation = Period | Exclamation | Question deriving (Eq, Show)
