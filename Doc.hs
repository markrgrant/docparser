module Doc where

data Doc         = Doc  [Par]       deriving (Eq, Show)
data Par         = Par  [Sen]       deriving (Eq, Show)
data Sen         = Sen  [Word] Punc deriving (Eq, Show)
data Word        = Word String      deriving (Eq, Show)
data Punc        = Period |
                   Exclamation |
                   Question         deriving (Eq, Show)
