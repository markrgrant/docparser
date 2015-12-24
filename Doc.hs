module Doc where

data Doc       = Doc       [Paragraph]          deriving (Eq, Show)
data Paragraph = Paragraph [Sentence]           deriving (Eq, Show)
data Sentence  = Sentence  [Word] Punctuation   deriving (Eq, Show)
data Word      = Word      [NonPunctuationChar] deriving (Eq, Show)
type NonPunctuationChar = Char
type Punctuation        = Char
