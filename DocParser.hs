module DocParser (
    parseDoc
) where 

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error
import Data.Char (isSpace)

import Doc


-- Parses a string containing the document of interest and
-- returns either a string containing an error message if the
-- document is not valid, or a Doc value.
parseDoc :: String -> Either String Doc
parseDoc str = case parse document "" str of
    Left err -> Left $ showErr err
    Right result -> Right result
    where
        showErr :: ParseError -> String
        showErr err = showLastErr err

        showLastErr :: ParseError -> String
        showLastErr err = messageString $ last $ errorMessages err


-- Parses a string into a Doc value.  Leading whitespace is removed
-- and zero or more paragraphs are parsed until the end of the
-- document is reached.
document :: Parser Doc
document = do
    spaces
    paragraphs <- manyTill paragraph documentEnd
    return $ Doc paragraphs


-- A document ends with optional whitespace.
documentEnd = spaces >> eof


-- Parses a string into a paragraph.  There must be no leading
-- whitespace.   Trailing whitespace is removed, provided it is
-- either the end of the document or the whitespace is a valid 
-- paragraph separator (whitespace containing two or more carriage
-- returns).
paragraph :: Parser Paragraph
paragraph = do
    sentences <- manyTill sentence (try paragraphEnd)
    return $ Paragraph sentences


paragraphSep =
    spacesNoEndOfLine >>
    endOfLine >>
    spacesNoEndOfLine >>
    endOfLine >>
    spaces


paragraphEnd = try documentEnd <|> paragraphSep


-- A sentence is one or more words followed by a period.  The period could
-- be separated from the last word in the sentence by zero or more spaces.
-- A sentence ends either with the end of the paragraph (which is
-- not consumed), the start of a new sentence (whitespace containing at
-- most one newline), or whitespace with no newlines.
sentence :: Parser Sentence
sentence = do
    words <- many1 word
    punc <- punctuation
    sentenceEnd
    return $ Sentence words punc


sentenceEnd = (lookAhead (try paragraphEnd)) <|>
    try spacesNoEndOfLine <|>
     spacesNoEndOfLine

sentenceSep = spacesNoEndOfLine >> endOfLine >> spacesNoEndOfLine

spacesNoEndOfLine = skipMany (satisfy (\ch -> isSpace ch && ch /= '\n'))


-- A word is one or more characters that are not punctuation or whitespace.
-- Trailing whitespace is cleared.
word :: Parser Word
word = do
    chars <- many1 (noneOf wordEnd)
    spaces
    return $ Word chars

punctuation :: Parser Punctuation
punctuation = try period <|> try exclamation <|> question
    where period = char '.' >> return Period
          exclamation = char '!' >> return Exclamation
          question = char '?' >> return Question

punctuationChars = "!?."

wordEnd = " " ++ punctuationChars
