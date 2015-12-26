module DocParser (
    parseDoc
) where 

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error
import Data.Char (isSpace)

import Doc

-- Parses a string containing the document of interest and
-- returns either a list of strings containing error message
-- or a Doc value.
parseDoc :: String -> Either [String] Doc
parseDoc str = case parse document "" str of
    Left err -> Left $ showErr err
    Right result -> Right result
    where showErr err = map messageString $ errorMessages err


-- Parses a string into a Doc value.  Leading whitespace is removed
-- and zero or more paragraphs are parsed until the end of the
-- document is reached.
document :: Parser Doc
document = do
    spaces
    paragraphs <- many paragraph
    return $ Doc paragraphs


-- A document ends with zero or more whitespace and an end of file.
documentEnd = spaces >> eof


-- Parses a string into a paragraph.  There must be no leading
-- whitespace.   Trailing whitespace is removed, provided it is
-- either the end of the document or the whitespace is a valid 
-- paragraph separator (whitespace containing two or more carriage
-- returns).
paragraph :: Parser Par
paragraph = do
    sentences <- many1 sentence
    paragraphSep
    return $ Par sentences


paragraphEnd =
    spacesNoEndOfLine >>
    endOfLine >>
    spacesNoEndOfLine >>
    endOfLine >>
    spaces


paragraphSep = try documentEnd <|> paragraphEnd


-- A sentence is one or more words followed by a period.  The period could
-- be separated from the last word in the sentence by zero or more spaces.
-- A sentence ends either with the end of the paragraph (which is
-- not consumed), the start of a new sentence (whitespace containing at
-- most one newline), or whitespace with no newlines.
sentence :: Parser Sen
sentence = do
    words <- many1 word
    punc <- punctuation
    sentenceEnd
    return $ Sen words punc


punctuation :: Parser Punc
punctuation = try period <|> try exclamation <|> question <?> "missing an exclamation point (!, ., ?)"
    where period = char '.' >> return Period
          exclamation = char '!' >> return Exclamation
          question = char '?' >> return Question


sentenceEnd = (lookAhead (try paragraphSep)) <|>
    try spacesOneEndOfLine <|>
     spacesNoEndOfLine


-- consumes all whitespace up to but not including a second newline
spacesOneEndOfLine = spacesNoEndOfLine >> endOfLine >> spacesNoEndOfLine


-- consume all whitespace that is not a newline.  Terminates at, but does not
-- consume, the first non-whitespace non-newline character encountered
spacesNoEndOfLine = skipMany (satisfy (\ch -> isSpace ch && ch /= '\n'))


-- A word is one or more characters that are not whitespace and are
-- not punctuation.  Trailing punctuation is not consumed.  Trailing
-- whitespace is removed but can contain at most one newline.
word :: Parser Word
word = do
    chars <- many1 wordChar
    wordEnd
    return $ Word chars


wordChar = satisfy (\ch -> (not (isPunctuation ch)) && (not (isSpace ch)))
    where isPunctuation ch = case (parse punctuation "" [ch]) of
        Right _ -> True
        Left _ -> False


wordEnd = try spacesOneEndOfLine <|> spacesNoEndOfLine
