module DocParser (
    parseDoc
) where 

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error
import Data.Char (isSpace)
import Data.Either (isRight)

import Doc

-- notes:
-- manyTill p1 p2 first tests the terminating parser p2.  If p2
-- succeeds, the matching input is consumed and the list of matches 
-- of p1 is returned (if it fails, any partial match is still
-- consumed, so if partial matches are possible and consuming
-- could interfere with the parsing of p1, p2 should be surrounded
-- by try). Parser p1 is then applied.


-- Parses a string containing the document of interest and
-- returns either a list of strings containing error message if the
-- document is not valid, or a Doc value.
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
    sentences <- manyTill sentence (try paragraphSep)
    return $ Paragraph sentences


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
sentence :: Parser Sentence
sentence = do
    words <- many1 word
    punc <- punctuation
    sentenceEnd
    return $ Sentence words punc


punctuation :: Parser Punctuation
punctuation = try period <|> try exclamation <|> question
    where period = char '.' >> return Period
          exclamation = char '!' >> return Exclamation
          question = char '?' >> return Question


sentenceEnd = (lookAhead (try paragraphEnd)) <|>
    try spacesNoEndOfLine <|>
     spacesNoEndOfLine


sentenceSep = spacesOneEndOfLine


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
    where isPunctuation ch = isRight $ (parse punctuation "" [ch])

wordEnd = try spacesOneEndOfLine <|> spacesNoEndOfLine
