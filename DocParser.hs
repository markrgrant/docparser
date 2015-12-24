module DocParser where 

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error
import Data.Char (isSpace)

import Doc


parseDocument :: String -> Either String Document
parseDocument str = case parse document "" str of
    Left err -> Left $ showErr err
    Right result -> Right result


showErr :: ParseError -> String
showErr err = showLastErr err


showLastErr :: ParseError -> String
showLastErr err = messageString $ last $ errorMessages err


punctuationChars = "!?."  -- sentence termination characters


wordEndChars = " " ++ punctuationChars


spacesNoEndOfLine = skipMany (satisfy (\ch -> isSpace ch && ch /= '\n'))


-- A word is one or more characters that are not punctuation or whitespace.
-- Trailing whitespace is cleared.
word :: Parser Word
word = do
    chars <- many1 (noneOf wordEndChars)
    spaces
    return $ Word chars


-- A sentence is one or more words followed by a period.  The period could
-- be separated from the last word in the sentence by zero or more spaces.
-- A sentence ends either with a sentence separator (which is consumed) or 
-- with a paragraph separator (which is not consumed).
sentence :: Parser Sentence
sentence = do
    words <- many1 word
    punc <- oneOf punctuationChars
    sentenceEnd
    return $ Sentence words punc


sentenceEnd = (lookAhead (try paragraphEnd)) <|>
    try (spacesNoEndOfLine >> endOfLine >> spacesNoEndOfLine) <|> spacesNoEndOfLine


-- A paragraph consists of one or more sentences.  Paragraphs are
-- separated from each other by two newlines.
paragraph :: Parser Paragraph
paragraph = do
    sentences <- manyTill sentence (try paragraphEnd)
    return $ Paragraph sentences


-- a paragraph separator is two or more newlines occurring within whitespace.
paragraphEnd = try documentEnd <|> (spacesNoEndOfLine >> endOfLine >> spacesNoEndOfLine  >> endOfLine >> spaces)


document :: Parser Document
document = do
    spaces
    paragraphs <- manyTill  paragraph documentEnd
    return $ Document paragraphs


documentEnd = spaces >> eof
