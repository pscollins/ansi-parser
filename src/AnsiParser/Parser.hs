{-# LANGUAGE OverloadedStrings, FlexibleContexts  #-}
module AnsiParser.Parser where

import AnsiParser.Types

-- import Text.ParserCombinators.Parsec.Token
-- import Text.Parsec
-- import Text.Parsec.Char (oneOf, satisfy)
-- import Text.Parsec.ByteString (Parser)

-- import Data.String (IsString)
-- -- import Data.ByteString.Char8 (ByteString)

-- import Data.Char (isControl)



-- -- isEscapeCode :: IsString a => a -> Bool
-- -- isEscapeCode = (`elem` "\x027")

-- -- TODO: Other option here?
-- escapeCode :: (ConsoleString a, Stream s m a) => ParsecT s u m a
-- escapeCode = satisfy isEscape

-- -- commandBody :: (Stream s m Char) => ParsecT s u m Char
-- -- commandBody = satisfy $ not . isControl

-- -- -- TODO: set reservedNames to hold the single-character codes
-- -- ansiTokenParser :: (IsString a, Stream s m a) => GenTokenParser s u m
-- -- ansiTokenParser = makeTokenParser $
-- --                   LanguageDef { commentStart = ""
-- --                               , commentEnd = ""
-- --                               , commentLine = ""
-- --                               , nestedComments = False
-- --                               , identStart = parserZero
-- --                               , identLetter = parserZero
-- --                               , opStart = escapeCode
-- --                               , opLetter = commandBody
-- --                               , reservedNames = []
-- --                               , reservedOpNames = []
-- --                               , caseSensitive = True }

-- -- plainText :: Parser (Expr a)
-- -- plainText = do
-- --   body <- manyTill anyToken escapeCode
-- --   return $ Plain body


-- -- tokenParser :: Stream s m Char => GenLanguageDef s u m -> GenTokenParser s u m
-- -- tokenParser = makeTokenParser
