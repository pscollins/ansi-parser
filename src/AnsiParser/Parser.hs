{-# LANGUAGE OverloadedStrings, FlexibleContexts  #-}
module AnsiParser.Parser where

import Text.ParserCombinators.Parsec.Token
import Text.Parsec
import Text.Parsec.Char (oneOf, satisfy)
import Text.Parsec.ByteString (Parser)

import Data.ByteString.Char8 (ByteString)

import Data.Char (isControl)

escapeCode :: (Stream s m Char) => ParsecT s u m Char
escapeCode = oneOf "\x027"

commandBody :: (Stream s m Char) => ParsecT s u m Char
commandBody = satisfy $ not . isControl

-- TODO: set reservedNames to hold the single-character codes
ansiTokenParser :: Stream s m Char => GenTokenParser s u m
ansiTokenParser = makeTokenParser $
                  LanguageDef { commentStart = ""
                              , commentEnd = ""
                              , commentLine = ""
                              , nestedComments = False
                              , identStart = parserZero
                              , identLetter = parserZero
                              , opStart = escapeCode
                              , opLetter = commandBody
                              , reservedNames = []
                              , reservedOpNames = []
                              , caseSensitive = True }

plainText :: Parser ByteString
plainText = undefined

-- tokenParser :: Stream s m Char => GenLanguageDef s u m -> GenTokenParser s u m
-- tokenParser = makeTokenParser
