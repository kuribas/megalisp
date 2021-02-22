{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Data.Lisp (Number(..), SourceRange(..), Lisp(..), parseLisp,
                  parseLispFile, parseLispExpr, showLispPos, CharParser,
                  lispParser) where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text(Text)
import Control.Applicative hiding (some, many)
import Data.Ratio
import Data.Char
import Control.Monad
import Data.Complex
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data Number =
  Integer Integer |
  SingleFloat Float |
  DoubleFloat Double |
  NumRatio (Ratio Integer) |
  ComplexDouble (Complex Double)

replaceChar :: Char -> Char -> String -> String
replaceChar _ _ [] = []
replaceChar from to (c:cs)
  | c == from = to:replaceChar from to cs
  | otherwise = c:replaceChar from to cs

specialChars :: String
specialChars = "()#\"\\,'`| ;" 

instance Show Number where
  show (Integer i) = show i
  show (SingleFloat f) = replaceChar 'e' 's' $ show f
  show (DoubleFloat f)
    | 'd' `elem` str = str
    | otherwise = str ++ "d0"
    where str = replaceChar 'e' 'd' $ show f
  show (NumRatio r) = show (numerator r) ++ "/" ++ show (denominator r)
  show (ComplexDouble (a :+ b)) = "#(" ++ show a ++ " " ++ show b ++ ")"

-- | A position range in the Lisp source file or string.
data SourceRange = SourceRange
  { sourceFrom :: SourcePos
  , sourceTo :: SourcePos
  }

instance Show SourceRange where
  show (SourceRange from to) =
    "<" ++ showPos from ++ "," ++ showPos to ++ ">"
    where showPos (SourcePos _ l c) =
            show (unPos l) ++ ":" ++ show (unPos c)

data Lisp =
  LispString Text SourceRange |
  LispNumber Number SourceRange |
  LispSymbol Text SourceRange |
  LispVector [Lisp] SourceRange |
  LispList [Lisp] SourceRange |
  LispDotList [Lisp] Lisp SourceRange

instance Show Lisp where
  show (LispString t _) = show t
  show (LispNumber n _) = show n
  show (LispSymbol s _)
    | Text.null s = "||"
    | Text.any (`elem` specialChars) s = '|': Text.unpack s ++ "|"
    | otherwise = Text.unpack s
    
  show (LispVector l _) =
    "#(" ++ unwords (map show l) ++ ")"
  show (LispList l _) = "(" ++ unwords (map show l) ++ ")"
  show (LispDotList l e _) =
    "(" ++ unwords (map show l) ++ " . " ++ show e ++ ")"

-- | show the lisp with position info
showLispPos :: Lisp -> String
showLispPos (LispString t p) = show t ++ show p
showLispPos (LispNumber n p) = show n ++ show p
showLispPos (LispSymbol s p)
  | Text.null s = "||" ++ show p
  | Text.any (`elem` specialChars) s = '|': Text.unpack s ++ "|" ++ show p
  | otherwise = Text.unpack s ++ show p
showLispPos (LispVector l p) =
  "#(" ++ unwords (map showLispPos l) ++ ")" ++ show p
showLispPos (LispList l p) = "(" ++ unwords (map showLispPos l) ++ ")" ++ show p
showLispPos (LispDotList l e p) =
  "(" ++ unwords (map showLispPos l) ++ " . " ++ showLispPos e ++ ")" ++ show p

instance Read Lisp where
  readsPrec _ input =
    case runParser' (whiteSpace >> withSourceRange lispExprP) $
         State input 0 (PosState input 0 (initialPos "read") (mkPos 0) []) [] of
      (_, Left _) -> []
      (rest, Right r) -> [(r, stateInput rest)]

dummyRange :: SourceRange
dummyRange = SourceRange (initialPos "dummy") (initialPos "dummy")

-- | A megaparsec parser that has characters as tokens.
type CharParser t a = (Stream t, Token t ~ Char
#if MIN_VERSION_megaparsec(9,0,0)
                      , TraversableStream t
#endif
                      ) => Parsec Void t a

-- | A megaparsec parser for lisp expressions
lispParser :: CharParser t Lisp
lispParser = withSourceRange lispExprP

-- | Parse a lisp file
parseLispFile :: String -> IO (Either (ParseErrorBundle Text Void) [Lisp])
parseLispFile file = 
  runParser (many lispParser <* whiteSpace <* eof) file
  <$> Text.readFile file

-- | @parse source text@: parse the text into a list of lisp
-- expressions.  Source is used for the error messages, and in the
-- `SourceRanges`.
parseLisp :: String -> Text -> Either (ParseErrorBundle Text Void) [Lisp]
parseLisp = runParser (sepEndBy lispParser whiteSpace <* eof)

-- | parse a single expression
parseLispExpr :: String -> Text -> Either (ParseErrorBundle Text Void) Lisp
parseLispExpr = runParser lispParser

signP :: CharParser t String
signP = option "" $ ("" <$ char '+') <|> ("-" <$ char '-')

withSourceRange :: CharParser t (SourceRange -> a) -> CharParser t a
withSourceRange p = do
  startRange <- getSourcePos
  mkParser <- p
  endRange <- getSourcePos
  pure $ mkParser $ SourceRange startRange endRange

-- numbers not starting with #
numP :: CharParser t (SourceRange -> Lisp)
numP = label "number" $ do
  sign <- signP
  -- 'try' the dot, because it could be a single dot, and then we need
  -- to backtrack
  let decimalP :: CharParser t String
      decimalP = some digitChar

      -- number starting with number
      numNumP :: CharParser t (SourceRange -> Lisp)
      numNumP = do
        decimal <- decimalP
        choice [ ratioP decimal
               , try (floatP decimal)
               , do _ <- optional (char '.')
                    pure $ LispNumber $ Integer $ read (sign++decimal)
               ]

      ratioP :: String -> CharParser t (SourceRange -> Lisp)
      ratioP d = do
        _ <- char '/'
        denom <- decimalP
        pure $ LispNumber $ NumRatio $ read (sign++d) % read denom

      floatP :: String -> CharParser t (SourceRange -> Lisp)
      floatP d = 
        exptP sign d "0"
        <|> do _ <- char '.'
               exptP sign d "0" <|> do
                 fract <- decimalP
                 exptP sign d fract <|>
                   do pure $ LispNumber $ DoubleFloat $
                        read (sign++d ++ "." ++ fract)
      dotNumP = do
        _ <- char '.'
        fract <- decimalP
        exptP sign "0" fract <|>
          pure (LispNumber $ DoubleFloat $ read $ sign++"0." ++ fract)

  (numNumP <|> try dotNumP) <* notFollowedBy identifierBlocksP
  
exptP :: String -> String -> String -> CharParser t (SourceRange -> Lisp)
exptP sign num fract = do
  -- e would be context dependend, but I am defaulting it to Double here
  e <- oneOf ("esd" :: String)
  eSign <- option '+' $ char '+' <|> char '-'
  expt <- some digitChar
  let toFloat :: (Read a, Num a) => a
      toFloat = read $ sign ++ num ++ "." ++ fract ++ "e" ++ eSign:expt
  pure $ LispNumber $ case e of
    's' -> SingleFloat toFloat
    _   -> DoubleFloat toFloat

quoteAnyChar :: CharParser t Char
quoteAnyChar = char '\\' >> anySingle

stringP :: CharParser t (SourceRange -> Lisp)
stringP =
  label "string" $ do 
  str <- between (char '"') (char '"') $
         Text.pack <$> many (quoteAnyChar <|> noneOf ("\\\"" :: String))
  pure $ LispString str

identifierP :: CharParser t (SourceRange -> Lisp)
identifierP =
  label "identifier" $ do
  str <- fmap Text.pack $ (++) <$> (firstBlock <|> quotedBlockP) <*> moreBlocksP
  if str == "."
    then fail ("dot" :: String)
    else pure $ LispSymbol str

  where firstBlock :: CharParser t String
        firstBlock = (:) <$> (notSpecial <|> quoteAnyChar) <*> many blockCharP

        moreBlocksP :: CharParser t String
        moreBlocksP = concat <$> many (some blockCharP <|> quotedBlockP)
        
quotedBlockP :: CharParser t String
quotedBlockP = between (char '|') (char '|') $
               many (noneOf ("|\\" :: String) <|> quoteAnyChar)

notSpecial :: CharParser t Char
notSpecial = toUpper <$> noneOf specialChars

blockCharP :: CharParser t Char
blockCharP = notSpecial <|> char '#' <|> char '`' <|> quoteAnyChar
          
identifierBlocksP :: CharParser t String
identifierBlocksP = concat <$> some (some blockCharP <|> quotedBlockP)

lispExprP :: CharParser t (SourceRange -> Lisp)
lispExprP = choice [ stringP
                   , listP
                   , try numP
                   , try identifierP
                   , quoteP
                   , readersP
                   ]

listP :: CharParser t (SourceRange -> Lisp)
listP =
  label "list" $
  between (char '(') (char ')') $ do
  elems <- lispParser `sepEndBy` whiteSpace
  dotElem <- optional $ 
    char '.' *> whiteSpace *>
    lispParser <* whiteSpace
  pure $ case dotElem of
    Nothing -> LispList elems
    Just (LispList l _) ->  LispList $ elems ++ l
    Just (LispDotList l el _) -> LispDotList (elems ++ l) el
    Just el -> LispDotList elems el

commentP :: CharParser t ()
commentP =
  label "comment" $
  char ';' >>
  many (noneOf ("\r\n" :: String)) >>
  (void eol <|> eof)

whiteSpace :: CharParser t ()
whiteSpace = () <$ many (space1 <|> commentP)

quoteSymbol :: String -> Text -> CharParser t (SourceRange -> Lisp)
quoteSymbol symbol quoteExpansion  = do
  foldMap (void <$> char) symbol
  pure $ \(SourceRange from _) ->
    LispSymbol quoteExpansion $ SourceRange from $
    from {sourceColumn = mkPos $ length symbol + unPos (sourceColumn from)}

quoteP :: CharParser t (SourceRange -> Lisp)
quoteP = do
  quote <- (quoteSymbol "'" "quote" <|>
            try (quoteSymbol ",@" "unquote-splicing") <|>
            quoteSymbol "," "unquote" <|>
            quoteSymbol "`" "quasiquote")
           <* whiteSpace
  expr <- lispParser
  pure $ \range -> LispList [quote range, expr] range
  
readersP :: CharParser t (SourceRange -> Lisp)
readersP = do
  _ <- char '#'
  (vectorReaderP <|> complexReaderP) <|>
    (octalReaderP <|> hexReaderP <|> binaryReaderP)
    <* notFollowedBy identifierBlocksP

vectorReaderP :: CharParser t (SourceRange -> Lisp)
vectorReaderP = 
  between (char '(') (char ')') $
  LispVector <$> (lispParser `sepEndBy` whiteSpace)

octalReaderP :: CharParser t (SourceRange -> Lisp)
octalReaderP = do
  _ <- char 'o' <|> char 'O'
  sign <- signP
  digits <- some octDigitChar
  pure $ LispNumber $ Integer $ read $ sign ++ "0o" ++ digits

binaryReaderP :: CharParser t (SourceRange -> Lisp)
binaryReaderP = do
  _ <- char 'b' <|> char 'B'
  sign <- signP
  digits <- some binDigitChar
  let digitSum = foldl (\tot dig -> tot*2 + if dig == '1' then 1 else 0)
                 0 digits
      signedSum | sign == "-" = negate digitSum
                | otherwise = digitSum
  pure $ LispNumber $ Integer signedSum

hexReaderP :: CharParser t (SourceRange -> Lisp)
hexReaderP = do
  _ <- char 'x' <|> char 'X'
  sign <- signP
  digits <- some hexDigitChar
  pure $ LispNumber $ Integer $ read $ sign ++ "0x" ++ digits

convertToDouble :: Number -> Double
convertToDouble l = case l of
  Integer i -> realToFrac i
  SingleFloat f -> realToFrac f
  DoubleFloat f -> realToFrac f
  NumRatio r -> realToFrac r
  ComplexDouble _ -> error "convertToDouble"

complexReaderP :: CharParser t (SourceRange -> Lisp)
complexReaderP = do
  _ <- char 'c' <|> char 'C'
  between (char '(') (char ')') $ do
    _ <- many whiteSpace
    LispNumber rl _ <- ($ dummyRange) <$> numP
    _ <- some whiteSpace
    LispNumber imag _ <- ($ dummyRange) <$> numP
    _ <- many whiteSpace
    pure $ LispNumber $ ComplexDouble $
      convertToDouble rl :+ convertToDouble imag
