{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Lisp where
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
specialChars = "()#\"\\,'| ;" 

instance Show Number where
  show (Integer i) = show i
  show (SingleFloat f) = replaceChar 'e' 's' $ show f
  show (DoubleFloat f)
    | 'd' `elem` str = str
    | otherwise = str ++ "d0"
    where str = replaceChar 'e' 'd' $ show f
  show (NumRatio r) = show (numerator r) ++ "/" ++ show (denominator r)
  show (ComplexDouble (a :+ b)) = "#(" ++ show a ++ " " ++ show b ++ ")"

data Lisp =
  LispString Text |
  LispNumber Number |
  LispSymbol Text |
  LispVector [Lisp] |
  LispList [Lisp] |
  LispDotList [Lisp] Lisp

instance Show Lisp where
  show (LispString t) = show t
  show (LispNumber n) = show n
  show (LispSymbol s)
    | Text.null s = "||"
    | Text.any (`elem` specialChars) s = '|': Text.unpack s ++ "|"
    | otherwise = Text.unpack s
    
  show (LispVector l) =
    "#(" ++ unwords (map show l) ++ ")"
  show (LispList l) = "(" ++ unwords (map show l) ++ ")"
  show (LispDotList l e) =
    "(" ++ unwords (map show l) ++ " . " ++ show e ++ ")"

instance Read Lisp where
  readsPrec _ input =
    case runParser' (whiteSpace >> lispExprP) $
         State input 0 (PosState input 0 (initialPos "") (mkPos 0) []) [] of
      (_, Left _) -> []
      (rest, Right r) -> [(r, stateInput rest)]


parseLispFile :: String -> IO (Either (ParseErrorBundle Text Void) [Lisp])
parseLispFile file = runParser (many lispExprP <* whiteSpace <* eof) file <$>
                     Text.readFile file

parseLisp :: String -> Text -> Either (ParseErrorBundle Text Void) [Lisp]
parseLisp = runParser (many lispExprP <* whiteSpace <* eof)

parseLispExpr :: String -> Text -> Either (ParseErrorBundle Text Void) Lisp
parseLispExpr = runParser lispExprP

type CharParser t a = (Stream t, Token t ~ Char)
                    => Parsec Void t a

signP :: CharParser t String
signP = option "" $ ("" <$ char '+') <|> ("-" <$ char '-')

-- numbers not starting with #
numP :: CharParser t Lisp
numP = label "number" $ do
  sign <- signP
  -- 'try' the dot, because it could be a single dot, and then we need
  -- to backtrack
  (numNumP sign <|> try (dotNumP sign)) <*
    notFollowedBy identifierBlocksP
  
    where
      decimalP :: CharParser t String
      decimalP = some digitChar

      -- number starting with number
      numNumP :: String -> CharParser t Lisp
      numNumP sign = do
        decimal <- decimalP
        choice [ ratioP sign decimal
               , try (floatP sign decimal)
               , optional (char '.') >>
                 pure (LispNumber $ Integer $ read (sign++decimal))
               ]

      ratioP :: String -> String -> CharParser t Lisp
      ratioP s d = do
        _ <- char '/'
        denom <- decimalP
        pure $ LispNumber $ NumRatio $ read (s++d) %
          read denom

      floatP :: String -> String -> CharParser t Lisp
      floatP s d = 
        exptP s d "0"
        <|> do _ <- char '.'
               exptP s d "0" <|> do
                 fract <- decimalP
                 option (LispNumber $ DoubleFloat $ read (s++d ++ "." ++ fract))
                   $ exptP s d fract

      dotNumP sign = do
        _ <- char '.'
        fract <- decimalP
        exptP sign "0" fract <|>
          pure (LispNumber $ DoubleFloat $ read $ sign++"0." ++ fract)


exptP :: String -> String -> String -> CharParser t Lisp
exptP sign num fract = do
  -- e would be context dependend, but I am defaulting it to Double here
  e <- oneOf ("esd" :: String)
  eSign <- option '+' $ char '+' <|> char '-'
  expt <- some digitChar
  let toFloat :: (Read a, Num a) => a
      toFloat = read $ sign ++ num ++ "." ++ fract ++ "e" ++ eSign:expt
  case e of
    's' -> pure $ LispNumber $ SingleFloat toFloat
    _   -> pure $ LispNumber $ DoubleFloat toFloat

quoteAnyChar :: CharParser t Char
quoteAnyChar = char '\\' >> anySingle

stringP :: CharParser t Lisp
stringP =
  label "string" $
  fmap LispString $
  between (char '"') (char '"') $
  Text.pack <$> many (quoteAnyChar <|> noneOf ("\\\"" :: String) )

identifierP :: CharParser t Lisp
identifierP =
  label "identifier" $ do
  str <- fmap Text.pack $ (++) <$> (firstBlock <|> quotedBlockP) <*> moreBlocksP
  if Text.all (== '.') str
    then fail ("all dots" :: String)
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
blockCharP = notSpecial <|> char '#' <|> quoteAnyChar
          
identifierBlocksP :: CharParser t String
identifierBlocksP = concat <$> some (some blockCharP <|> quotedBlockP)

singleton :: CharParser t a -> CharParser t [a]
singleton = fmap (:[])

lispExprP :: CharParser t Lisp
lispExprP = choice [ stringP
                   , listP
                   , try numP
                   , try identifierP
                   , quoteP
                   , readersP
                   ]

listP :: CharParser t Lisp
listP =
  label "list" $
  between (char '(') (char ')') $ do
  elems <- lispExprP `sepEndBy` whiteSpace
  dotElem <- optional $ 
    char '.' *> whiteSpace *>
    lispExprP <* whiteSpace
  pure $ case dotElem of
    Nothing -> LispList elems
    Just (LispList l) -> LispList $ elems ++ l
    Just (LispDotList l el) -> LispDotList (elems ++ l) el
    Just el -> LispDotList elems el

commentP :: CharParser t ()
commentP =
  label "comment" $
  char ';' >> noneOf ("\r\n" :: String) >> void eol

whiteSpace :: CharParser t ()
whiteSpace = () <$ many (space1 <|> commentP)

whiteSpace1 :: CharParser t ()
whiteSpace1 = () <$ some (space1 <|> commentP)

quoteSymbol :: Lisp
quoteSymbol = LispSymbol "quote"

quoteP :: CharParser t Lisp
quoteP = do
  _ <- char '\'' >> whiteSpace
  (\expr -> LispList [quoteSymbol, expr]) <$> lispExprP
  
readersP :: CharParser t Lisp
readersP = do
  _ <- char '#'
  vectorReaderP <|>
    (octalReaderP <|> complexReaderP <|> hexReaderP <|> binaryReaderP)
    <* notFollowedBy identifierBlocksP

vectorReaderP :: CharParser t Lisp
vectorReaderP = 
  between (char '(') (char ')') $
  LispVector <$> (lispExprP `sepEndBy` whiteSpace)

octalReaderP :: CharParser t Lisp  
octalReaderP = do
  _ <- char 'o' <|> char 'O'
  sign <- signP
  digits <- some octDigitChar
  pure $ LispNumber $ Integer $ read $ sign ++ "0o" ++ digits

binaryReaderP :: CharParser t Lisp
binaryReaderP = do
  _ <- char 'b' <|> char 'B'
  sign <- signP
  digits <- some binDigitChar
  let digitSum = foldl (\tot dig -> tot*2 + if dig == '1' then 1 else 0)
                 0 digits
      signedSum | sign == "-" = negate digitSum
                | otherwise = digitSum
  pure $ LispNumber $ Integer signedSum

hexReaderP :: CharParser t Lisp
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

complexReaderP :: CharParser t Lisp
complexReaderP = do
  _ <- char 'c' <|> char 'C'
  between (char '(') (char ')') $ do
    _ <- many whiteSpace
    LispNumber rl <- numP
    _ <- some whiteSpace
    LispNumber imag <- numP
    _ <- many whiteSpace
    pure $ LispNumber $ ComplexDouble $
      convertToDouble rl :+ convertToDouble imag
