-- To test examples in commnets, type
-- % doctest 10misc.hs

import Data.Char
import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------

-- | Parser for mathematical expressions
--
-- >>> parse my_paren "" "()()()"
-- Right 3
-- >>> parse my_paren "" "(((())))"
-- Right 4
-- >>> parse my_paren "" "(())(()())"
-- Right 5

my_paren :: Parser Int
my_paren = openClose <|> return 0
  where
    openClose = do
        char '('
        cnt1 <- my_paren
        char ')'
        cnt2 <- my_paren
        return (1 + cnt1 + cnt2)

----------------------------------------------------------------

-- | Parser for mathematical expressions
--
-- >>> parse my_expr "" "1"
-- Right 1
-- >>> parse my_expr "" "(1+2)*3"
-- Right 9
-- >>> parse my_expr "" "(1+2)*(3+4)+5"
-- Right 26

my_expr :: Parser Int
my_expr = do
    n <- my_term
    m <- plus <|> return 0
    return (n + m)
  where
    plus = do
        char '+'
        my_expr

my_term :: Parser Int
my_term = do
    n <- my_factor
    m <- multi <|> return 1
    return (n * m)
  where
    multi = do
        char '*'
        my_term

my_factor :: Parser Int
my_factor = paren <|> my_nat
  where
    paren = do
        char '('
        n <- my_expr
        char ')'
        return n

my_nat :: Parser Int
my_nat = do
    n <- oneOf ['0'..'9']
    return $ ord n - ord '0'

----------------------------------------------------------------

main :: IO ()
main = undefined
