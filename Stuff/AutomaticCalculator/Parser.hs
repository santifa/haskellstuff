module Stuff.AutomaticCalculator.Parser (Parser,
               apply,
               applyParser,
               symbol,
               alphanum,
               char,
               letter,
               space,
               somewith,
               some,
               sat,
               many,
               orelse) where 
import Data.Char


newtype Parser a = MkP(String -> [(a,String)])


apply :: Parser a -> String -> [(a, String)]
apply (MkP f) s = f s


applyParser :: Parser a -> String -> a
applyParser p = fst.head.apply p

instance Monad Parser where
  return x = MkP f where f s = [(x,s)]
  p >>= q = MkP f
    where f s = [(y,s'') | (x,s') <- apply p s, (y, s'') <- apply(q x) s']


item :: Parser Char
item = MkP f 
        where f []      = []
              f (c:cs)  = [(c,cs)]

zero :: Parser a
zero = MkP f where f s = []

-- zero >>= p = zero
-- p >>= (const zero zero) = zero


sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else zero}

char :: Char -> Parser()
char x = do {c <- sat (== x); return ()}

string :: String -> Parser()
string [] = return ()
string (x:xs) = do { char x ; string xs ; return() }

lower :: Parser Char
lower = sat isLower

alphanum :: Parser Char
alphanum = sat isAlphaNum

letter :: Parser Char
letter = sat isAlpha

digit :: Parser Int
digit = do { d <- sat isDigit; return(ord d - ord '0') }


-- alternation

plus :: Parser a -> Parser a -> Parser a
p `plus` q = MkP f 
            where f s = apply p s ++ apply q s

lowers :: Parser String
lowers = do { c <- lower; cs <- lowers; return (c:cs) } `plus` return ""


orelse :: Parser a -> Parser a -> Parser a
p `orelse` q = MkP f 
           where f s  = if null (apply p s) then apply q s else (apply p s)



right :: Parser Int
right = digit `plus` addition

wrong :: Parser Int
wrong = digit `orelse` addition

addition :: Parser Int
addition = do {m <- digit; char '+'; n <- digit; return( m + n )}


better :: Parser Int
better = addition `orelse` digit


ident :: Parser String
ident = do {c <- lower; cs <- many alphanum; return (c:cs)}


nat :: Parser Int
nat = do {ds <- some digit; return (foldl1 (l) ds)}
      where m `l` n = 10 * m + n

int :: Parser Int
int = do{f <- op; n <- nat; return(f n)}
      where op = do {char '-'; return negate} `orelse` return id

ints :: Parser [Int]
ints = do char '['
          n <- int
          ns <- many(do{char ','; int})
          char ']'
          return (n : ns)



somewith :: Parser b -> Parser a -> Parser [a]
somewith q p = do x <- p
                  xs <- many (do{q; p})
                  return (x:xs)


manywith :: Parser b -> Parser a -> Parser [a]
manywith q p = somewith q p `orelse` return []

space :: Parser()
space = do many (sat isSpace)
           return()


token :: Parser a -> Parser a
token p = do {space; x <- p ; space; return x}

symbol :: String -> Parser()
symbol xs = token(string xs)




data Expr = Con Int | Bin Op Expr Expr deriving (Show)
data Op = Plus | Minus deriving (Show)


expr :: Parser Expr
expr = do {t <- factor; rest t}
rest t = do {op <- addop; u <- factor; rest(Bin op t u)}
         `orelse` return t


const' = do {n <- int; return(Con n)}
term = do {t <- expr; op <- addop; u <- expr; return(Bin op t u)}
addop = do {symbol "-"; return Minus}
factor = token const' `orelse`
         do {symbol "("; e <- expr; symbol ")"; return e}



force :: Parser a -> Parser a
force p = MkP f
          where f cs = (fst (head rs), snd(head rs)) : tail rs
                       where rs = apply p cs

many :: Parser a -> Parser [a]
many p = force(some p `orelse` return [])

some :: Parser a -> Parser [a]
some p = do {x <- p; xs <- many p; return (x:xs)}


limit :: Parser a -> Parser a
limit p = MkP (first.apply p)

first :: [a] -> [a]
first [] = []
first (r:rs) = [r]


