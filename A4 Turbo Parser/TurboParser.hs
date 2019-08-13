-- module TurboParser where

-- import Control.Applicative

-- import ParserLib
-- import TurboDef

-- -- This can help testing by reading from a file so you can test multi-line input
-- -- and also have little hassle with \
-- parseFile :: String -> IO (Maybe Stmt)
-- parseFile filename = do
    -- inp <- readFile filename
    -- let ans = runParser mainParser inp
    -- return ans

-- reserved = ["forward", "turn", "for", "penup", "pendown", "to"]

-- mainParser :: Parser Stmt
-- mainParser = whitespaces *> turboParser <* eof

-- turboParser :: Parser Stmt
-- turboParser = loopParser <|> forwardParser <|> penParser <|> assignParser <|> turnParser 

-- -- parses forward stmts
-- forwardParser :: Parser Stmt
-- forwardParser = do 
  -- keyword "forward" 
  -- whitespaces
  -- val <- exprParser
  -- return (Forward val)

-- exprParser :: Parser RealExpr
-- exprParser = adds
    -- where
        -- adds = chainl1 mults (char '+' *> whitespaces *> pure (:+) <|> (char '-' *> whitespaces *> pure (:-)))
        -- mults = chainl1 atom ((char '*' *> whitespaces *> pure (:*)) <|> (char '/' *> whitespaces *> pure (:/)))
  -- --      negs = chainl1Unary atom (char '-' *> whitespaces *> pure Neg)
        -- atom = fmap RLit double <|> fmap RLit integerForm
                                -- <|> (char '-' *> whitespaces *> fmap Neg atom)
                                -- <|> (fmap RVar (identifier reserved))
                                -- <|> between (char '(' *> whitespaces) (char ')' *> whitespaces) adds


-- integerForm :: Parser Double
-- integerForm = do
    -- a <- natural
    -- pure (fromIntegral a)

-- double :: Parser Double
-- double = do
    -- a <- natural
    -- char '.'
    -- b <- natural <* whitespaces
    -- pure ((fromIntegral a) + (doubleDecimal (fromIntegral b)))
            
            
-- doubleDecimal :: Double -> Double
-- doubleDecimal inp
    -- | inp > 1 = doubleDecimal (inp / 10)
    -- | inp >= 0 = inp     

-- --parses turn stmts
-- turnParser :: Parser Stmt
-- turnParser = keyword "turn" *> whitespaces *> exprParser >>= (\val -> return (Turn val))

-- -- parses pen stmts
-- penParser :: Parser Stmt
-- penParser = up <|> down

-- -- up pen state
-- up :: Parser Stmt
-- up = keyword "penup" *> whitespaces *> return PenUp

-- -- down pen state
-- down :: Parser Stmt
-- down = keyword "pendown" *> whitespaces *> return PenDown


-- -- assignParser :: Parser Stmt
-- -- assignParser = do 
    -- -- var <- identifier reserved
    -- -- whitespaces
    -- -- keyword "="
    -- -- whitespaces
    -- -- val <- exprParser
    -- -- return ( var := val)
-- -- parses assignments
-- assignParser :: Parser Stmt
-- assignParser = getVar >>= \var -> getEq >>= \eq -> getArg >>= \arg -> pure (eq var arg)
    -- where
        -- getVar = (identifier ["pendown", "penup", "turn", "forward", "for", "to"])
        -- getEq = (char '=' *> whitespaces *> pure (:=))
        -- getArg = exprParser

-- --parses all loop based stmts
-- loopParser :: Parser Stmt
-- loopParser = forParser <|> seqParser

-- -- parses for loops
-- -- forParser :: Parser Stmt
-- -- forParser = keyword "for" *> whitespaces *> (identifier reserved) >>= (
    -- -- \var -> keyword "=" *> whitespaces *> exprParser >>= (
        -- -- \val1 -> keyword "to" *> whitespaces *> exprParser >>= (\val2 -> listParser >>= (\list -> return (For var val1 val2 list)))))

-- forParser = do
  -- keyword "for"
  -- whitespaces
  -- var <- identifier reserved
  -- operator "="
  -- whitespaces
  -- val1 <- exprParser
  -- keyword "to"
  -- val2 <- exprParser
  -- list <- listParser
  -- return (For var val1 val2 list)
-- -- forParser = do
    -- -- keyword "for"
    -- -- tempVar <- identifier reserved
    -- -- char '='
    -- -- initVal <- exprParser
    -- -- keyword "to"
    -- -- endVal <- exprParser
    -- -- list <- listParser
    -- -- return (For tempVar initVal endVal list)

-- -- parses seq blocks
-- seqParser :: Parser Stmt
-- seqParser = listParser >>= (\list -> return (Seq list))


-- -- listParser :: Parser [Stmt]
-- -- listParser = between (char '{' *> whitespaces) (char '}' *> whitespaces) ((some turboParser) >>= (\cmd -> char ';' -> whitespaces -> return cmd))

-- listParser :: Parser [Stmt]
-- listParser = do
    -- s <- between (char '{' *> whitespaces)
                 -- (char '}' *> whitespaces)
                 -- (many seq_statement)
    -- return (s)
    -- where
       -- seq_statement = do
       -- s <- turboParser
       -- char ';' *> whitespaces
       -- pure s  
module TurboParser where

import Control.Applicative

import ParserLib
import TurboDef

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \

reservedWords = ["pendown", "penup", "turn", "forward", "for", "to"]

parseFile :: String -> IO (Maybe Stmt)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans

mainParser :: Parser Stmt
mainParser = whitespaces *> turboParser <* eof


-- Parses a stmt
turboParser :: Parser Stmt
turboParser = forwardParser <|> penParser <|> assignParser <|> turnParser <|> seqParser <|> loopParser 


-- parses forward commands
forwardParser :: Parser Stmt
forwardParser = keyword "forward" <* whitespaces >>= \_ -> exprParser >>= \val -> return (Forward val) 


-- Parses turn commands
turnParser :: Parser Stmt
turnParser = keyword "turn" *> whitespaces *> exprParser >>= (\val -> return (Turn val))


-- Parses penup/pendown commands
penParser :: Parser Stmt
penParser = up <|> down
    where
        up = keyword "penup" *> whitespaces *> return PenUp
        down = keyword "pendown" *> whitespaces *> return PenDown


-- Parses variable assignment equations
assignParser :: Parser Stmt
assignParser = getVar >>= \var -> getEq >>= \eq -> getArg >>= \arg -> pure (eq var arg)
    where
        getVar = (identifier reservedWords)
        getEq = (char '=' *> whitespaces *> pure (:=))
        getArg = exprParser


-- Parses Sequences
seqParser :: Parser Stmt
seqParser = listParser >>= (\item -> return (Seq item))


-- Parses lists of the form:  { ... }
-- Helper of Seq and For parsers.
listParser :: Parser [Stmt]
-- parse {, } and stmts between parenthesis.
listParser = between (char '{' *> whitespaces) (char '}' *> whitespaces) (many listStmt) >>= \lst -> return lst
    where
        -- parser for the stmts in between, separated by ';'.
        listStmt = turboParser >>= \stmt -> char ';' <* whitespaces >>= \_ -> pure stmt


-- Parses the for-loops
loopParser :: Parser Stmt
loopParser = keyword "for" >>= \_ -> identifier reservedWords >>= \tempVar -> (operator "=") >>= \_ -> exprParser >>= \initVal -> keyword "to" >>= \_ -> exprParser >>= \endVal -> listParser >>= \list ->return (For tempVar initVal endVal list)


-- Parses for expressions
exprParser :: Parser RealExpr
exprParser = adds
    where
        -- left-associatively parses +,- after *,/
        adds = chainl1 mults (char '+' *> whitespaces *> pure (:+) <|> (char '-' *> whitespaces *> pure (:-)))
        -- left-associateively parses *,/ after atomic expressions
        mults = chainl1 atom ((char '*' *> whitespaces *> pure (:*)) <|> (char '/' *> whitespaces *> pure (:/)))
        -- parses literals (integer or doubles to doubles), negatives, variables, and brackets.
        atom = fmap RLit double <|> fmap RLit integerForm
                                <|> (char '-' *> whitespaces *> fmap Neg atom)
                                <|> (fmap RVar (identifier ["pendown", "penup", "turn", "forward", "for", "to"]))
                                <|> between (char '(' *> whitespaces) (char ')' *> whitespaces) adds

-- Parses numbers in integer form (no decimal points).
integerForm :: Parser Double
integerForm = do
    a <- natural
    pure (fromIntegral a)

-- Parses numbers in double form (decimal points).
double :: Parser Double
double = do
    a <- natural
    char '.'
    b <- natural <* whitespaces
    pure ((fromIntegral a) + (doubleDecimal (fromIntegral b)))

-- Helper for double; turns digits after "." to decimals.
doubleDecimal :: Double -> Double
doubleDecimal inp
    | inp > 1 = doubleDecimal (inp / 10)
    | inp >= 0 = inp