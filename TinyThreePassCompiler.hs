module TinyThreePassCompiler where

import Control.Applicative ((<$>), (<$))
import Data.List (elemIndex)
import Text.ParserCombinators.Parsec (Parser(..),
                                      parse,
                                      between,
                                      many,
                                      many1,
                                      manyTill,
                                      sepBy,
                                      char,
                                      string,
                                      digit,
                                      letter,
                                      anyChar,
                                      (<|>))
import Text.ParserCombinators.Parsec.Expr (Assoc(AssocLeft),
                                           Operator(Infix),
                                           buildExpressionParser)

data AST = Imm Integer
         | Arg Int 
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

compile :: String -> [String]
compile = pass3 . pass2 . pass1

-- Takes a function and generates an AST for it
-- with Parsec
pass1 :: String -> AST
pass1 code = runParse parseExpression "pass1" (pullExpression) 
  where runParse rule parseStage str = case parse rule parseStage str of
          Left errorMessage -> error $ "In code: " ++ code ++ "\n" ++ show errorMessage
          Right result      -> result

        argList :: [String]
        argList = runParse parseArgList "argList" code

        parseArgList :: Parser [String]
        parseArgList = do
          list <- between (char '[') (char ']') $ parseWords
          return $ filter (not . null) list

        pullExpression :: String
        pullExpression = filter (/=' ') $ runParse peParser "pullExpr" code
            where peParser = do
                      manyTill anyChar $ char ']'
                      many whitespace
                      many anyChar

        argForIdentifier :: String -> AST
        argForIdentifier l = case elemIndex l argList of
            Nothing -> error $ "No argument named " ++ l
            Just i  -> Arg i

        whitespace :: Parser Char
        whitespace = char ' '

        parseWord :: Parser String
        parseWord = many letter

        parseWords :: Parser [String]
        parseWords = parseWord `sepBy` whitespace

        parseExpression :: Parser AST
        parseExpression = buildExpressionParser table term
            where binaryOp n f = Infix (f <$ string n)
                  term         = (    (between `on` char) '(' ')' parseExpression
                                  <|> (Imm . read <$> many1 digit)
                                  <|> (argForIdentifier <$> many1 letter)
                                 )
                  table        = [ [binaryOp "*" Mul AssocLeft, binaryOp "/" Div AssocLeft]
                                 , [binaryOp "+" Add AssocLeft, binaryOp "-" Sub AssocLeft]
                                 ]
                  on f g       = \x y -> f (g x) (g y)

-- Recurses into the AST and optimizes any operations
-- that don't depend on any arguments
pass2 :: AST -> AST
pass2 (Imm a) = (Imm a)
pass2 (Arg a) = (Arg a)
pass2 (Add (Imm a) (Imm b)) = Imm $ a+b
pass2 (Mul (Imm a) (Imm b)) = Imm $ a*b
pass2 (Sub (Imm a) (Imm b)) = Imm $ a-b
pass2 (Div (Imm a) (Imm b)) = Imm $ a `div` b
pass2 (Add a b) = (Add (pass2 a) (pass2 b))
pass2 (Mul a b) = (Mul (pass2 a) (pass2 b))
pass2 (Sub a b) = (Sub (pass2 a) (pass2 b))
pass2 (Div a b) = (Div (pass2 a) (pass2 b))

-- For every node, recursively generates assembly which:
-- For immediates/arguments, just loads them straight into R0
-- For math operations it:
-- 1) Evaluates first operand, and pushes that onto stack
-- 2) Evaluates second operand, and SW's that into R1
-- 3) Pops the stack into R0
-- 4) Runs the actual assembly for the operation
-- 5) Leaves its result in R0
-- 
-- By "evaluating" it means perform those steps, or use IM/AR for
-- immediates/args. Basically, every function always leaves its result in r0

pass3 :: AST -> [String]
pass3 = recursiveAssemble []
  where recursiveAssemble acc (Imm a)     = ("IM " ++ show a):acc
        recursiveAssemble acc (Arg a)     = ("AR " ++ show a):acc
        recursiveAssemble acc (Add a b)   = assembleWithOpcode acc "AD" a b
        recursiveAssemble acc (Sub a b)   = assembleWithOpcode acc "SU" a b
        recursiveAssemble acc (Mul a b)   = assembleWithOpcode acc "MU" a b
        recursiveAssemble acc (Div a b)   = assembleWithOpcode acc "DI" a b
        assembleWithOpcode acc opcode a b = recursiveAssemble [] a
                                            ++ pushStack
                                            ++ recursiveAssemble [] b 
                                            ++ swapAndPop
                                            ++ opcode:acc
        swapAndPop = ["SW", "PO"]
        pushStack  = ["PU"]
