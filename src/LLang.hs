module LLang where

import           AST
import           Combinators
import           Expr
import           Control.Applicative
import           Data.Char (isSpace)
import           Data.List   (intercalate)
import qualified Data.Map    as Map
import           Text.Printf (printf)
import qualified Data.Map as Map

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int], defs :: Defs }
                   deriving (Show, Eq)

type Defs = Map.Map String Function

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst, returnExpr :: Expr }

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  | FunCall { funName :: Var, funArgs :: [Expr] }
  deriving (Eq)


stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

-- Парсер для пробелов

ws :: Parser String String String
ws = many $ satisfy isSpace

-- Парсеры для скобок

lbr :: Parser String String Char
lbr = ws *> symbol '(' <* ws

rbr :: Parser String String Char
rbr = ws *> symbol ')' <* ws

-- Парсеры для команд
  
parseIf :: Parser String String LAst
parseIf = let 
  parseElse = do
    word "else"
    satisfy isSpace
    els <- parseL
    return els
  in do 
    word "if"
    cond <- lbr *> parseExpr <* rbr
    thn <- parseL
    els <- parseElse <|> pure (Seq [])
    return $ If cond thn els

parseWhile :: Parser String String LAst
parseWhile = do
  word "while"
  cond <- lbr *> parseExpr <* rbr
  body <- parseL
  return $ While cond body

parseAssign :: Parser String String LAst
parseAssign = do
  var <- parseIdent
  ws *> symbol '='
  expr <- parseExpr
  return $ Assign var expr
  
parseRead :: Parser String String LAst
parseRead = do
  word "read" 
  var <- lbr *> parseIdent <* rbr
  return $ Read var
  
parseWrite :: Parser String String LAst
parseWrite = do
  word "print"
  expr <- lbr *> parseExpr <* rbr
  return $ Write expr

parseSeq :: Parser String String LAst
parseSeq = do
  symbol '{'
  cmds <- many $ parseL <* symbol ';'
  ws <* symbol '}'
  return $ Seq cmds
 
parseReturn :: Parser String String Expr
parseReturn = do
  word "return"
  expr <- parseExpr
  return expr

parseFunCall :: Parser String String LAst
parseFunCall = (uncurry FunCall) <$> parseFunctionCall

parseL :: Parser String String LAst
parseL = ws *> (parseIf <|> parseWhile <|> 
                parseAssign <|> parseRead <|>
                parseWrite <|> parseSeq <|>
                parseFunCall) <* ws


parseDef :: Parser String String Function
parseDef = do
  ws <* word "fun" <* ws
  name <- parseIdent
  lbr
  args <- sepBy1 (ws *> symbol ',' <* ws) parseIdent <|> pure []
  rbr
  symbol '{'
  body <- many $ parseL <* symbol ';'
  ret <- ws *> parseReturn
  ws <* symbol '}'
  return $ Function name args (Seq body) ret
    

parseProg :: Parser String String Program
parseProg = do
  funcs <- many parseDef
  main <- parseL
  return $ Program funcs main


initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input [] Map.empty
  

instance Show Function where
  show (Function name args funBody returnExpr) =
    printf "%s(%s) =\n%s\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody) (identation 1 ("return " ++ show returnExpr))

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els   -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body   -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr   -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var          -> makeIdent $ printf "read %s" var
          Write expr        -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts         -> intercalate "\n" $ map (go n) stmts
          FunCall name args -> makeIdent $ printf "%s" (flatShowExpr (FunctionCall name args))
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)

ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id

instance Eq Function where
  a == b = show a == show b
  
instance Eq Program where
  a == b = show a == show b
