module LLang where

import           AST (AST (..), Operator (..), Subst (..))
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

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst }

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  | Return { expr :: Expr }
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

-- Парсеры для команд
  
parseIf :: Parser String String LAst
parseIf = let 
  parseElse = do
    prefix "else"
    satisfy isSpace
    els <- parseL
    return els
  in do 
    prefix "if" <* ws
    cond <- symbol '(' *> parseExpr <* symbol ')'
    thn <- parseL
    els <- parseElse <|> pure (Seq [])
    return $ If cond thn els

parseWhile :: Parser String String LAst
parseWhile = do
  prefix "while" <* ws
  cond <- symbol '(' *> parseExpr <* symbol ')'
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
  prefix "read" <* ws
  symbol '(' <* ws
  var <- parseIdent
  ws <* symbol ')'
  return $ Read var
  
parseWrite :: Parser String String LAst
parseWrite = do
  prefix "print" <* ws
  symbol '('
  expr <- parseExpr
  ws <* symbol ')'
  return $ Write expr

parseSeq :: Parser String String LAst
parseSeq = do
  symbol '{'
  cmds <- many $ parseL <* symbol ';'
  ws <* symbol '}'
  return $ Seq cmds

parseL :: Parser String String LAst
parseL = ws *> (parseIf <|> parseWhile <|> 
                parseAssign <|> parseRead <|>
                parseWrite <|> parseSeq) <* ws


parseDef :: Parser String String Function
parseDef = error "parseDef undefined"

parseProg :: Parser String String Prog
parseProg = error "parseProg undefined"

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration

eval (If cond thn els) conf = 
  case evalExpr (subst conf) cond of
    Nothing -> Nothing
    Just 0  -> eval els conf
    _       -> eval thn conf

eval (While cond body) conf = 
  case evalExpr (subst conf) cond of
    Nothing -> Nothing
    Just 0  -> Just conf
    _       ->
      case eval body conf of
        Nothing    -> Nothing
        Just conf' -> eval (While cond body) conf'

eval (Assign var expr) (Conf subst i o) =
  case evalExpr subst expr of
    Nothing -> Nothing
    Just x  -> Just $ Conf (Map.insert var x subst) i o

eval (Read var) (Conf _ [] _)             = Nothing
eval (Read var) (Conf subst (x:input') o) = Just $
  Conf (Map.insert var x subst) input' o

eval (Write expr) (Conf subst i output) = 
  case evalExpr subst expr of
    Nothing -> Nothing
    Just x  -> Just $ Conf subst i (x:output)

eval (Seq stmts) conf = foldl (\cnf stmt -> cnf >>= eval stmt) (Just conf) stmts

instance Show Function where
  show (Function name args funBody) =
    printf "%s(%s) =\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody)

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
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
          Return expr     -> makeIdent $ printf "return %s" (flatShowExpr expr)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
