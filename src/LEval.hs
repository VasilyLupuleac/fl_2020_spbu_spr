module LEval where

import           LLang
import           AST
import           Expr
import           Combinators
import qualified Data.Map as Map

-- Соответствие между логическими и целочисленными значениями

fromBool :: Bool -> Int
fromBool False = 0
fromBool True  = 1

toBool :: Int -> Bool
toBool 0 = False
toBool _ = True

-- Вычисление выражения

evalBinOp :: Configuration -> Operator -> (Int -> Int -> Int) -> AST -> AST -> Maybe (Configuration, Int)
evalBinOp conf op opf x y = case evalExpr conf x of
  Nothing             -> Nothing
  Just (confx, resx)  -> (\(с, resy) -> (с, opf resx resy)) <$> evalExpr confx y
  
evalBoolOp :: Configuration -> Operator -> (Int -> Int -> Bool) -> AST -> AST -> Maybe (Configuration, Int)
evalBoolOp conf op opf = evalBinOp conf op (\x y -> fromBool $ opf x y)

evalExpr :: Configuration -> AST -> Maybe (Configuration, Int)
evalExpr conf (Num x)            = Just (conf, x)
evalExpr conf (Ident v)          = ((,) conf) <$> Map.lookup v (subst conf)
evalExpr conf (BinOp Plus x y)   = evalBinOp conf Plus (+) x y
evalExpr conf (BinOp Mult x y)   = evalBinOp conf Mult (*) x y
evalExpr conf (BinOp Minus x y)  = evalBinOp conf Minus (-) x y
evalExpr conf (BinOp Mod x y)    = evalBinOp conf Mod mod x y
evalExpr conf (BinOp Pow x y)    = evalBinOp conf Pow (^) x y
evalExpr conf (BinOp Equal x y)  = evalBoolOp conf Equal (==) x y
evalExpr conf (BinOp Nequal x y) = evalBoolOp conf Nequal (/=) x y
evalExpr conf (BinOp Ge x y)     = evalBoolOp conf Ge (>=) x y
evalExpr conf (BinOp Gt x y)     = evalBoolOp conf Gt (>) x y
evalExpr conf (BinOp Le x y)     = evalBoolOp conf Le (<=) x y
evalExpr conf (BinOp Lt x y)     = evalBoolOp conf Lt (<) x y

evalExpr conf (BinOp Div x y)    = case evalExpr conf x of
  Nothing -> Nothing
  Just (c, resx) -> case evalExpr c y of
    Just (c', resy) | resy /= 0 -> Just (c', resx `div` resy)
    _                           -> Nothing

evalExpr conf (BinOp And x y)    = evalBinOp conf And (\x y -> fromBool $ toBool x && toBool y) x y
evalExpr conf (BinOp Or x y)     = evalBinOp conf Or (\x y -> fromBool $ toBool x || toBool y) x y

evalExpr conf (UnaryOp Minus x)  = (\(c, y) -> (c, (-y))) <$> evalExpr conf x
evalExpr conf (UnaryOp Not x)    = (\(c, y) -> (c, fromBool $ y == 0)) <$> evalExpr conf x

evalExpr conf (FunctionCall name args) = do
  f <- Map.lookup name (defs conf)
  c <- mkFunConf f args conf
  c' <- eval (funBody f) c
  (Conf s i o d, res) <- evalExpr c' (returnExpr f)
  return $ (Conf (subst conf) i o d, res)

evaluate :: [(String, Int)] -> String -> Maybe Int
evaluate subst input =
  case runParser parseExpr input of
    Success rest ast | null (stream rest) -> snd <$> evalExpr (Conf (Map.fromList subst) [] [] (Map.fromList [])) ast
    _                                     -> Nothing


mkFunConf :: Function -> [Expr] -> Configuration -> Maybe Configuration
mkFunConf f argList conf = if length argList /= length (args f) then Nothing
  else foldr evalArg (Just (conf, [])) (zip (args f) argList) >>= mkConf where
    evalArg :: (Var, Expr) -> Maybe (Configuration, [(Var, Int)])-> Maybe (Configuration, [(Var, Int)])
    evalArg (x, expr) Nothing          = Nothing
    evalArg (x, expr) (Just (c, list)) = (evalExpr c expr) >>= (\(conf, res) -> Just (conf, (x, res):list))
    mkConf :: (Configuration, [(Var, Int)]) -> Maybe Configuration
    mkConf ((Conf _ i o d), args) = Just $ Conf (Map.fromList args) i o d
    

eval :: LAst -> Configuration -> Maybe Configuration

eval (If cond thn els) conf = 
  case evalExpr conf cond of
    Nothing          -> Nothing
    Just (conf', 0)  -> eval els conf'
    Just (conf', _)  -> eval thn conf'

eval (While cond body) conf = 
  case evalExpr conf cond of
    Nothing         -> Nothing
    Just (conf', 0) -> Just conf'
    Just (conf', _) ->
      eval body conf' >>= eval (While cond body)

eval (Assign var expr) conf =
  case evalExpr conf expr of
    Nothing                    -> Nothing
    Just (Conf subst i o d, x) -> Just $ Conf (Map.insert var x subst) i o d

eval (Read var) (Conf _ [] _ _)             = Nothing
eval (Read var) (Conf subst (x:input') o d) = Just $
  Conf (Map.insert var x subst) input' o d

eval (Write expr) conf = 
  case evalExpr conf expr of
    Nothing -> Nothing
    Just (Conf subst i output d, x) ->
      Just $ Conf subst i (x:output) d

eval (Seq stmts) conf = foldl (\cnf stmt -> cnf >>= eval stmt) (Just conf) stmts

eval (FunCall name args) conf = do
  f <- Map.lookup name (defs conf)
  c <- mkFunConf f args conf
  (Conf _ i o d) <- eval (funBody f) c
  return $ Conf (subst conf) i o d 

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program funcs main) input = let
  defs = (Map.fromList $ (\f -> (name f, f)) <$> funcs) in
    eval main $ Conf Map.empty input [] defs

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg code input = case (runParser parseProg) code of
  Success (InputStream [] _) prog -> evalProg prog input
  _                               -> Nothing