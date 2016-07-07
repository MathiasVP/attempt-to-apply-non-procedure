import Language.Scheme.Parser
import Language.Scheme.Types
import Control.Monad.State
import System.Environment
import qualified Data.Array as Array

unique :: State Int String
unique = do
  n <- get
  modify (+1)
  return $ show n

instrument :: LispVal -> State Int LispVal
instrument (List []) = return $ List []
instrument (List [Atom "time", e]) = do
  e' <- instrument e
  return $ List [Atom "time", e']
instrument (List [Atom "if", e, then_, else_]) = do
  e' <- instrument e
  then_' <- instrument then_
  else_' <- instrument else_
  return $ List [Atom "if", e', then_', else_']
instrument (List [Atom "else", e]) = do
  e' <- instrument e
  return $ List [Atom "else", e']
instrument (List [Atom "case", clauses]) = do
  clauses' <- instrument clauses
  return $ List [Atom "case", clauses]
instrument (List [Atom "cond", clauses]) = do
  clauses' <- instrument clauses
  return $ List [Atom "cond", clauses']
instrument (List (Atom "and" : exprs)) = do
  exprs' <- mapM instrument exprs
  return $ List (Atom "and" : exprs')
instrument (List (Atom "or" : exprs)) = do
  exprs' <- mapM instrument exprs
  return $ List (Atom "or" : exprs')
instrument (List [Atom "define", name, body]) = do
  args' <- instrument body
  return $ List [Atom "define", name, args']
instrument (List [Atom "lambda", args, body]) = do
  body' <- instrument body
  return $ List [Atom "lambda", args, body']
instrument (List [Atom "let", bindings, e]) = do
  bindings' <- instrument bindings
  e' <- instrument e
  return $ List [Atom "let", bindings, e']
instrument (List [Atom "let*", bindings, e]) = do
  bindings' <- instrument bindings
  e' <- instrument e
  return $ List [Atom "let*", bindings', e']
instrument (List [Atom "letrec", bindings, e]) = do
  bindings' <- instrument bindings
  e' <- instrument e
  return $ List [Atom "letrec", bindings', e']
instrument (List (func:args)) = do
  n <- unique
  func' <- instrument func
  args' <- mapM instrument args
  return $
    List
      [Atom "if", List [Atom "procedure?", func'],
                  List (func:args'),
                  List [Atom "error", Atom "'instrumented'", String (n ++ ": procedure? returned #f")]]
instrument (Atom s) = return $ Atom s
instrument (DottedList vals val) =
  liftM2 DottedList (mapM instrument vals) (instrument val)
instrument (Vector arr) =
  fmap (Vector . Array.listArray (Array.bounds arr))
       (mapM instrument (Array.elems arr))
instrument (ByteVector bs) = return $ ByteVector bs
instrument (HashTable m) = HashTable <$> mapM instrument m
instrument (Number n) = return $ Number n
instrument (Float d) = return $ Float d
instrument (Complex c) = return $ Complex c
instrument (Rational r) = return $ Rational r
instrument (String s) = return $ String s
instrument (Char ch) = return $ Char ch
instrument (Bool b) = return $ Bool b
instrument (PrimitiveFunc f) = return $ PrimitiveFunc f
instrument (Func params vararg body closure) = do
  body' <- mapM instrument body
  return $ Func params vararg body' closure
instrument (hf@HFunc{}) = return hf
instrument (IOFunc f) = return $ IOFunc f
instrument (EvalFunc f) = return $ EvalFunc f
instrument (CustFunc f) = return $ CustFunc f
instrument (Pointer v e) = return $ Pointer v e
instrument (Opaque dyn) = return $ Opaque dyn
instrument (Port h mk) = return $ Port h mk
instrument (c@Continuation{}) = return c
instrument (Syntax synClos synRenameClos synDef synEllip synIds synRs) =
  liftM2 (Syntax synClos synRenameClos synDef synEllip)
         (mapM instrument synIds) (mapM instrument synRs)
instrument (SyntaxExplicitRenaming val) =
  fmap SyntaxExplicitRenaming (instrument val)
instrument (LispEnv env) = return $ LispEnv env
instrument EOF = return EOF
instrument (Nil s) = return $ Nil s

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input, output] -> do
      content <- readFile input
      putStrLn "Parsing..."
      case readExprList content of
        Left err -> print err
        Right ast -> do
          putStrLn "Processing..."
          let ast' = evalState (mapM instrument ast) 0
          mapM_ (appendFile output. (++ "\n") . show) ast'
          putStrLn "Done!"
    _ -> putStrLn "Usage: path/to/input.scm path/to/output.scm"
