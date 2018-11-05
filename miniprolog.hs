import Debug.Trace
import System.IO
import System.Environment
import Data.List
import Data.Ord
import Data.Maybe
import Text.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char
import Control.Monad.State

type Parser = Parsec String ()

data Term = Atom String
          | Variable String Int
          | Compound String [Term]
          deriving (Eq, Ord) -- need to derive Ord for compareBy

data Rule = Rule Term [Term]

type Program = [Rule]

type Substitutions = Maybe [(Term, Term)]

instance Show Term where
    show (Atom x) = x
    show (Variable x 0) = x
    show (Variable x i) = x ++ show i
    show term@(Compound "." _) = "[" ++ f term ++ "]"
        where f (Compound "." [x, Variable y _]) = show x ++ "|" ++ y
              f (Compound "." [x, Atom "[]"])    = show x
              f (Compound "." [x, y])            = show x ++ "," ++ f y
    show (Compound x ys) = x ++ "(" ++ (intercalate "," (show <$> ys)) ++ ")"

instance Show Rule where
    show (Rule x []) = show x ++ "."
    show (Rule x ys) = show x ++ ":-" ++ intercalate "," (show <$> ys) ++ "."

idBody :: Parser String
idBody = many $ letter <|> digit <|> char '_'

atom :: Parser Term
atom = ((\x y -> Atom (x:y)) <$> lower <*> idBody) <|>
       (Atom <$> many1 (oneOf "!@#$%^&*<>"))

variable :: Parser Term
variable = (\x y -> Variable (x:y) 0) <$> (upper <|> char '_') <*> idBody

parens = between (char '(') (char ')')
brackets = between (char '[') (char ']')

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (char ',')
commaSep1 :: Parser a -> Parser [a]
commaSep1 p = p `sepBy1` (char ',')

compound :: Parser Term
compound = do
    Atom functor <- atom
    args <- parens $ commaSep1 term
    return $ Compound functor args

list :: Parser Term
list = brackets $
    do terms <- commaSep term
       tail <- option (Atom "[]") (char '|' *> (list <|> variable))
       return $ f terms tail
           where f [] ys = ys
                 f (x:xs) ys = Compound "." [x, f xs ys]

term :: Parser Term
term = (try compound) <|> atom <|> variable <|> list

rule :: Parser Rule
rule = do
    head <- term
    tail <- option [] (string ":-" *> commaSep1 term)
    char '.'
    return $ Rule head tail

program :: Parser Program
program = many (rule <* many (char '\n')) <* eof

unify :: Term -> Term -> Substitutions
unify (Atom a) (Atom b)
  | a == b = Just []
unify (Variable v i) x = Just [(Variable v i, x)]
unify x (Variable v i) = Just [(Variable v i, x)]
unify (Compound f xs) (Compound g ys)
  | f == g && length xs == length ys = unifyList xs ys
unify _ _ = Nothing

unifyList :: [Term] -> [Term] -> Substitutions
unifyList [] [] = Just []
unifyList (x:xs) (y:ys) = (++) <$> s <*> unifyList (apply s <$> xs) (apply s <$> ys)
    where s = unify x y

unifyRule :: Term -> Rule -> Substitutions
unifyRule x rule@(Rule y _) = unify x y

mergeSub :: [Substitutions] -> Substitutions
mergeSub xs = concat <$> sequence xs

rename :: Int -> Rule -> Rule
rename i (Rule head body) = Rule (f head) (f <$> body)
    where f (Variable x _)  = Variable x i
          f (Compound x ys) = Compound x (f <$> ys)
          f x               = x

apply :: Substitutions -> Term -> Term
apply (Just ((x, y):xs)) v@(Variable _ _)
  | x == v = y
  | otherwise = apply (Just xs) v
apply s (Compound x xs) = Compound x (apply s <$> xs)
apply _ t = t

-- Repeatedly apply substitutions to get actual assignment of a variable
getAssignment :: Substitutions -> Term -> Term
getAssignment sub term = if newTerm == term then term
                                       else getAssignment sub newTerm
                                           where newTerm = apply sub term

-- Given rules and goals, return required substitutions and resulting new goals
explore :: [Rule] -> [Term] -> [(Substitutions, [Term])]
explore rules (goal:goals) = do
    rule@(Rule _ body) <- rules
    let s = unifyRule goal rule
    return (s, apply s <$> (body ++ goals))

solve :: [Term] -> [Rule] -> Int -> [Substitutions]
solve [] rules i = [Just []]
solve goals rules i = do
    (Just s, nextGoals) <- (explore (rename i <$> rules) goals)
    Just rest <- solve nextGoals rules (i+1)
    return $ Just (s ++ rest)

-- Simplify solution set
-- Merge equivalent variables and remove free variables
simplify :: [(Term, Term)] -> [[Term]]
simplify xs = filter (\x -> length x > 1) merged
    where groups = groupBy (\a b -> snd a == snd b) $ sortBy (comparing snd) xs
          merged = do 
              l <- groups
              let val = snd $ head l
              case val of
                Variable _ _ -> return $ fst <$> l
                _            -> return $ (fst <$> l) ++ [val]

main = do
    path:_ <- getArgs
    source <- readFile path
    let database = parse program "" source
    case database of
      Left err -> putStrLn $ show err
      Right db -> do
          putStrLn "Imported rules:"
          mapM_ (putStrLn . show) db
          repl db

repl :: Program -> IO ()
repl database = do
    putStr' "?- "
    line <- getLine
    let Right (Rule query []) = parse rule "" line
    let subs = solve [query] database 1
    hSetBuffering stdin NoBuffering
    eval subs
    hSetBuffering stdin LineBuffering
    repl database

eval :: [Substitutions] -> IO ()
eval [] = putStrLn "false."
eval (x@(Just s):xs) = do
    let solution = simplify [(v, getAssignment x v) | (v@(Variable _ 0), _) <- s]
    if solution == [] then putStr' "true"
                      else putStr' $ intercalate ", " $ (\l -> intercalate "=" (show <$> l)) <$> solution
    if xs == [] then putStrLn "."
                else do action <- getChar
                        if action == ';' then putStrLn "" >> eval xs
                                         else return ()

putStr' x = putStr x >> hFlush stdout
