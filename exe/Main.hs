import System.Environment
import Control.Monad.State
import Data.Foldable
import Debug.Trace

fst3 (x,_,_)=x
trd3 (_,_,x)=x

main :: IO()
main = 
  do
    args <- getArgs
    case (args) of
        [arg1]      -> compileIO arg1 "out.s"
        [arg1,arg2] -> compileIO arg1 arg2
        _           -> putStrLn ("Expected two arguments, but "++ show (length args) ++ " were given.")

compileIO :: String -> String -> IO()
compileIO inF outF = 
  do
    cCode <- readFile inF
    let out = do
               cTokens <- parse cTokenize cCode
               parse cParser (traceShowId cTokens)
    case out of
       Valid a -> putStrLn a --writeFile outF a
       Error s -> putStrLn s
        


data Parser c a = Parser ([c] -> Error (a,[c]))
data Error a =  Valid a | Error String
  deriving Show



instance Monad Error where
  Valid a   >>= f = f a
  Error s   >>= f = Error s

  --Valid f <*> Valid a = Valid (f a)
  --Error s <*> _       = Error s
  --_       <*> Error s = Error s

  --return = Valid

instance Functor Error where
  fmap f (Error s) = Error s
  fmap f (Valid a) = Valid (f a)

instance Applicative Error where
  pure = Valid
  

instance Functor (Parser c) where
  --fmap :: (a->b) -> Parser c a -> Parser c b
  fmap f (Parser g) = Parser h
    where 
      h s = g s >>= (\(parsed,rest) -> pure (f parsed, rest))
--        do
--          (parsed,rest) <- g s
--          pure (f parsed,rest)

showE (Error _) = "E"
showE (Valid _) = "V"

pToFunc :: Parser c s -> [c] -> Error (s,[c])
pToFunc (Parser f) = f

instance Applicative (Parser c) where
  pure x = Parser (\s ->  Valid (x,s))
  (<*>) f g = Parser h
    where
      h s=
        do
          (func,rest) <- pToFunc f s
          (out,rest2) <- pToFunc g rest
          pure (func out,rest2)

(<|>) :: Parser c a -> Parser c a -> Parser c a
f <|> g = Parser h
  where 
    h s = case (pToFunc f s,pToFunc g s) of
            (x@(Valid _),_) -> x
            (Error _,y)    -> y

parse :: Parser c a -> [c] -> Error a
parse f s = fmap fst (pToFunc f s)


word :: String -> a -> (Char->Bool) -> Parser Char a
word s a wordSep = Parser f
  where
    f t = case valid t of
            True  -> Valid (a, drop (length s) t)
            False -> Error ("Expected "++s)
    valid t = s== take (length s) t &&  (droppedValid $ drop (length s) t)
    droppedValid (x:_) = wordSep x
    droppedValid []    = True

symbol :: (Show s,Eq s) =>s -> a -> Parser s a
symbol c a = Parser f
  where
    f [] = Error "No symbol"
    f (s:ss) = if s==c then Valid (a,ss) else Error ("Expected "++show c)

many :: Parser c a -> Parser c [a]

many g = ((:) <$> g <*> many g) <|> pure []

many1 :: Parser c a -> Parser c [a]
many1 g = (:) <$> g <*>  many g 

data Statement = SExpression Expression | SDecl String (Maybe Expression) |
                 SCodeBlock [Statement] | SReturn Expression
  deriving Show

data Expression = ENumber Int | EVariable String |ENegate Expression |
                  EComplement Expression | ENot Expression | EAdd Expression
                  Expression | ESubtract Expression Expression | EMultiply
                  Expression Expression | EDivide Expression Expression |
                  ELogAnd Expression Expression | ELogOr Expression Expression |
                  EEqual Expression Expression | ENotEqual Expression
                  Expression | ELess Expression Expression | ELeq Expression
                  Expression | EGreater Expression Expression | EGeq Expression Expression |
                  EAssign Expression Expression
  deriving Show

exprToInstr :: Expression -> State ProgInfo  [String]

exprToInstr (ENumber     n) = return $ ["movl\t$"++(show n)++", %eax"]
exprToInstr (EVariable   s)= do (table,_,_) <- get
                                case lookup s table of
                                  Just offset -> return $ ["movl\t"++show offset++"(%r10), %eax"]
                                  Nothing -> error $ "Variable "++s++" does not exist."

exprToInstr (ENegate     expr) = flip (++) ["neg\t%eax"] <$> exprToInstr expr
exprToInstr (EComplement expr) = flip (++) ["not\t%eax"] <$> exprToInstr expr
exprToInstr (ENot expr) = flip (++) ["cmpl\t$0, %eax","movl\t$0,%eax","sete\t%al"] <$> exprToInstr expr

exprToInstr (EAdd expr1 expr2) = binaryOp ["addl\t%ecx,%eax"] expr1 expr2
exprToInstr (ESubtract expr1 expr2)= binaryOp ["subl\t%ecx,%eax"] expr1 expr2
exprToInstr (EMultiply expr1 expr2)= binaryOp ["imul\t%ecx,%eax"] expr1 expr2
exprToInstr (EDivide expr1 expr2)= binaryOp ["cdq","idivl %ecx"] expr1 expr2

--(exprToProg expr2 v)++["push\t%rax"]++
--                                    (exprToProg expr1 v)++["pop\t%rcx","cdq","idivl %ecx"]

exprToInstr (ELogAnd expr1 expr2)= do modify (\(x,y,z) -> (x,y,z+1))
                                      labelCount <- trd3 <$> get
                                      (\e1-> \e2->e1++["cmpl\t$0, %eax","je\tendAnd"++show labelCount]++e2++["cmpl\t$0, %eax","endAnd"++show labelCount++":","setne\t%al"]) <$> exprToInstr expr1 <*> exprToInstr expr2
exprToInstr (ELogOr expr1 expr2)= do modify (\(x,y,z) -> (x,y,z+1))
                                     labelCount <- trd3 <$> get
                                     (\e1-> \e2->e1++["cmpl\t$0, %eax","jne\tendAnd"++show labelCount]++e2++["cmpl\t$0, %eax","endAnd"++show labelCount++":","setne\t%al"]) <$> exprToInstr expr1 <*> exprToInstr expr2
exprToInstr (EEqual expr1 expr2) = comparison "e" expr1 expr2
exprToInstr (ENotEqual expr1 expr2)= comparison "ne" expr1 expr2
exprToInstr (ELess expr1 expr2) = comparison "l" expr1 expr2
exprToInstr (ELeq expr1 expr2) = comparison "le" expr1 expr2
exprToInstr (EGreater expr1 expr2) = comparison "g" expr1 expr2
exprToInstr (EGeq expr1 expr2)= comparison "ge" expr1 expr2

exprToInstr (EAssign (EVariable s) expr) = do (table,_,_) <- get 
                                              case lookup s table of
                                                Just offset -> flip (++) ["movl\t%eax, "++show offset++"(%r10)"] <$> (exprToInstr expr)
                                                otherwise -> error $ "Variable "++s++" does not exist"
exprToInstr (EAssign _ _) = error "Can only assign a variable"

binaryOp instr expr1 expr2 = (\e2 -> \e1 -> e2++["push\t%rax"]++e1++["pop\t%rcx"]++instr) <$> (exprToInstr expr2) <*> (exprToInstr expr1)
comparison c expr1 expr2 = flip (++) ["set"++c++" %al"] <$> (binaryOp ["cmpl\t%ecx,%eax"] expr1 expr2 )

parentheses :: Parser Token a -> Parser Token a
parentheses p = (\_ a _ -> a) <$> symbol LeftBracket undefined <*> p <*> symbol RightBracket undefined

pExpressionLast :: Parser Token Expression
pExpressionLast = ((\a b   -> a b)   <$> (symbol Minus ENegate<|>symbol Complement EComplement<|>symbol Not ENot) <*> pExpressionLast)
           <|> Parser getNumFromToken <|> parentheses pExpression <|> Parser getVarFromToken
  where
    getNumFromToken ((Number n):r) = Valid (ENumber n,r)
    getNumFromToken _ = Error "Expected a number"

    getVarFromToken ((Variable s):r) = Valid (EVariable s,r)
    getVarFromToken _ = Error "Expected a string"

--pExpression2 :: Parser Token Expression
--pExpression2 = (chain <$> pExpression1 <*> many ((,)<$>(symbol Multiply EMultiply <|> symbol Divide EDivide) <*>pExpression1)) <|> parentheses pExpression
--
--pExpression :: Parser Token Expression
--pExpression = (chain <$> pExpression2 <*> many ((,)<$>(symbol Add EAdd <|> symbol Minus ESubtract) <*>pExpression2)) <|> parentheses pExpression

dualExpressionP :: ([Parser Token (Expression->Expression->Expression)],Direction) -> Parser Token Expression -> Parser Token Expression -> Parser Token Expression
dualExpressionP (ps,d) child top = (chain d <$> child <*> many ((,)<$>(foldr1 (<|>) ps) <*>child)) <|> parentheses top

pExpression = foldr (\p q -> p q pExpression) pExpressionLast $ map dualExpressionP $ map (\(l,d)-> (map (\(a,b) -> symbol a b) l, d)) precedenceList 

precedenceList = [([(Assign,EAssign)],R),
                  ([(LogicalOr,ELogOr)],L),
                  ([(LogicalAnd,ELogAnd)],L),
                  ([(Equal,EEqual),(NotEqual,ENotEqual)],L),
                  ([(Less,ELess),(Leq,ELeq),(Greater,EGreater),(Geq,EGeq)],L),
                  ([(Add,EAdd),(Minus,ESubtract)],L),
                  ([(Multiply,EMultiply),(Divide,EDivide)],L)]

data Direction = L | R

chain :: Direction -> e -> [(e->e->e,e)] -> e
chain L e ((op,e'):opes) = chain L (op e e') opes
chain R e ((op,e'):opes) = op e (chain R e' opes)
chain _  e []  = e


pStatement = pCodeBlock <|> ((\a _ -> SExpression a) <$> pExpression <*> symbol Semicolon undefined) <|> ((\a _ -> a) <$> pDeclaration <*> symbol Semicolon undefined) <|> ((\_ e _ ->SReturn e)<$>symbol Return undefined <*> pExpression <*> symbol Semicolon undefined)

pDeclaration = ((\_ s _ e -> SDecl s (Just e)) <$> symbol Integer undefined <*> (Parser getVarFromToken) <*> symbol Assign undefined <*> pExpression)<|>((\_ s -> SDecl s Nothing) <$> symbol Integer undefined <*> (Parser getVarFromToken))
  where
    getVarFromToken ((Variable s):r) = Valid (s,r)
    getVarFromToken _ = Error "Expected a string"

pCodeBlock = (\_ a _ -> SCodeBlock a) <$> symbol LeftCurlyBracket undefined <*> many pStatement <*> symbol RightCurlyBracket undefined


cParser :: Parser Token String
cParser = (\_ _ _ _ stat-> generateProgram stat) <$>
          symbol Integer Integer <*> symbol Main Main <*> symbol LeftBracket LeftBracket
          <*> symbol RightBracket RightBracket 
          <*> pStatement
  where
    generateProgram stat= "\t.text\n \t.globl\tmain\n main:\n"
                            ++concatMap (\i->'\t':i++"\n") (["mov\t%rsp,%r10"] ++ (evalState  (statToInstr (traceShowId stat)) ([],0,0)))
type VarDic = [(String,Int)]
type PopAmount = Int
type LabelCount = Int
type ProgInfo = (VarDic,PopAmount,LabelCount)

data GlobVar a = GlobVar ProgInfo a

instance Functor GlobVar where
  fmap f (GlobVar i a) = GlobVar i (f a)

instance Applicative GlobVar where
  pure x = GlobVar ([],0,0) x

instance Monad GlobVar where
  GlobVar (var,popam,labelc)  a >>= f = GlobVar (var++var',popam+popam',labelc'+labelc) a'
    where
      GlobVar (var',popam',labelc') a' = f a

statToInstr :: Statement ->  State ProgInfo [String]

clearLocal (_,n,_) (_,n',_) = ["add\t$"++show (n-n')++", %rsp"]

statToInstr (SExpression expr) = exprToInstr expr
statToInstr (SDecl s mexpr) =do 
                                vars <- get
                                put $  nvars vars
                                flip (++) ["push\t%rax"] <$> instrBeforePush
  where
    nvars (l,n,nl) = case lookup s l of
                       Just _ -> error $ "Variable \""++s++"\" already exists"
                       Nothing -> ((s,n-8):l,n-8,nl)
    instrBeforePush  = case mexpr of
                         Just expr -> exprToInstr expr
                         Nothing   -> return []



statToInstr (SCodeBlock stats) = do vars <- get
                                    flip (++) ["movl\t$0,%eax","ret"] <$> ((++) <$> instrs <*> (clearLocal vars <$> get))
  where
    instrs = foldlM f [] stats
    f instr stat = (instr++) <$> statToInstr stat
statToInstr (SReturn expr) = flip (++) ["ret"] <$> ((++) <$> exprToInstr expr <*> (clearLocal (undefined,0,undefined) <$> get))

data Token = Integer | Main | LeftBracket | RightBracket | LeftCurlyBracket |
             RightCurlyBracket | Return | Semicolon | Number Int | Minus |
             Not | Complement | Add | Multiply | Divide | LogicalAnd | LogicalOr |
             Equal | NotEqual | Less | Leq | Greater | Geq | Variable String | Assign
  deriving (Show, Eq)

wordDictionary = [("int",Integer),("main",Main),("return", Return),(";",Semicolon)]

symbolDictionary = [("!=",NotEqual),("&&",LogicalAnd),("||",LogicalOr),
                    ("==",Equal),("<=",Leq),(">=",Geq),(";",Semicolon),                     ("-",Minus),("!",Not),("~",Complement),("+",Add),
                    ("*",Multiply), ("/",Divide), ("<",Less),
                    (">",Greater), ("=",Assign)]
bracketDictionary = [("(",LeftBracket),(")",RightBracket),("{",LeftCurlyBracket),
                    ("}",RightCurlyBracket)]

pNumber = Number . digitsToNum <$> many1 digit
pVariable  = Variable <$> ((:) <$> pAbc <*> many pAbc123)
pAbc :: Parser Char Char
pAbc = Parser f
  where
    f (c:cs) | c `elem` (['a'..'z']++['A'..'Z']) = Valid (c,cs)
             | otherwise = Error "Not in the alphabet"
    f [] = Error "No charachter"
pAbc123 :: Parser Char Char
pAbc123 = Parser f
  where
    f (c:cs) | c `elem` (['a'..'z']++['A'..'Z']++['0'..'9']) = Valid (c,cs)
             | otherwise = Error "Not in the alphabet"
    f [] = Error "No charachter"

digit :: Parser Char Int
digit = Parser digitf

digitf:: String -> Error (Int, String)
digitf [] = Error "A digit was expected"
digitf (d:ds) = if d >='0' && d<='9' then Valid (read [d],ds) else Error (d:" is not a digit")

digitsToNum :: [Int] -> Int
digitsToNum = foldl (\b a -> a+10*b) 0

wordSep = [' ','\n','\r','(',')','{','}',';']
symbSep = ['a'..'z']++['A'..'Z']++['0'..'9']++wordSep

dicToParser :: [(String,Token)] -> (Char->Bool) -> Parser Char Token
dicToParser dic sep = foldl1 (<|>) (map (\(w,t) -> word w t sep)  dic)

pToken = dicToParser wordDictionary (`elem` wordSep) <|> dicToParser symbolDictionary (const True) <|> dicToParser bracketDictionary (const True) <|> pNumber <|> pVariable

pSpaces = many pSpace

pSpace = symbol ' ' ' ' <|> symbol '\n' '\n' <|> symbol '\r' '\r' <|> symbol '\t' '\t'

cTokenize :: Parser Char [Token]
cTokenize = (\a b -> b) <$> pSpaces <*> (many ((\a b -> a) <$> pToken <*> pSpaces))


