data SExpr = StringLit String | List [SExpr]

instance Show SExpr where
	show (StringLit s) = s
	show (List e) = show e

reader :: String -> SExpr
reader s = let (sexpr, _) = readWithRest s in sexpr

readL :: String -> SExpr -> (SExpr,String)
readL (')' : tail) (List acc) = (List acc, tail)
readL (x : xs) (List acc) = let (next, rest) = readWithRest(x : xs) in readL rest (List (acc ++ [next]))
readL _ _ = error "unterminated list"

readChars :: String -> String -> (SExpr, String)
readChars (' ' : tail) acc = (StringLit acc, [' '] ++ tail)
readChars (')' : tail) acc = (StringLit acc, [')'] ++tail)
readChars ('(' : tail) acc = (StringLit acc, ['('] ++tail)
readChars (c : tail) acc = readChars tail (acc ++ [c])
readChars [] acc = (StringLit acc, [])

readWithRest :: String -> (SExpr,String)
readWithRest (' ' : tail) = readWithRest tail
readWithRest ('(' : tail) = readL tail (List [])
--readWithRest ('"' : tail) = readStringLit tail ['"']
readWithRest (c : tail) = readChars (c : tail) []

--readStringLit :: String -> String -> (SExpr, String)
--readStringLit ('"' : tail) acc = (StringLit (acc ++ ['"']), tail)
--readStringLit (c : tail) acc = readStringLit tail (acc ++ [c])

data Exp = StringLit2 String | Concat Exp Exp | RestAfter Exp Exp

instance Show Exp where
	show (StringLit2 s) = s
	show (Concat l r) = show l ++ " & " ++ show r
	show (RestAfter l r) = show l ++ " @ " ++ show r

parser :: SExpr -> Exp
parser (StringLit s) = StringLit2 s
parser (List (xs : (StringLit "&") : ys : [])) = Concat (parser xs) (parser ys)
parser (List (xs : (StringLit "@") : ys : [])) = RestAfter (parser xs) (parser ys)
parser _ = error "unexpected token"

finder :: String -> String -> String
finder findme instring =
    if (length findme) > (length instring) then error (findme ++ " not found")
    else if findme == (take (length findme) instring) then drop (length findme) instring
    else finder findme (drop 1 instring)

interpreter :: Exp -> String
interpreter (StringLit2 s) = s
interpreter (Concat l r) = (interpreter l) ++ (interpreter r)
interpreter (RestAfter l r) = finder (interpreter r) (interpreter l)

