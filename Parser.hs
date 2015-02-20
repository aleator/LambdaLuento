{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE DeriveFunctor#-}
module Parser where
import Text.Parser.Token
import Text.Trifecta.Parser
import Text.Parser.Char
import Text.Parser.Combinators
import Data.List hiding ((\\))
import Text.Trifecta.Combinators
import Control.Applicative
import Test.QuickCheck hiding (reason,Success)
import Text.Parser.Token.Style
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Data.String
import Data.Tree
import Data.Monoid
import Data.Maybe
import qualified Data.Set as S
import Data.Set ( (\\) )
import qualified Data.Map as M

data LC a = V a | L a (LC a) | A (LC a) (LC a) deriving (Functor, Show, Eq)
type Expr = LC String

-- QC
instance Arbitrary (LC String) where
    arbitrary = sized $ \n -> case n of
                          0 -> (V <$> var )
                          _ -> oneof [(L <$> var <*> (resize (n`div`2) arbitrary))
                                     ,(A <$> (resize (n`div`2) arbitrary) 
                                         <*> (resize (n`div`2) arbitrary))]
                    where var = elements (map bx ['a'..'z'])
    shrink (V _)   = []
    shrink (L x b) = [V x, b]
    shrink (A (V a) (V b)) = [V a, V b] 
    shrink (A (V a) b)  = [V a, b, A (V a) (V "x"), (V a), b] 
    shrink (A a (V b))  = [a,V b, A a (V "x")] 
    shrink (A a b) = [a, b, A (V "x") b, A a (V "x"), a, b] 

bx x = [x]
                                    
ppLC (V a)   = PP.text a
ppLC (L a e) = PP.braces $ PP.text a PP.<+> PP.text "=>"  PP.<+> ppLC e 
-- ppLC (A a (L la le)) = ppLC a PP.<> PP.brackets (PP.text la PP.<+> PP.text "=>" PP.<+> ppLC le)
ppLC (A a e) = ppLC a PP.<> PP.parens (ppLC e)

a <:> b = a PP.<> PP.comma PP.<+> b
reason a = PP.text "Joka on muotoa" PP.<+> a PP.<+> "missä"
toTree (V a)   = Node (PP.text a <:> "eli muuttuja") []
toTree x@(L a e) = Node (ppLC x)
                    [Node (reason " {○ => ●}") []
                    ,addDescription (" ○ on ") (toTree (V a)) 
                    ,addDescription (" ● on ") (toTree e)]
toTree x@(A a e) = Node (ppLC x )
                    [Node (reason " ■(●) ") []
                    ,addDescription "■ on " (toTree a)
                    ,addDescription "● on " (toTree e)]

addDescription d (Node as bs) = Node (d <> as) bs

-- Manipulation & Evaluator

fv (V a)   = S.singleton a
fv (A a b) = fv a <> fv b
fv (L a b) = (fv b) \\ S.singleton a


replace :: (IsString a, Monoid a, Ord a) => LC a -> a -> LC a -> LC a
replace expr var arg = replace' expr var arg (fv arg) 
 where
  bump x = x<>"'"
  -- replace' :: (Monoid a, IsString a, Ord a) => LC a -> LC a -> S.Set a -> LC a
  replace' (V a) var arg forbid 
    | a == var   = arg
    | otherwise  = V a 
  replace' (A a b) var arg forbid = A (replace' a var arg forbid) 
                                      (replace' b var arg forbid)
  replace' (L x b) var arg forbid  
   | x == var            = L x b 
   | x `S.member` forbid
   , var `S.member` (fv b)
   , x`S.member` (fv b)  = let exp' = L (bump x) (replace' b x (V (bump x)) 
                                                   (forbid<>S.singleton (bump x)))
                           in replace' exp' var arg forbid
   | otherwise           = L x (replace' b var arg forbid)

evaluate (A (L x e) b)  = Just (replace e x b) 
evaluate (A e b)        = (A <$> pure e <*> evaluate b) 
                           <|> (A <$> evaluate e <*> pure b)
evaluate (L x y)        = L x <$> (evaluate y)
evaluate _              = Nothing

expand defs (V x) | Just d <- M.lookup x defs = Just d
expand defs (A e1 e2) = (A <$> expand defs e1 <*> pure e2) <|>
                        (A <$> pure e1 <*> expand defs e2)
expand defs (L v e)   = L v <$> expand (M.delete v defs) e
expand defs _ = Nothing

data Reduct a = Arg a | Param (LC a) | Reduct (LC a) | P (LC a) deriving (Show, Functor)

hil x (V y) | x==y = V (Reduct (V x))
hil _ (V y) = V (P (V y))
hil x (A e1 e2) = A (hil x e1) (hil x e2)
hil x (L y e) | x == y = L (P (V y)) (fmap (P . V) e)
hil x (L y e) = L (P (V y)) (hil x e) 

annotateRedex :: Eq a => LC a -> (LC (Reduct a))
annotateRedex lc = fromMaybe (fmap (P . V) lc) (annot lc)
  where
    annot x@(A (L v e) a) = 
        Just (A (L (Arg v) (hil v e)) (V (Param a)))
    annot (A e b)        = (A <$> pure (V (P e)) <*> annot b) 
                           <|> (A <$> annot e <*> pure (V (P b)))
    annot (L x y)        = L (P (V x)) <$> (annot y)
    annot (V x)          = Nothing  -- Just (V (P (V x)))
    annot _              = Nothing

-- Expr


lexpr :: Parser (LC String)
lexpr = 
            try ((L <$> (identifier<?> "parameter") 
                   <* (whiteSpace *> string "=>" <*whiteSpace)
                   <*> (lexpr <?> "function body"))<?> "function")
        <|> try ((do
                 f <- term
                 apps f)<?> "application")
        <|> term

apps x  = foldl' A x <$>  some (parens lexpr)
term  = V <$> identifier
          <|> braces lexpr

identifier :: (Monad m, TokenParsing m) => m String
identifier = ident haskellIdents <|> string "○" <|> string "◊" <|> string "●" <|> string "■"
