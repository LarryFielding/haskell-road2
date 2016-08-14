-----------------------------------------------------------------
--                                                             --
--                                                             --
--  Logic                                                      --
--                                                             --
--                                                             --
-----------------------------------------------------------------

module Logic 

where 

type Id       = (String,Integer) 
type Name     = String
data Term     = Var Id | Struct Name [Term] deriving (Eq,Show)

type Subst = Term -> Term

data Qform = Atom Name [Term]
           | Neg Qform
           | Conj Qform Qform 
           | Disj Qform Qform 
           | Impl Qform Qform
           | Equi Qform Qform 
           | Forall Id Qform 
           | Exists Id Qform
     deriving (Eq,Show)

varsInTerm              :: Term -> [Id]
varsInTerm (Var i)       = [i]
varsInTerm (Struct i ts) = 
      (remove_dups . concat . map varsInTerm) ts

remove_dups :: Eq a => [a] -> [a]
remove_dups [] = []
remove_dups (x:xs) = (x: remove_dups (delete x xs))

delete :: Eq a => a -> [a] -> [a] 
delete x [] = []
delete x (y:ys) | x == y    = delete x ys
                | otherwise = y:(delete x ys)

fvarsInQform              :: Qform -> [Id]
fvarsInQform (Atom a ts) = (remove_dups . concat . map varsInTerm) ts
fvarsInQform (Neg form)  = fvarsInQform form
fvarsInQform (Conj form1 form2) = 
    remove_dups ((fvarsInQform form1) ++ (fvarsInQform form2))
fvarsInQform (Disj form1 form2) = 
    remove_dups ((fvarsInQform form1) ++ (fvarsInQform form2))
fvarsInQform (Impl form1 form2) = 
    remove_dups ((fvarsInQform form1) ++ (fvarsInQform form2))
fvarsInQform (Equi form1 form2) = 
    remove_dups ((fvarsInQform form1) ++ (fvarsInQform form2))
fvarsInQform (Forall v form) = delete v (fvarsInQform form) 
fvarsInQform (Exists v form) = delete v (fvarsInQform form) 

ignore :: Id -> Subst -> Subst 
ignore v s w | (Var v) == w  = w
             | otherwise     = s w 

app   :: Subst -> Qform -> Qform 
app s (Atom a ts)            = Atom a (map s ts)
app s (Neg form)             = Neg (app s form)
app s (Conj form1 form2)     = Conj (app s form1) (app s form2)
app s (Disj form1 form2)     = Disj (app s form1) (app s form2)
app s (Impl form1 form2)     = Impl (app s form1) (app s form2)
app s (Equi form1 form2)     = Equi  (app s form1) (app s form2)
app s (Forall v form)        = Forall v (app (ignore v s) form)
app s (Exists v form)        = Exists v (app (ignore v s) form)


infix  6 :-
data Clause =  Term :- [Term] deriving Show
type Goal   =  [Term]

