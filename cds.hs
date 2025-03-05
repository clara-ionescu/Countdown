import Choices


data Op = Add | Sub | Mul | Div deriving Eq

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"


apply :: Integral a => Op -> a -> a -> a
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

valid :: (Ord a, Integral a) => Op -> a -> a -> Bool
valid Add x y  =  x <= y
valid Sub x y  =  x > y
valid Mul x y  =  x <= y && x /= 1 && y /= 1
valid Div x y  =  y /= 1 && x `mod` y == 0
  
data Term a = Val a | App Op (Term a) (Term a)

instance Show a => Show (Term a) where
  show (Val n)         =  show n
  show (App op e1 e2)  =  brack e1 ++ show op ++ brack e2
    where
      brack :: Term a -> String
      brack e@(App _ _ _)  =  "(" ++ show e ++ ")"      
      brack (Val n)        =  show n

values :: Term a -> [a]
values (Val n)        =  [n]
values (App o e1 e2)  =  values e1 ++ values e2


eval :: (Ord a, Integral a) => Term a -> [a]
eval (Val n)        =  [n | n > 0]
eval (App o e1 e2)  =  [apply o x y | x <- eval e1,
                                      y <- eval e2,
                                      valid o x y]

-- solution e ns t =  elem (values e) (choices ns) && eval e == [t]

-- type Result = (Term, Int)


istIN :: Op -> Term a -> Bool   
istIN _ (Val _)        = False  
istIN op (App op' _ _) = op == op'
-- takes an Operator an Termession and gives back a bool, if you give it any operator and an expression that does                                     not have an operator it just gives you back false , else it gives you true if you give it an operator and an                                      expression with the same operator




type Struct a = ([Op],                           -- operations allowed in the structure
                 a -> Bool,                      -- ad-hoc conditions on parameters of expressions (like n>0)
                 Op -> Term a -> Term a -> Bool, -- associativity
                 Op -> a -> a -> Bool,           -- commutativity
                 Op -> a -> a -> Bool,           -- units
                 Op -> a -> a -> Bool)           -- ad-hoc conditions on operations (like for sub x > y)
-- The type struct has a parameter a wich is a list of op together with all these functions named above


ops                     :: Struct a -> [Op]
validv                  :: Struct a -> a -> Bool
valida                  :: Struct a -> Op -> Term a -> Term a -> Bool
validc, validu, validah :: Struct a -> Op -> a -> a -> Bool

ops     (o, fv, fa, fc, fu, fah) = o
validv  (o, fv, fa, fc, fu, fah) = fv
valida  (o, fv, fa, fc, fu, fah) = fa
validc  (o, fv, fa, fc, fu, fah) = fc
validu  (o, fv, fa, fc, fu, fah) = fu
validah (o, fv, fa, fc, fu, fah) = fah

results :: (Ord a, Integral a) =>
             Struct a ->
             [a] -> [(Term a, a)]
results s []    =  []
results s [n]   =  [(Val n, n) | validv s n]
results s ns    =  [(App o e1 e2, apply o v1 v2) | (l, r)   <- split ns,
                                                   (e1, v1) <- results s l,
                                                   (e2, v2) <- results s r,
                                                   o <- ops s,
                                                   valida s o e1 e2, 
                                                   validc s o v1 v2,
                                                   validu s o v1 v2,
                                                   validah s o v1 v2]
-- results takes a structure with a parameter and a list of these parameters and gives back an expression with the value of this expression this function is within the ord and integral. results of any structurce and the empty list is the empty list. results of a structure and a singleton list is the value of the paramter inside this list given it is a valid value.(Adhoc conditions) results fo a structure and a list of parameters gives back the list of all expressions together with the the result of applying apply to the expression and does this through splitting ns in a left and right part and than recursively applying this so that you get all the values. o then gets taken out of the operations and the validity for associativity, commutativity, unit, and adhoc conditions get checked.


solutions :: (Ord a, Integral a) => Struct a -> [a] -> a -> [Term a]
solutions s ns t = [e | xs <- choices ns, (e, v) <- results s xs, v == t]

-- solutions takes a structure and a list of paramaters and an parameter which is the target and gives back all expressions that are a solution l.a.f.r. soltutions of a structure s a list ns and a target t gives back all expressions where xs comes from all the choices of ns, the pair e, v comes fom applying results to s and xs and where v equals the target



-- Original countdown problem --

orig_v :: (Ord a, Integral a) => a -> Bool -- selfexplenanatory
orig_v n = n > 0

orig_a :: Op -> Term a -> Term a -> Bool 
orig_a Sub _ _               = True
orig_a Div _ _               = True
orig_a op _  (Val _)         = True
orig_a Add e1 (App Sub _ _)  = False
orig_a op e1 (App op' _ _)   = op /= op'  -- reject x + (y + z)
-- the original countdown problem has associativity for add and mul and add-sub if you give it any operator and an val than it is true if you give it add and sub then it is false and if you give it an operator and another expr. it is true if the two operators are different


orig_c :: Ord a => Op -> a -> a -> Bool
orig_c Add x y = x <= y
orig_c Mul x y = x <= y
orig_c _ _ _   = True


orig_u :: (Eq a, Num a) => Op -> a -> a -> Bool
orig_u Add x y = x /= 0 && y /= 0
orig_u Mul x y = x /= 1 && y /= 1
orig_u Sub x y = y /= 0
orig_u Div x y = y /= 1

orig_ah :: (Ord a, Integral a) => Op -> a -> a -> Bool
orig_ah Sub x y = x > y
orig_ah Div x y = x `mod` y == 0
orig_ah _ _ _   = True
-- the adhoc condition for expression on operations is that you cant go below zero and only natural numbers so sub and mul


orig_s :: Struct Int
orig_s = ([Add, Sub, Mul, Div],
          orig_v,
          orig_a,
          orig_c,
          orig_u,
          orig_ah)
         

-- Monoid ([a], (++), [])

instance Num [a] where
  (+)         = (++)
  (*)         = undefined
  abs         = undefined
  signum      = undefined
  fromInteger = undefined
  (-)         = undefined

instance Ord a => Real [a] where
  toRational = undefined
  
instance Enum [a] where
  toEnum = undefined
  fromEnum = undefined
  
instance Ord a => Integral [a] where
  div = undefined
  quotRem = undefined
  toInteger = undefined
-- Here is the Structure being defined first the list of numbers is set as being of type num, the operations are then defined and the underclasses


lmon_a :: Op -> Term a -> Term a -> Bool
lmon_a Add _ (App Add _ _) = False -- reject xs ++ (ys ++ zs)
lmon_a Add _ (Val _)       = True

lmon_u :: Op -> [a] -> [a] -> Bool
lmon_u o xs ys = not (null xs) && not (null ys)

lmon_s :: (Ord a, Integral a) => Struct [a]
lmon_s = ([Add],
          const True, -- no ad-hoc conditions on params
          lmon_a,
          \ _ _ _ -> True, -- no commutativity
          lmon_u,
          \ _ _ _ -> True) -- no ad-hoc conditions on operations

-- Here the different functions needed are defined. Only associativity and unit are important. comutativity, does not exist and neither do adhoc conditions on operations



-- Example: [[3], [4, 5], [1], [2, 3], [1, 4], [2]] [1, 2, 3, 4, 5]

-- Commutative Monoid (Z, + , 0)



comm_v :: (Ord a, Integral a) => a -> Bool
comm_v n = n > 0


comm_a :: Op -> Term a -> Term a -> Bool
comm_a op _  (Val _)         = True
comm_a op e1 (App op' _ _)   = op /= op'

comm_c :: Ord a => Op -> a -> a -> Bool
comm_c Add x y = x <= y

comm_u :: (Eq a, Num a) => Op -> a -> a -> Bool
comm_u Add x y = x /= 0 && y /= 0

--comm_ah :: (Ord a, Integral a) => Op -> a -> a -> Bool
--comm_ah _ _ _   = True

comm_s :: Struct Int
comm_s = ([Add],
          comm_v,
          comm_a,
          comm_c,
          comm_u,
          \_ _ _ -> True)





-- Groups: 2x2 non-singular matrices --
-- its associativ, untit , convertible - any program that works with monoid of natural number works in same way on matrix - find appendix

type Matrix = ((Float, Float), (Float, Float))

i2 :: Matrix
i2  = ((1, 0), (0, 1))

instance Num Matrix where
  ((a,b), (c, d)) - ((u, v), (w, z))  = ((a-u,b-v),(c-w,d-z))
  ((a, b), (c, d)) * ((u, v), (w, z)) = ((a*u+b*w, a*v+b*z), (c*u+d*w, c*v+d*z))
  ((a, b), (c,d)) + ((u,v),(w,z))     = ((a+u,b+v),(c+w, d+z))
  abs         = undefined
  signum      = undefined
  fromInteger = undefined

instance Real Matrix where
  toRational = undefined
  
instance Enum Matrix where
  toEnum   = undefined
  fromEnum = undefined
  
instance Integral Matrix where
  div m1 ((a, b), (c, d)) = m1 * ((d/det, -b/det), (-c/det, a/det))
    where det = a*d - b*c
  quotRem   = undefined
  toInteger = undefined

determinant :: Matrix -> Float
determinant ((a, b), (c, d)) = a*d - b*c

valid_Mat :: Matrix -> Bool
valid_Mat m = determinant m /= 0

valida_Mat :: Op -> Term Matrix -> Term Matrix -> Bool
valida_Mat Mul _ (App Mul _ _) = False
valida_Mat Mul _ (App Div _ _) = False
valida_Mat Div _ (App Div _ _) = False
valida_Mat Div (App Div _ _) _ = False
valida_Mat _ _ _               = True

validu_Mat :: Op -> Matrix -> Matrix -> Bool
validu_Mat Mul x y = x /= i2 && y /= i2
validu_Mat Div x y = y /= i2

mats_s :: Struct Matrix
mats_s =  ([Mul, Div],
           valid_Mat,
           valida_Mat,
           \ _ _ _ -> True, -- matrix multiplication is not commutative
           validu_Mat,
           \ _ _ _ -> True) -- no ad-hoc conditions on operations
          
mats :: [Matrix]
mats = [((1, 2), (2, 1)), ((1, 0), (1, 1)), ((1, 2), (0, 1)), ((2, 2), (0, 1)), ((2, 1), (1, 0)), ((1, 1), (2, 1))]
matt :: Matrix
matt = ((1, -1), (-1, 4))

-- Commutative Group (Z, +, 0)


comg_v :: (Ord a, Integral a) => a -> Bool
comg_v n = n > 0

comg_a :: Op -> Term a -> Term a -> Bool
comg_a Sub _ _               = True
comg_a op _  (Val _)         = True
comg_a Add e1 (App Sub _ _)  = False
comg_a op e1 (App op' _ _)   = op /= op'

comg_c :: Ord a => Op -> a -> a -> Bool
comg_c Add x y = x <= y
comg_c Sub x y = True

comg_u :: (Eq a, Num a) => Op -> a -> a -> Bool
comg_u Add x y = x /= 0 && y /= 0
comg_u Sub x y = y /= 0

comg_ah :: (Ord a, Integral a) => Op -> a -> a -> Bool
comg_ah Sub x y = x > y
comg_ah _ _ _   = True

comg_s :: Struct Int
comg_s = ([Add, Sub],
          comg_v,
          comg_a,
          comg_c,
          comg_u,
          comg_ah)
         

       
-- Ring: 2x2 matrices --
-- list ab dc - list xy uv 
         {-
>>> (A + B)*(C + D).inv()*(E + F)
⎡1  1⎤
⎢    ⎥
⎣0  1⎦
>
-}
valid_Matr m = True -- determinant m /= 0

valida_Matr :: Op -> Term Matrix -> Term Matrix -> Bool
valida_Matr Mul _ (App Mul _ _) = False
valida_Matr Mul _ (App Div _ _) = False
valida_Matr Div _ (App Div _ _) = False
valida_Matr Div (App Div _ _) _ = False
valida_Matr Add _ (App Add _ _) = False
valida_Matr Add _ (App Sub _ _) = False
valida_Matr Sub _ (App Sub _ _) = False
valida_Matr _ _ _               = True

validc_Matr Add m1 m2  =  determinant m1 <= determinant m2 && fst (fst m1) <= fst (fst m2)
validc_Matr Mul m1 m2  =  determinant m1 <= determinant m2 && fst (fst m1) <= fst (fst m2)
validc_Matr _ _ _    =  True

o2 = ((0, 0), (0, 0))

validu_Matr Add x y  =  x /= o2 && y /= o2
validu_Matr Mul x y  =  x /= i2 && y /= i2
validu_Matr Sub x y  =  y /= o2
validu_Matr Div x y  =  y /= i2

validah_Matr Add x y =  determinant (x+y) /= 0
validah_Matr Mul _ _ =  True
validah_Matr Div _ _ =  True
validah_Matr Sub x y =  determinant (x-y) /= 0
 
matsr_s :: Struct Matrix
matsr_s =  ([Add, Sub, Mul, Div],
           valid_Matr,
           valida_Matr,
           validc_Matr,
           validu_Matr,
           validah_Matr) 
