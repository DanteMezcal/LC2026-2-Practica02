module Practica02 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

--EJERCICIOS

--Ejercicio 1
variables :: Prop -> [String]
variables (Cons _) = []
variables (Not p) = noRep (variables p)
variables (Or p q) = noRep (variables p ++ variables q)
variables (And p q) = noRep (variables p ++ variables q)
variables (Impl p q) = noRep (variables p ++ variables q)
variables (Syss p q) = noRep (variables p ++ variables q)
variables (Var p) = [p]

--Funcion auxiliar para el ejercicio 1
noRep :: Eq a => [a] -> [a]
noRep [] = []
noRep (x:xs)
    | x `elem` xs = noRep xs
    | otherwise = x : noRep xs

--Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons True) _ = True
interpretacion (Cons False) _ = False
interpretacion (Var p) i = p `elem` i
interpretacion (Not p) i = not (interpretacion p i)
interpretacion (Or p q) i = (interpretacion p i) || (interpretacion q i)
interpretacion (And p q) i = (interpretacion p i) && (interpretacion q i)
interpretacion (Impl p q) i = not (interpretacion p i) || (interpretacion q i)
interpretacion (Syss p q) i = (interpretacion (Impl p q) i) && (interpretacion (Impl q p) i)

--Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles prop = conjPotencia(variables prop)

--Ejercicio 4
modelos :: Prop -> [Estado]
modelos prop = [x | x <- (estadosPosibles prop), (interpretacion prop x)]

--Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes phi1 phi2
    | interpretacion (phi1) (variables phi2) == interpretacion (phi2) (variables phi1) = True
    | otherwise = False

--Ejercicio 6 
tautologia :: Prop -> Bool
tautologia phi
    | False `elem` iterarInterpretaciones phi (estadosPosibles phi) = False
    | otherwise = True

--Ejercicio 7
contradiccion :: Prop -> Bool
contradiccion = undefined

--Ejercicio 8
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica = undefined

--Funcion auxiliar
iterarInterpretaciones :: Prop -> [Estado] -> [Bool]
iterarInterpretaciones prop (e:es) = interpretacion prop e : iterarInterpretaciones prop es
iterarInterpretaciones _ [] = []

--Funcion auxiliar
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs

f1 = Syss (Not (Or (Var "p") (Var "q"))) (And (Not (Var "p")) (Not (Var "q")))