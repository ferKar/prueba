{-
- Lenguajes de programación 2018-1
- Profesora: Susana H. Martín Lunas
- Ayudante: Alejandra Krystel Colapa Díaz
- Laboratorio: Fernando A. Galicia Mendoza
- Implementación del lenguaje PostFix.
-
- Integrantes:
- Jiménez Gutiérrez Karla Fernanda - 313131170
- Vargas Alba Alfonso -313010929
-}

module EAB where
import Data.List
type Nombre = String
data VBool = T | F deriving(Show)


data E = Var Nombre|N Int| True | False | VB Bool|If E E E|Suma E E| Prod E E|Ltn E E|Eqn E E| Conj E E|Disy E E|Neg E|Iz E| Suc E|Pred E| Let Nombre E E deriving(Show)

type Sust = (Nombre, E)

------------------------------------------------------
fv :: E -> [Nombre]
fv (N n) = []
fv (Var x) = [x]
fv (Neg x) = fv x
fv (Pred x) = fv x
fv (Suc x) = fv x
fv (Iz x) = fv x
fv (Suma x y) = fv x `union` fv y
fv (Prod x y) = fv x `union` fv y
fv (Ltn x y) = fv x `union` fv y
fv (Eqn x y) = fv x `union` fv y
fv (Conj x y) = fv x `union` fv y
fv (Disy x y) = fv x `union` fv y
fv (If x y z) = fv x `union` fv y `union` fv z
fv (Let w x y) = [z |  z <- fv x ++ fv y, z /= w]

