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


