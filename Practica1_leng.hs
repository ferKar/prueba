{-
- Lenguajes de programación 2018-1
- Profesora: Susana H. Martín Lunas
- Ayudante: Alejandra Krystel Colapa Díaz
- Laboratorio: Fernando A. Galicia Mendoza
- Implementación del lenguaje PostFix.
-
- Integrantes:
- Jiménez Gutiérrez Karla Fernanda - 313131170
-}
module LenguajePostfix where

--Sintaxis
data PF = Postfix deriving(Show)
data Comando = L Int|ADD|SUB|MUL|DIV|REM|LOT|EQN|GTN|POP|SWAP|SEL|NGET|EXEC|SEC[Comando] deriving(Show)
type Programa = (PF, Int,[Comando])
type Pila = [Comando]

--Semántica
--Función verifArgs : verifica que la n coincida con la longitud de la pila
verifArgs :: Programa -> Pila -> Bool
verifArgs (_,n,_) p = n == length(p)

--Función op : realiza comandos aritmeticos, asumiendo los primeros 2 elementos tipo L Int.
op :: Comando -> Pila -> Pila
op _ [] = error "Error: Argumentos insuficientes."
op ADD ((L a1):(L a2):l) = ((L (a2 + a1):l)) 
op SUB ((L a1):(L a2):l) = ((L (a2 - a1):l)) 
op MUL ((L a1):(L a2):l) = ((L (a2 * a1)):l)
op REM ((L a1):(L a2):l) = ((L (mod a2 a1)):l)
op EQN ((L a1):(L a2):l)
  |a2 == a1 = ((L 1):l)
  |otherwise = ((L 0):l)
op LOT ((L a1):(L a2):l)
  |a2 < a1 = ((L 1):l)
  |otherwise = ((L 0):l)
op GTN ((L a1):(L a2):l)
  |a2 > a1 = ((L 1):l)
  |otherwise = ((L 0):l)
op DIV ((L a1):(L a2):l)
  | a1 /= 0 = ((L (quot a2 a1)):l)
  |otherwise = error "Error Aritmético: División entre cero."

op _ _ = error "Error: Argumentos insuficientes."

--Función opPila : realiza los comandos de pila.
opPila :: Comando -> Pila -> Pila
opPila _ [] = error "Error: Argumentos insuficientes."
opPila (L x) p = (L x):p
opPila SWAP (a:b:ls) = (b:a:ls)
opPila POP (l:ls) = ls
opPila SEL (a:b:(L 0):ls) = (a:ls)
opPila SEL (a:b:(L x):ls) = (b:ls)
opPila NGET ((L x):ls)
  |(x <= length(ls)) && (x > 0) = [nGetter (ls !! (x - 1))] ++ ls
  |otherwise = error "Error NGET: Índice fuera de rango." 
opPila _ _ = error "error: Argumentos insuficientes."

--Funcion nGetter:Funcion auxilia para el opPila con NGET, vefifica que sea una literal
nGetter :: Comando -> Comando
nGetter (L x) = (L x)
nGetter _ = error "Error NGET: J-ésimo elemento no entero."

--Función opaux: Ejecuta los comandos y dar el resultado (tope)de la pila, esto con ayuda de op y opPila.
opaux :: Programa -> Pila -> Comando
--Casos base
opaux (s,n,[]) p
  |verifArgs (s,n,[]) p = tope p
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[L x]) p
  |verifArgs (s,n,[L x]) p = tope ([L x] ++ p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[SEC l]) p
  |verifArgs (s,n,[SEC l]) p = tope ([SEC l] ++ p) 
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[ADD]) p
  |verifArgs (s,n,[ADD]) p = tope (op ADD p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[SUB]) p
  |verifArgs (s,n,[SUB]) p = tope (op SUB p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[MUL]) p
  |verifArgs (s,n,[MUL]) p = tope (op MUL p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[DIV]) p
  |verifArgs (s,n,[DIV]) p = tope (op DIV p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[REM]) p
  |verifArgs (s,n,[REM]) p = tope (op REM p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[EQN]) p
  |verifArgs (s,n,[EQN]) p = tope (op EQN p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[LOT]) p
  |verifArgs (s,n,[LOT]) p = tope (op LOT p)
  |otherwise =  error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[GTN]) p
  |verifArgs (s,n,[GTN]) p = tope (op GTN p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[SEL]) p
  |verifArgs (s,n,[SEL]) p = tope (opPila SEL p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[POP]) p
  |verifArgs (s,n,[POP]) p = tope (opPila POP p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[NGET]) p
  |verifArgs (s,n,[NGET]) p = tope (opPila NGET p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,[SWAP]) p
  |verifArgs (s,n,[SWAP]) p = tope (opPila SWAP p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
--Casos recursivos
opaux (s,n,((L x):l)) p
  |verifArgs (s,n,((L x):l)) p = opaux (s,(n + 1),l) ((L x):p)
  |otherwise = error "Argumentos: Número de argumentos distintos al tamaño de la pila."
opaux (s,n,(ADD):l) p = opaux (s,(n-1),l) (op ADD p)
opaux (s,n,(SUB):l) p = opaux (s,(n-1),l) (op SUB p)
opaux (s,n,(MUL):l) p = opaux (s,(n-1),l) (op MUL p)
opaux (s,n,(DIV):l) p = opaux (s,(n-1),l) (op DIV p)
opaux (s,n,(REM):l) p = opaux (s,(n-1),l) (op REM p)
opaux (s,n,(EQN):l) p = opaux (s,(n-1),l) (op EQN p)
opaux (s,n,(LOT):l) p = opaux (s,(n-1),l) (op LOT p)
opaux (s,n,(GTN):l) p = opaux (s,(n-1),l) (op GTN p)
opaux (s,n,(SEL):l) p = opaux (s,(n-2),l) (opPila SEL p)
opaux (s,n,(POP):l) p = opaux (s,(n-1),l) (opPila POP p)
opaux (s,n,(NGET):l) p = opaux (s,(n+1),l) (opPila NGET p)
opaux (s,n,(SWAP):l) p = opaux (s,n,l) (opPila SWAP p)
opaux (s,n,(SEC l):ls) p = opaux (s,n + 1,ls) ((SEC l):p) 
opaux (s,n,(EXEC):ls) ((SEC l):p) = opaux (s,n - 1,(l++ls)) p

--Función opauxb: da el elemento tope de la pila.
tope :: Pila -> Comando
tope [L x] = L x
tope (l:ls) = l

--Función exec: manda a ejecutar el programa con opaux
exec :: Programa -> Pila -> Comando
exec (s,n,l) p = opaux (s,n,l) p

--Ejemplos de prueba:

--Programas que terminan con un valor entero.

--Resultado: L 3
programa0 = exec (Postfix,0,[L 2,L 4,EQN]) []
programa01 = exec (Postfix,0,[L 2,L 4,LOT]) []
programa02 = exec (Postfix,0,[L 2,L 4,GTN]) []
programa1 = exec (Postfix,0,[L (-1),L 2,ADD,L 3,MUL]) []
--Resultado: L (-14)
programa2 = exec (Postfix,3,[MUL,SWAP,L 2,MUL,SWAP,SUB]) [L 5,L 4,L 3]
--Resultado: L 14
programa3 = exec (Postfix,1,[SEC [L 2,MUL],EXEC]) [L 7]
--Resultado: L (-7)
programa4 = exec (Postfix,0,[SEC [L 0,SWAP,SUB],L 7,SWAP,EXEC]) []
--Resultado: L 12
programa5 = exec (Postfix,4,[LOT,SEC [ADD],SEC [MUL],SEL,EXEC]) [L 5,L 6,L 4,L 3]
programa51 = exec(Postfix,4,[SEL,EXEC]) [L 5,L 6,L 4,L 3]
--Resultado: L 12
programa6 = exec (Postfix,2,[L 2,NGET]) [L 9,L 12]

--Programas que terminan en error.

--Resultado: *** Exception: Error argumentos: Número de argumentos distintos al tamaño de la pila.
error1 = exec (Postfix,2,[SWAP]) [L 3]
--Resultado: *** Exception: Error argumentos: Número de argumentos distintos al tamaño de la pila.
error2 = exec (Postfix,1,[POP]) [L 4,L 5]
--Resultado: *** Exception: Error aritmético: Argumentos insuficientes.
error3 = exec (Postfix,1,[L 4,MUL,ADD]) [L 3]
--Resultado: *** Exception: Error NGET: Índice fuera de rango.
error4 = exec (Postfix,2,[L 3,NGET]) [L 7,L 8]
--Resultado: *** Exception: Error NGET: J-ésimo elemento no entero.
error5 = exec (Postfix,1,[SEC [L 2,MUL],L 1,NGET]) [L 3]
