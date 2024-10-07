module Solucion where

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]

{-
  NOMBRE DEL GRUPO: carpinchos_recursivos
  INTEGRANTES:
  NOMBRE: Camila Nohara Arakaki - DNI: 45.753.027 - MAIL: camila.nohara1@gmail.com
  NOMBRE: - DNI: - MAIL:
  NOMBRE: - DNI: - MAIL:
 . -}

-- EJERCICIO 1
vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True 
vuelosValidos ((a,b,c):xs) | not (vueloValido (a,b,c))= False
                           | a==b = False
                           | vuelosRepetidosIda a xs && vuelosRepetidosVuelta b xs = False
                           | otherwise = vuelosValidos xs

vueloValido :: Vuelo -> Bool
vueloValido (a,b,c) = c > 0 && a/=b

vuelosRepetidosIda :: Ciudad -> AgenciaDeViajes -> Bool
vuelosRepetidosIda _ [] = False
vuelosRepetidosIda v ((a,b,c):xs) | v == a  = True
                                  | otherwise= vuelosRepetidosIda v xs

vuelosRepetidosVuelta :: Ciudad -> AgenciaDeViajes -> Bool
vuelosRepetidosVuelta _ [] = False
vuelosRepetidosVuelta v ((a,b,c):xs) | v == b = True
                                     | otherwise= vuelosRepetidosVuelta v xs

-- EJERCICIO 2
ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas [] _ = []
ciudadesConectadas agencia v = eliminarRepetidos (conexionesTotales agencia v)

conexionesTotales :: AgenciaDeViajes -> Ciudad -> [Ciudad]
conexionesTotales [] _ = []
conexionesTotales ((a,b,c):xs) v | a==v= b:  conexionesTotales xs v
                                 | b==v= a:conexionesTotales xs v
                                 | otherwise= conexionesTotales xs v

soloCiudades :: AgenciaDeViajes -> [Ciudad]
soloCiudades [] = []
soloCiudades ((a,b,c):xs) = a : b : soloCiudades xs 

eliminoRepe :: Ciudad -> [Ciudad] -> [Ciudad]
eliminoRepe _ [] = []
eliminoRepe c (x:xs) |c==x= eliminoRepe c xs
                     | otherwise= x : eliminoRepe c xs

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos m (x:xs) | m/=x = x : quitarTodos m xs
                     | otherwise= quitarTodos m xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) | pertenece x xs = x: eliminarRepetidos( quitarTodos x xs )
                         | otherwise= x : eliminarRepetidos xs      

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False  
pertenece n (x:xs) | n ==x = True
                   | otherwise= pertenece n xs


-- EJERCICIO 3
modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota ((a,b,c):xs) = (a,b,(c-c*0.10)) : modernizarFlota xs 


-- EJERCICIO 4
ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada ((a,b,c):xs) | contadorDeVuelos a (soloCiudades((a,b,c):xs) ) >= maximaCiudad xs = a
                                | otherwise= ciudadMasConectada xs 

maximaCiudad :: AgenciaDeViajes -> Duracion
maximaCiudad [] = 0
maximaCiudad [(a,b,c)] = c
maximaCiudad ((a,b,c):xs) | contadorDeVuelos a (soloCiudades((a,b,c):xs) ) >= maximaCiudad xs = c
                          | otherwise= maximaCiudad xs

contadorDeVuelos :: Ciudad -> [Ciudad] -> Float
contadorDeVuelos _ [] = 0
contadorDeVuelos n (x:xs) | n == x = 1 + contadorDeVuelos n xs
                          | otherwise= contadorDeVuelos n xs

-- EJERCICIO 5 preguntar si está para ser así
--sePuedeLlegar [("A","S",2.1),("S","E",6.1),("E","A",5.0)] "A" "E"

sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar [] _ _ =False                                                        --Caso Base
sePuedeLlegar a ori des | buscoVuelos a ori des =True                              --Primero me fijo si hay un vuelo directo y devuelvo true
                        | buscoEscalas a ori des = True                            --Busco si hay por lo menos una escala entre medio aplicando recursión tomando buscoVuelos siendo ida=escala y vuelta=destino
                        | otherwise=False                                          --Si no cumple, devuelvo False

buscoVuelos:: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
buscoVuelos [] _ _ = False                                                         --Caso base
buscoVuelos ((a,b,c):xs) ida vuelta | ida == a && vuelta ==b = True                --Busco vuelo directo (misma dupla)
                                    | otherwise= buscoVuelos xs ida vuelta         --Paso recursivo

buscoEscalas :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
buscoEscalas [] _ _ =False                                                         --Caso base
buscoEscalas ((a,b,c):xs) escala vuelta | a == escala  = buscoVuelos xs b vuelta   --Busco la escala
                                        | otherwise= buscoEscalas xs escala vuelta --Paso recursivo


-- EJERCICIO 6
-- duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
-- duracionDelCaminoMasRapido [] _ _ = 0.0 
-- duracionDelCaminoMasRapido ((a,b,c):xs) ida vuelta 

-- duraciones :: AgenciaDeViajes -> Ciudad -> Ciudad -> [Duracion] 
-- duraciones [] _ _ = []
-- duraciones ((a,b,c):xs) ida vuelta | buscoVuelos ((a,b,c):xs) ida vuelta = c : duraciones xs ida vuelta   --Caso vuelo directo: devuelvo c
--                                    | buscoEscalas ((a,b,c):xs) ida vuelta =

-- devuelvoDuracionIda :: AgenciaDeViajes -> Ciudad -> Duracion
-- devuelvoDuracionIda [] _ _ = 0
-- devuelvoDuracionIda ((a,b,c):xs) ida  |  ida == a = c 
--                                       | otherwise= devuelvoDuracionIda xs ida 
                                            
-- devuelvoDuracionVuelta :: AgenciaDeViajes -> Ciudad -> Duracion
-- devuelvoDuracionVuelta [] _ _ = 0
-- devuelvoDuracionVuelta ((a,b,c):xs) vuelta | vuelta ==b = c 
--                                            | otherwise= devuelvoDuracionVuelta xs vuelta

-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedoVolverAOrigen vuelos origen = True -- Borrar y escribir el código correcto
