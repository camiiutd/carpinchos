module Solucion where

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)
type AgenciaDeViajes = [Vuelo]

{-
  NOMBRE DEL GRUPO: carpinchos_recursivos
  INTEGRANTES:
  NOMBRE: Camila Nohara Arakaki - DNI: 45.753.027 - MAIL: camila.nohara1@gmail.com
  NOMBRE: Dante Nataniel Eduardo Acosta  - DNI: 46.2673.34 - MAIL: davapeac@gmail.com
  NOMBRE: Juan Ignacio Bianchini - DNI: 44.7807.56 - MAIL: juanignaciobianchini@gmail.com
 . -}

-- EJERCICIO 1
vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos [] = True 
vuelosValidos ((salida,llegada,t):agencias) | not (vueloValido (salida,llegada,t))= False
                                            | salida==llegada = False
                                            | vuelosRepetidosIda salida agencias && vuelosRepetidosVuelta llegada agencias = False
                                            | not (conexionValida (salida,llegada,t)) = False
                                            | otherwise = vuelosValidos agencias

vueloValido :: Vuelo -> Bool
vueloValido (salida,llegada,t) = t > 0 && salida/=llegada

vuelosRepetidosIda :: Ciudad -> AgenciaDeViajes -> Bool
vuelosRepetidosIda _ [] = False
vuelosRepetidosIda vuelo ((salida,_,_):agencias) | vuelo == salida  = True
                                                       | otherwise= vuelosRepetidosIda vuelo agencias

vuelosRepetidosVuelta :: Ciudad -> AgenciaDeViajes -> Bool
vuelosRepetidosVuelta _ [] = False
vuelosRepetidosVuelta vuelo ((_,llegada,_):agencias) | vuelo == llegada = True
                                                     | otherwise= vuelosRepetidosVuelta vuelo agencias

conexionValida ::  Vuelo -> Bool
conexionValida (salida,llegada,t) | conexionIda (salida,llegada,t) /= conexionVuelta (salida,llegada,t) = False

conexionVuelta:: Vuelo -> Ciudad
conexionVuelta (salida,llegada,_) = salida

conexionIda :: Vuelo -> Ciudad
conexionIda (salida,llegada,_) = llegada


-- EJERCICIO 2

ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas [] _ = []
ciudadesConectadas agencia vuelos = eliminarRepetidos (conexionesTotales agencia vuelos)

conexionesTotales :: AgenciaDeViajes -> Ciudad -> [Ciudad]
conexionesTotales [] _ = []
conexionesTotales ((salida,llegada,t):agencias) vuelo | salida==vuelo= llegada :  conexionesTotales agencias vuelo
                                                      | llegada==vuelo = salida : conexionesTotales agencias vuelo
                                                      | otherwise= conexionesTotales agencias vuelo

soloCiudades :: AgenciaDeViajes -> [Ciudad]
soloCiudades [] = []
soloCiudades ((salida,llegada,t):agencias) = salida : llegada : soloCiudades agencias 

eliminoRepe :: Ciudad -> [Ciudad] -> [Ciudad]
eliminoRepe _ [] = []
eliminoRepe ciudad (c:cs) | ciudad==c= eliminoRepe ciudad cs
                          | otherwise= c : eliminoRepe ciudad cs

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
modernizarFlota ((salida,llegada,t):agencias) = (salida,llegada,(t-t*0.10)) : modernizarFlota agencias


-- EJERCICIO 4

ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada ((salida,llegada,t):agencias) | contadorDeVuelos salida (soloCiudades((salida,llegada,t):agencias) ) >= maximaCiudad agencias = salida
                                                 | otherwise= ciudadMasConectada agencias 

maximaCiudad :: AgenciaDeViajes -> Duracion
maximaCiudad [] = 0
maximaCiudad [(salida,llegada,t)] = t
maximaCiudad ((salida,llegada,t):agencias) | contadorDeVuelos salida (soloCiudades ((salida,llegada,t):agencias) ) >= maximaCiudad agencias = t
                                           | otherwise= maximaCiudad agencias

contadorDeVuelos :: Ciudad -> [Ciudad] -> Float
contadorDeVuelos _ [] = 0
contadorDeVuelos ciudad (c:cs) | ciudad == c = 1 + contadorDeVuelos ciudad cs
                               | otherwise= contadorDeVuelos ciudad cs


-- EJERCICIO 5

nonStopFlight :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
nonStopFlight [] _ _ = False
nonStopFlight ((x, y, z):xs) a b = (x == a && y == b) || nonStopFlight xs a b

getAvailableDestinations :: AgenciaDeViajes -> Ciudad -> [Ciudad]
getAvailableDestinations [] _ = []
getAvailableDestinations ((x, y, z):xs) city
    | x == city = [y] ++ getAvailableDestinations xs city
    | otherwise = getAvailableDestinations xs city

itemIn :: (Eq t) => t -> [t] -> Bool
itemIn _ [] = False
itemIn i (x:xs) = (i == x) || itemIn i xs

lookForDestination :: Ciudad -> [Ciudad] -> AgenciaDeViajes -> Bool
lookForDestination _ [] _ = False
lookForDestination dst (x:xs) ag = (itemIn dst (getAvailableDestinations ag x)) || (lookForDestination dst xs ag)

sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar ag dep dst = (nonStopFlight ag dep dst) || (lookForDestination dst (getAvailableDestinations ag dep) ag)


-- EJERCICIO 6

getFasterNonStop :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
getFasterNonStop [] _ _ = 0
getFasterNonStop ((x, y, z):xs) dep dst
    | (dep == x) && (dst == y) = z
    | otherwise = getFasterNonStop xs dep dst

getOneStopDuration :: AgenciaDeViajes -> Ciudad -> Ciudad -> [Ciudad] -> Duracion
getOneStopDuration _ _ _ [] = 0
getOneStopDuration ag dep dst (x:xs)
    | itemIn dst (getAvailableDestinations ag x) = (getFasterNonStop ag dep x) + (getFasterNonStop ag x dst)
    | otherwise = getOneStopDuration ag dep dst xs

lenAnalyzer :: Duracion -> Duracion -> Duracion
lenAnalyzer ns os
    | (ns == 0) || (os == 0) = ns + os
    | ns < os = ns
    | otherwise = os

duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido ag dep dst = lenAnalyzer (getOneStopDuration ag dep dst (getAvailableDestinations ag dep)) (getFasterNonStop ag dep dst)


-- EJERCICIO 7

pathFinder :: AgenciaDeViajes -> [Ciudad] -> Ciudad -> Bool
pathFinder _ [] _ = False
pathFinder ag (x:xs) dst
    | itemIn dst ([x]++xs) = True
    | otherwise = (pathFinder ag (getAvailableDestinations ag x) dst) || pathFinder ag xs dst 

puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedoVolverAOrigen vuelos origen = pathFinder vuelos (getAvailableDestinations vuelos origen) origen