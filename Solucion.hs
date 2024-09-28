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
                           | vuelosRepetidos a xs || vuelosRepetidos b xs = False
                           | otherwise = vuelosValidos xs

vueloValido :: Vuelo -> Bool
vueloValido (a,b,c) = c > 0 && a/=b

vuelosRepetidos :: Ciudad -> AgenciaDeViajes -> Bool
vuelosRepetidos _ [] = False
vuelosRepetidos v ((a,b,c):xs) | v == a || v == b = True
                               | otherwise= vuelosRepetidos v xs

-- EJERCICIO 2
ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas [] _ = []
ciudadesConectadas ((a,b,c):xs) v | v == a = b : ciudadesConectadas xs v
                                  | v == b = a : ciudadesConectadas xs v
                                  | otherwise= ciudadesConectadas xs v


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

soloCiudades :: AgenciaDeViajes -> [Ciudad]
soloCiudades [] = []
soloCiudades ((a,b,c):xs) = a : b : soloCiudades xs 

contadorDeVuelos :: Ciudad -> [Ciudad] -> Float
contadorDeVuelos _ [] = 0
contadorDeVuelos n (x:xs) | n == x = 1 + contadorDeVuelos n xs
                          | otherwise= contadorDeVuelos n xs

-- EJERCICIO 5
sePuedeLlegar :: AgenciaDeViajes -> Ciudad -> Ciudad -> Bool
sePuedeLlegar vuelos origen destino = True -- Borrar y escribir el código correcto


-- EJERCICIO 6
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido _ _ _ = 10.0 -- Borrar y escribir el código correcto



-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedoVolverAOrigen vuelos origen = True -- Borrar y escribir el código correcto