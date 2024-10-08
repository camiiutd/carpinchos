module Solucion where

type Ciudad = String
type Duracion = Float
type Vuelo = (Ciudad, Ciudad, Duracion)

type AgenciaDeViajes = [Vuelo]

-- EJERCICIO 1

vueloValido :: Vuelo -> Bool
vueloValido (x, y, z) = x /= y && z > 0

getDep :: Vuelo -> Ciudad
getDep (x, y, z) = x

getDst :: Vuelo -> Ciudad
getDst (x, y, z) = y

isFlightHere :: Ciudad -> Ciudad -> AgenciaDeViajes -> Bool
isFlightHere _ _ [] = False
isFlightHere dep dst (x:xs)
    | (getDep x) == dep && (getDst x) == dst = True
    | otherwise = isFlightHere dep dst xs

checkFlightsInvalid:: AgenciaDeViajes -> Bool
checkFlightsInvalid [] = False
checkFlightsInvalid (x:xs) = (isFlightHere (getDep x) (getDst x) xs) || not (vueloValido x) || checkFlightsInvalid xs

vuelosValidos :: AgenciaDeViajes -> Bool
vuelosValidos xs = not (checkFlightsInvalid xs)

-- EJERCICIO 2

remAll :: (Eq t) => t -> [t] -> [t]
remAll _ [] = []
remAll y (x:xs)
    | x == y = remAll y xs
    | otherwise = [x] ++ remAll y xs

remRep :: (Eq t) => [t] -> [t]
remRep [] = []
remRep [x] = [x]
remRep (x:xs) = [x] ++ remRep (remAll x xs)

getRelatedCities :: AgenciaDeViajes -> Ciudad -> [Ciudad]
getRelatedCities [] _ = []
getRelatedCities (x:xs) city
    | getDep x == city = [getDst x] ++ getRelatedCities xs city
    | getDst x == city = [getDep x] ++ getRelatedCities xs city
    | otherwise = getRelatedCities xs city



ciudadesConectadas :: AgenciaDeViajes -> Ciudad -> [Ciudad]
ciudadesConectadas av c = remRep (getRelatedCities av c)

-- EJERCICIO 3

modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota ((x,y,z):xs) = [(x, y, z * 0.9)] ++ modernizarFlota xs


-- EJERCICIO 4

len :: [t] -> Integer
len [] = 0
len (x:xs) = 1 + len xs

getAllCities_ :: AgenciaDeViajes -> [Ciudad]
getAllCities_ [] = []
getAllCities_ ((x, y, z):xs) = [x] ++ [y] ++ getAllCities_ xs

getAllCities :: AgenciaDeViajes -> [Ciudad]
getAllCities cl = remRep (getAllCities_ cl)

getMostConnected :: AgenciaDeViajes -> [Ciudad] -> Ciudad -> Ciudad
getMostConnected _ [] x = x
getMostConnected ag (x:xs) city
    | city == "" = getMostConnected ag xs x
    | len (ciudadesConectadas ag x) > len (ciudadesConectadas ag city) = getMostConnected ag xs x
    | otherwise = getMostConnected ag xs city

ciudadMasConectada :: AgenciaDeViajes -> Ciudad
ciudadMasConectada ag = getMostConnected ag (getAllCities ag) ""


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