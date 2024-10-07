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

{- problema ciudadesConectadas (agencia: AgenciaDeViajes, ciudad: Ciudad) : seq⟨Ciudad⟩ {
    requiere: {vuelosValidos(agencia)}
    asegura: {res no tiene elementos repetidos}
    asegura: {res contiene todas las ciudades a las que se puede llegar con un vuelo desde ciudad teniendo en cuenta los
    vuelos ofrecidos por agencia}
    asegura: {res contiene todas las ciudades a las que se puede llegar con un vuelo hacia ciudad teniendo en cuenta los
    vuelos ofrecidos por agencia}
    asegura: {res no contiene ninguna ciudad que no est´e conectada mediante vuelos directos con ciudad, teniendo en
    cuenta los vuelos ofrecidos por agencia}
} -}

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

{- problema modernizarFlota (agencia: AgenciaDeViajes) : AgenciaDeViajes {
    requiere: {vuelosValidos(agencia)}
    asegura: {res contiene la misma cantidad de vuelos que agencia}
    asegura: {Si dos ciudades c1 y c2 estaban conectadas mediante un vuelo en agencia, entonces res contiene un vuelo
    conectando esas dos ciudades (manteniendo cu´al es el origen y cu´al el destino). Adem´as, en res, ese vuelo demora un
    10 % menos del tiempo que demoraba el vuelo entre esas ciudades en agencia}
} -}

modernizarFlota :: AgenciaDeViajes -> AgenciaDeViajes
modernizarFlota [] = []
modernizarFlota ((x,y,z):xs) = [(x, y, z * 0.9)] ++ modernizarFlota xs


-- EJERCICIO 4

{- problema ciudadMasConectada (agencia: AgenciaDeViajes) : Ciudad {
    requiere: {vuelosValidos(agencia)}
    requiere: {|agencia| > 0}
    asegura: {res es alguna de las ciudades con m´as conexiones teniendo en cuenta los vuelos ofrecidos por agencia}
} -}

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
duracionDelCaminoMasRapido :: AgenciaDeViajes -> Ciudad -> Ciudad -> Duracion
duracionDelCaminoMasRapido _ _ _ = True



-- EJERCICIO 7
puedoVolverAOrigen :: AgenciaDeViajes -> Ciudad ->  Bool
puedoVolverAOrigen vuelos origen = True -- Borrar y escribir el código correcto