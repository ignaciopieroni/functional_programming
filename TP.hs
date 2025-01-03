import Utiles

--Punto 1
data Organizacion = Organizacion{
    nombre:: String, 
    web::String, 
    pais::String, 
    descripcion::String, 
    anio::Int, 
    industria::String, 
    empleados::Int
    } deriving (Show,Eq) --Le agregamos Eq para poder comparar organizaciones en el punto 10
    

--Modelo el tipo de dato Organizacion. Cada organizacion es un registro de tipo "Organizacion"
deTuplaDeOrganizacionesAOrganizaciones :: [TuplaDeOrganizacion] -> [Organizacion]
deTuplaDeOrganizacionesAOrganizaciones [] = []
deTuplaDeOrganizacionesAOrganizaciones orgs = map convertir orgs

convertir :: TuplaDeOrganizacion -> Organizacion
convertir (organization,id,name,website,country,description,founded,industry,number_of_employees) = Organizacion name website country description founded industry number_of_employees


--Punto 2
esmasVieja :: Organizacion -> Organizacion -> Organizacion
esmasVieja org1 org2
    | anio org1 < anio org2 = org1
    | otherwise = org2


organizacionMasVieja :: [Organizacion] -> Organizacion
organizacionMasVieja [] = error "Error: La lista no tiene ninguna organizacion"
organizacionMasVieja [org1] = org1 --Si hay una sola organizacion, entonces esta será la más vieja.
organizacionMasVieja(org1:orgs) = esmasVieja org1 (organizacionMasVieja orgs)

convertir :: TuplaDeOrganizacion -> Organizacion
convertir (organization,id,name,website,country,description,founded,industry,number_of_employees) = Organizacion name website country description founded industry number_of_employees

--Punto 3

sumarDosOrg :: Organizacion -> Organizacion -> Int
sumarDosOrg org1 org2 = empleados org1 + empleados org2

--Punto 4
totalEmpleados :: [Organizacion] -> Int
totalEmpleados [] = 0
totalEmpleados [org1] = empleados org1
totalEmpleados (org1:org2:orgs) = sumarDosOrg org1 org2 + totalEmpleados orgs


--Punto 5
totalEmpleadosAlternativa:: [Organizacion] -> Int
totalEmpleadosAlternativa = sum . map empleados

--Punto 6 
orgInicial :: Organizacion
orgInicial = Organizacion "" "" "" "" 0 "" 0

organizacionMasEmpleadosPlastics :: [Organizacion] -> Organizacion
organizacionMasEmpleadosPlastics = foldl masEmpleados orgInicial . filter funcionPlastic

funcionPlastic :: Organizacion -> Bool
funcionPlastic org = industria org == "Plastics" && anio org > 1960

masEmpleados :: Organizacion -> Organizacion -> Organizacion
masEmpleados org1 org2
    | empleados org1 > empleados org2 = org1
    | otherwise = org2


ampliarOrganizacion :: Organizacion -> Int -> Organizacion
ampliarOrganizacion org incremento = org { empleados = empleados org + incremento}


--Punto 8
ampliarOrganizaciones10 :: [Organizacion] -> [Organizacion]
ampliarOrganizaciones10 [] = []
ampliarOrganizaciones10 lista = map (\org -> ampliarOrganizacion org (floor (fromIntegral (empleados org) * 0.1))) lista


-- Punto 9
-- Organizaciones con una cantidad par de empleados.
empleadosPar:: Organizacion -> Bool
empleadosPar org = even (empleados org)
-- Organizaciones cuya primer palabra del nombre tenga más de una candidad dada de caracteres.
primerPalabra:: Organizacion -> Int -> Bool
primerPalabra org cantidad =  length (head (words (nombre org) )) > cantidad
-- Organizaciones fundadas despues del 2000 y cuya primera palabra del nombre sea la que se indica.
fundadaYpalabra :: Organizacion -> String -> Bool
fundadaYpalabra org palabra = anio org > 2000 && head (words (nombre org)) == palabra

--Funcion cumple que utiliza como parametro funciones sin parametro adicional
cumple :: (a -> Bool) -> [a] -> Bool
cumple f lista = length (filter(f) lista ) > (length lista `div` 2)+1
--Funcion cumple que utiliza como parametro funciones con parametro adicional
cumpleConParametro :: (a-> b -> Bool) -> [a] -> b -> Bool
cumpleConParametro f lista param= length (filter (\x -> f x param) lista) > (length lista `div` 2)+1


--Punto 10 
--Eliminar repetidos
repetido :: Eq a => [a] -> [a]
repetido [] = []
repetido (x:xs) = x : repetido (filter (/= x) xs)

--HayRepetidas compara el tamaño de la lista original con la lista sin repetidos (en el caso que hubiera)
hayRepetidas lista = length (lista) /= length (repetido lista)


