module Utiles (
    leerTuplasDeOrganizaciones,
    split,
    TuplaDeOrganizacion,
    parsearOrganizacion
) where
import GHC.IO ( unsafePerformIO )

-- campos en orden:
-- Index,Organization Id,Name,Website,Country,Description,Founded,Industry,Number of employees
type TuplaDeOrganizacion = (Int, String, String, String, String, String, Int, String, Int)

-- Dado un string (el contenido de un archivo csv), lo parsea y devuelve
-- una lista con todas las tuplas de organizaciones que contenga.
leerTuplasDeOrganizaciones :: String -> [TuplaDeOrganizacion]
leerTuplasDeOrganizaciones archivo = map parsearOrganizacion filasDeOrganizaciones
    where
        -- paso los contenidos del archivo a un string
        contenidoDelArchivo = leerArchivo archivo
        -- divido el string del contenido por linea, asi puedo trabajar
        -- con cada una individualmente
        filasDelArchivo = lines contenidoDelArchivo
        -- descarto la primer linea, ya que es la cabecera del archivo (no es un dato)
        filasDeOrganizaciones = tail filasDelArchivo

-- Dada una fila del CSV de organizaciones (en formato string), 
-- lo parsea y devuelve una tupla con los diferentes componentes
parsearOrganizacion :: String -> TuplaDeOrganizacion
parsearOrganizacion string = (index, id, nombre, website, pais, desc, fundada, industria, nroEmpleados)
    where
        -- divido el string en multiples lineas, usando las comas como separador.
        camposComoString = split ',' string
        -- leo cada campo, usando la funcion read para pasar de String a numeros
        -- cuando hace falta
        index = (read . head) camposComoString
        id = camposComoString !! 1
        nombre = camposComoString !! 2
        website = camposComoString !! 3
        pais = camposComoString !! 4
        desc = camposComoString !! 5
        fundada = (read . (!! 6)) camposComoString
        industria = camposComoString !! 7
        nroEmpleados = (read . (!! 8)) camposComoString

-- Dado un separador (un caracter) y un string, devuelve la lista de 
-- strings que resulta de dividir el string en partes teniendo en cuenta
-- el separador.
-- Ejemplo:
-- split ',' "hola, como estas?"
-- devuelve: ["hola", "como estas?"]
split :: Char -> String -> [String]
split separador [] = [""]
split separador (c:cs) 
    | c == separador = "" : resto
    | otherwise = (c : head resto) : tail resto
    where resto = split separador cs

-- Lee y devuelve los contenidos de un archivo como string.
leerArchivo :: String -> String
leerArchivo archivo = unsafePerformIO (readFile archivo)