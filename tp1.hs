type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year

data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo :: Fecha
} deriving Show

auto1::Auto
auto1 = Auto {
    patente = "AT001LN",
    desgasteLlantas = [0, 1, 0, 0],
    rpm = 1500,
    temperaturaAgua = 40,
    ultimoArreglo = (10,20,2019)
    }

auto2:: Auto
auto2 = Auto "DJV214" [0, 1, 0, 0] 1500 40 (10,20,2019)

auto3:: Auto
auto3 = Auto "DJV216" [0, 1, 0, 0] 1500 40 (10,20,2019)



primerasLetras::String->String
primerasLetras patente = take 2 patente -- funcion take ("cantidad de letras a tomar") (String)

costoArreglo::Auto -> Int
costoArreglo (Auto patente _ _ _ _ ) | ((==6).length ) patente = 12500
                                     | (primerasLetras patente == "DJ") && (last patente == '4') = length patente * 3000
                                     | (primerasLetras patente == "DJ") && (last patente /= '4') = 20000
                                     | otherwise = 15000


