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
    desgasteLlantas = [0.51, 1, 0, 0],
    rpm = 1500,
    temperaturaAgua = 40,
    ultimoArreglo = (10,20,2019)
    }

auto2:: Auto
auto2 = Auto "DJV214" [0, 1, 0, 0] 1500 40 (10,20,2015)

auto3:: Auto
auto3 = Auto "DJV216" [0.5, 1, 0, 0] 3000 40 (10,20,2016)

auto4:: Auto
auto4 = Auto "DJV216" [0.4, 1, 0, 0] 1500 40 (10,20,2019)


primerasLetras::String->String
primerasLetras patente = take 2 patente -- funcion take ("cantidad de letras a tomar") (String)

costoArreglo::Auto -> Int
costoArreglo (Auto patente _ _ _ _ ) | ((==6).length ) patente = 12500
                                     | (primerasLetras patente == "DJ") && (last patente == '4') = length patente * 3000
                                     | (primerasLetras patente == "DJ") && (last patente /= '4') = 20000
                                     | otherwise = 15000


autoPeligroso::Auto->Bool
autoPeligroso (Auto _ desgasteLlantas _ _ _ ) | ((>=0.5) . head ) desgasteLlantas = True
                                              | otherwise = False


-- necesitaRevision::Auto->Bool
-- necesitaRevision (Auto _ _ _ _ ( _, _, year) ) | year == 2016 = False
--                                                | year == 2015 = True



alfa::Auto->Int
alfa (Auto _ _ rpm _ _) | rpm < 2000 = rpm
                        | otherwise = 2000

bravo::Auto->[Desgaste]         
bravo (Auto _ desgasteLlantas _ _ _ ) =  [0 ,0 ,0 ,0 ]       

-- 

tomarElemento::(Int,Int) -> Int
tomarElemento ( _ ,b ) = b
-- zulu::Auto -> (Int,[Desgaste])
-- zulu (Auto _ desgasteLlantas rpm _ _) = ( (alfa (Auto _ _ rpm _ _)),(bravo (Auto _ desgasteLlantas _ _ _) ) )
