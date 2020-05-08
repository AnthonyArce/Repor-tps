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
auto1 = Auto "AT001LN" [0, 1, 0, 0] 1500 40 (10,20,2019)

auto2:: Auto
auto2 = Auto "DJV214" [0, 1, 0, 0] 1500 40 (10,20,2019)

costoArreglo::Auto -> Double
costoArreglo (Auto patente _ _ _ _ ) | ((>6).length ) patente = 12500
                                     | otherwise = 10000