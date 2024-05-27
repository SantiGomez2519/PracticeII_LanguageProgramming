import Data.List --La usamos para conocer la longitud del número

-- Función para calcular la suma alícuota de un número
--Crea una lista con todos los factores de n (sin contar a n) y los suma
aliquotSum :: Int -> Int
aliquotSum n = sum [x | x <- [1..n-1], n `mod` x == 0]

-- Función para clasificar el número según su suma alícuota
classifyNumber :: Int -> String
classifyNumber n
    | aliquotSum n == n = "Engineering"
    | aliquotSum n > n  = "Administrative"
    | otherwise         = "Humanities"

-- Función para extraer el período de admisión
getAdmissionPeriod :: Int -> String
getAdmissionPeriod n = case div n 100000 of
    241 -> "2024-1"
    242 -> "2024-2"
    251 -> "2025-1"
    252 -> "2025-2"
    261 -> "2026-1"
    262 -> "2026-2"
    _   -> error "Invalid admission period"


-- Función para obtener la categoría del programa académico
getCategory :: Int -> String
getCategory n = classifyNumber (div n 1000 `mod` 100) --Primero elimina del número los últimos tres números y después los primeros tres


-- Función para obtener el número consecutivo de admisión
getConsecutiveNumber :: Int -> String
getConsecutiveNumber n = show (n `mod` 1000) --Devuelve los últimos tres números 

-- Función para determinar si el número es par o impar
isEvenOrOdd :: Int -> String
isEvenOrOdd n
    | even n = "even"
    | otherwise = "odd"

-- Función principal para procesar el código de estudiante
processStudentID :: Int -> String
processStudentID id
    | length (show id) /= 8 = "Invalid ID."
    | otherwise = admissionPeriod ++ " " ++ category ++ " num" ++ consecutiveNumber ++ " " ++ evenOrOdd
  where
    admissionPeriod = getAdmissionPeriod id
    category = getCategory id
    consecutiveNumber = getConsecutiveNumber id
    evenOrOdd = isEvenOrOdd id

main :: IO ()
main = do
    id <- readLn :: IO Int
    putStrLn $ processStudentID id