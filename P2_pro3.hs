--Problema 8
{-
Funcion recursiva que produce como salida un numero entero de dígitos repetidos, la funcion recibirá 2 parámetros de entrada: las veces que se repetirá el digito
(mal1) y el digito que se repetirá (mal2). Ej  fjmlNcopias 3 2 -> 222; fjmlNcopias 5 7 -> 77777
-}

cuentadig::Int->Int
cuentadig n | n<10=1
            | otherwise = 1 + cuentadig (div n 10)

fjmlNcopias :: Integer -> Integer -> Integer
fjmlNcopias mal1 mal2  | (mal1 == 0) = 0
                             | (mal1 == 1) = mal2
                             | (mal1 > 1)  = mal2 * (10^(mal1-1)) + fjmlNcopias (mal1-1) (mal2)               