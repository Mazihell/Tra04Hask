module Main where
import Data.Char ( ord )

--1. Escreva uma função chamada fatorialn que usando o operador range e a função foldr devolva o fatorial de n.
fatorialn :: Int -> Int
fatorialn 0 = 1
fatorialn x = foldr (*) 1 [1,2..x]

--2. Usando a função map escreva uma função, chamada quadradoReal 
--que recebe uma lista de números reais, positivos e negativos e 
--devolva uma lista com o quadrado de cada um dos reais listados.
quadradoReal :: [Float] -> [Float]
quadradoReal x = map (^2) x

--3. Usando a função map escreva uma função, 
--comprimentoPalavras que recebe uma lista de palavras 
--e devolve uma lista com o comprimento de cada uma destas palavras.
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras p = map length p

--4. Usando a função filter escreva uma função, 
--chamada maiorMultiploDe29 devolva o maior 
--número entre 0 e 100000 que seja divisivel por 29. 
--(eu não entendi como usar filter, e acabei usando outro metodo para chegar no resultado)
divisor :: Int -> [Int]
divisor p = [x | x <- [1,2..p], x `mod` 29 == 0]
maiorMultiploDe29 :: Int -> Int
maiorMultiploDe29 x = maximum(divisor x)

--5. Usando a função filter escreva uma função, chamada maiorMultiploDe 
--que recebe um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro.
--(eu não entendi como usar filter, e acabei usando outro metodo para chegar no resultado)
divisorM :: Int -> [Int]
divisorM p = [x | x <- [1,2..p], x `mod` p == 0]
maiorMultiploDe :: Int -> Int
maiorMultiploDe x = maximum(divisor x)

--6.Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos 
--quadrados dos itens de uma lista de números naturais de 
--comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=12+22+32+42...+𝑛2.
somaQuadrados :: Int -> Int
somaQuadrados x = foldr (+) 0 (map (^2) [1..x])

--8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell 
--do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. 
--Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.


main :: IO ()
main = do


putStrLn $ ("Func.1: entrada 5 resultado " ++ show(fatorialn 5))
putStrLn $ ("Func.2: entrada -3.3;-2.5;2.6;9.7 resultado " ++ show(quadradoReal [-3.3,-2.5,2.6,9.7]))
putStrLn $ ("Func.3: entrada 'por Deus';'pela Patria';'pela Familia';'pela Liberdade' resultado "  ++ show(comprimentoPalavras ["por Deus","pela Patria","pela Familia","pela Liberdade"]))
putStrLn $ ("Func.4: entrada 100000 resultado " ++ show(maiorMultiploDe29 100000))
putStrLn $ ("Func.5: entrada 100000 resultado " ++ show(maiorMultiploDe 100000))
putStrLn $ ("Func.6: entrada 5 resultado " ++ show(somaQuadrados 5))
putStrLn $ ("Func.8.1: entrada max 4;5 resultado " ++ show(max 4 5))
putStrLn $ ("Func.8.2: entrada max 9;2 resultado " ++ show(max 9 2))
putStrLn $ ("Func.8.1: entrada min 4;5 resultado " ++ show(min 4 5))
putStrLn $ ("Func.8.2: entrada min 17;9 resultado " ++ show(min 17 9))
putStrLn $ ("Func.8.1: entrada flip zip [1,2,3,4]; 'abcd' resultado " ++ show(flip zip  [1,2,3,4] "abcd"))
putStrLn $ ("Func.8.2: entrada flip zip [4,3,2,1]; 'abcd' resultado " ++ show(flip zip  [4,3,2,1] "dcba"))
putStrLn $ ("Func.8.1: entrada ord 'a' resultado " ++ show(ord 'a'))
putStrLn $ ("Func.8.2: entrada ord 'b' resultado " ++ show(ord 'b'))
