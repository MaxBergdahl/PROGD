-- Max Bergdahl och Niklas Bergdahl 14/9-15 --
module F1 where
import Data.Char

-- Vad ska de andra funktionernas typsignaturer vara?
-- fib skriven av Max Bergdahl
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = if n > 1
	then fib (n-1) + fib (n-2)
	else n

--rovarsprak skriven av Max Bergdahl
rovarsprak :: String -> String
rovarsprak (s:b)
	| not(elem s "aeiouy") = s:'o':s : rovarsprak b
	| otherwise = s:rovarsprak b
rovarsprak s = s
-- a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z.

--karpsravor skriven av Max Bergdahl
karpsravor :: String -> String
karpsravor (s:b)
	| not(elem s "aeiouy") = s:karpsravor(drop 2 b)
	| otherwise = s:karpsravor b
karpsravor s = s

--medellangdHelp skriven av Max Bergdahl och Niklas Bergdahl
medellangdHelp :: String -> String
medellangdHelp s = [if not (isAlpha y) then ' ' else y | y<-s]

--medellangd skriven av Max Bergdahl och Niklas Bergdahl
medellangd :: String -> Double
medellangd s = fromIntegral(sum (map length (words (medellangdHelp s)))) / fromIntegral(length (words (medellangdHelp s)))
--medellangd s = fromIntegral((length (unwords(words(medellangdHelp s)))+1) - (length (words (medellangdHelp s)))) / fromIntegral(length (words (medellangdHelp s)))


--skyfflaHalf skriven av Max Bergdahl och Niklas Bergdahl
skyfflaHalf xs = case drop (1) xs of
	(y:ys) -> y : skyfflaHalf ys
	[] -> []

--skyffla skriven av Max Bergdahl och Niklas Bergdahl
skyffla s
	| length s >= 2 = [x | x <- skyfflaHalf (head (s):s)] ++ skyffla(skyfflaHalf s)
	| otherwise = s