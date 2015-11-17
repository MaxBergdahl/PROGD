-- Max Bergdahl och Niklas Bergdahl 30/9-15 --
module F2 where

data MolSeq = DNA (String, String) | PROT (String, String) deriving Show
--	String (Show)

-- Skriven av Max Bergdahl och Niklas Bergdahl
string2seqHelp :: String -> String -> Bool
string2seqHelp n (s:b)
	| not(elem s "ACGT") = False
	| elem s "ACGT" = string2seqHelp n b
string2seqHelp n s = True

-- Skriven av Max Bergdahl och Niklas Bergdahl
string2seq :: String -> String -> MolSeq
string2seq n s
	| string2seqHelp n s = DNA (n, s)
	| otherwise = PROT (n, s)

-- Skriven av Max Bergdahl
seqName :: MolSeq -> String
seqName (DNA (n, s)) = n
seqName (PROT (n, s)) = n

-- Skriven av Max Bergdahl
seqSequence :: MolSeq -> String
seqSequence (DNA (n, s)) = s
seqSequence (PROT (n, s)) = s

-- Skriven av Max Bergdahl
seqLength :: MolSeq -> Int
seqLength (DNA (n, (s:b))) = 1 + seqLength (DNA (n, b))
seqLength (DNA (n, s)) = 0
seqLength (PROT (n, (s:b))) = 1 + seqLength (PROT (n, b))
seqLength (PROT (n, s)) = 0

seqDifference :: String -> String -> Int
seqDifference (s:d) (z:x)
	| s /= z = 1 + seqDifference d x
	| otherwise = 0 + seqDifference d x
seqDifference s z
	| s /= z = 1
	| otherwise = 0

difRatio :: MolSeq -> MolSeq -> Double
difRatio (DNA(n, s)) (DNA(m, z)) = fromIntegral(seqDifference s z) / fromIntegral(seqLength (DNA(n, s)))
difRatio (PROT(n, s)) (PROT(m, z)) = fromIntegral(seqDifference s z) / fromIntegral(seqLength (PROT(n, s)))

protDifRatio :: MolSeq -> MolSeq -> Double
protDifRatio (PROT(n, s)) (PROT(m, z)) = fromIntegral(seqDifference s z) / fromIntegral(seqLength (PROT(n, s)))

-- log == natural logarithm
-- todo error message (using function error) if dna & prot comparison
seqDistance :: MolSeq -> MolSeq -> Double
seqDistance (DNA(n, s)) (DNA(m, z))
	| difRatio (DNA(n, s)) (DNA(m, z)) <= 0.74 = ((-3/4) * log(1-4*((difRatio (DNA(n, s)) (DNA(m, z)))/3)))
	| otherwise = 3.3
seqDistance (PROT(n, s)) (PROT(m, z))
	| difRatio (PROT(n, s)) (PROT(m, z)) <= 0.94 = ((-19/20) * log(1-20*((difRatio (PROT(n, s)) (PROT(m, z)))/19)))
	| otherwise = 3.7
seqDistance (DNA(n, s)) (PROT(m, z)) = error "Can't compare a DNA string with a Protein string"
seqDistance (PROT(n, s)) (DNA(m, z)) = error "Can't compare a Protein string with a DNA string"

--seqDistnace PROT (n, s) PROT (m, z) = 