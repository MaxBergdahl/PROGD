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