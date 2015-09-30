-- Max Bergdahl och Niklas Bergdahl 30/9-15 --
module F2 where

data MolSeq = DNA (String, String) | PROT (String, String)

--string2seq :: String -> String -> MolSeq
--string2seq n (s:b)
--	| not(elem s "ACGT") = PROT (n, s:b)
--	| elem s "ACGT" = DNA (n, s:string2seq n b)

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