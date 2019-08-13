ebinary i 
    | i == NegInf = "-inf"
    | i == PosInf = "inf"
	| i == 0 = "0"
    | i == 1 = "1" 
	| i == -1 = "-1"
    | mod i 2 == 0 = return ++ "0"
    | mod i 2 == 1 = return ++ "1"
	| i < 0 = return ++ "-"
    where return = ebinary (div i 2)