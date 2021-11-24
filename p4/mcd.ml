let rec mcd (x,y) =
	if      x = 0 then y
	else if y = 0 then x
	else if x > y then mcd (y, (x mod y))
	else mcd (x, (y mod x))
	;;

