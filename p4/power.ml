let rec power x y=
	if y != 0 then x * power x (y-1) else 1
	
let rec power' x y=
	if y mod 2 = 0 then (power x (y/2)) * (power x (y/2)) 
	else x * (power x (y/2)) * (power x (y/2))
	
let rec powerf x n=
	if n != 0 then x *. powerf x (n-1) else 1.0
	
	
	
let x = float_of_string Sys.argv.(1);;
let y = int_of_string Sys.argv.(2);;

if Array.length Sys.argv = 3 then Printf.printf"%f" (powerf x y)  
else Printf.printf"power: numero de argumentos invalido";;

print_newline();;
