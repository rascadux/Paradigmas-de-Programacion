let rec fact x =
	if x <= 1 then 1 else x * fact (x - 1)

let x = int_of_string Sys.argv.(1);;

if Array.length Sys.argv = 2 then Printf.printf"%d" (fact x)  
else Printf.printf"fact: numero de argumentos invalido";;

print_newline();;


 

