let rec fib n =
if n <= 1 then n
else fib (n-1) + fib (n-2);;

let rec print_fib n =
if n = 0 then "0"
else print_fib (n-1) ^ "\n" ^ string_of_int(fib n);;

let x = int_of_string Sys.argv.(1);;

if (Array.length Sys.argv) = 2  then 
print_endline (print_fib x)
else print_endline ("fib: numero de argumentos invalido");;
