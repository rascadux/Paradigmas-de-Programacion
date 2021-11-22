(*FUNCION ES PRIMO? DADA*)
let is_prime n =
	let rec check_from i =
		i >= n ||
		(n mod i <> 0 && check_from (i+1))
	in check_from 2;;
	

let rec next_prime n = 
	if is_prime n then n else next_prime (n+1);;

let last_prime_to n = 
	if is_prime n then n else next_prime (n-1);;
	
let is_prime2 n =
	let rec check_from i =
		float_of_int(i) >= sqrt(float_of_int(n)) || (n mod i<> 0 && check_from (i+1))
	in check_from 2;;
	
