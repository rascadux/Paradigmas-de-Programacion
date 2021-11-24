let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec orbit n =
        if n=1 then print_endline"1"
        else (print_int n ; print_string", " ; orbit(f n))
                ;;

let rec length n =
        if n = 1 then 0
        else length(f n) + 1
        ;;

let rec top n = 
        if n=1 then 1 
        else max n (top(f n))
        ;;
	
	
let rec length'n'top n = 
        let rec aux (i,j)=
        if i=1 then (0,1)
        else let(f1, f2) = aux(f i, j) in
        let y=f i in 
        if f2<y then (f1+1, y)
        else (f1+1, f2)
        in aux(n,n)
        ;;
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	(*PRACTICA 6
	let partition f l:
		filter f l, filter (fun x -> not(f x)) l*)
