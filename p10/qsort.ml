let rec qsort1 ord = function
     [] -> []
    | h::t -> let after, before = List.partition (ord h) t in
              qsort1 ord before @ h :: qsort1 ord after;;
  
(*
  Esta implementacion no es buena para casos en los que la
  lista no este balanceada
*)
  
let rec qsort2 ord =
    let append’ l1 l2 = List.rev_append (List.rev l1) l2 in
    function
        [] -> []
      | h::t -> let after, before = List.partition (ord h) t in
                append’ (qsort2 ord before) (h :: qsort2 ord after);;

(*
  La ventaja de no usar @ es usar funciones terminales como rev_append y rev.
  Ademas qsort2 es mas rapido cuando la lista esta ya ordenada.
  qsort2 permite ordenar listas mas grandes que qsort1 sin soltar un Stack Overflow
*)

let init n f =
  let rec aux (i, l) =
    if i = n
      then l
      else aux(i + 1, f i::l)
    in rev(aux(0, []));;


let l1 = init 600000 (function x -> Random.int 5000);;
(*
  La desventaja que tiene qsort2 es que es mas lenta si 
  la lista esta ordenada aleatoriamente o si esta ordenada 
  de forma inversa, es del orden de 117% mas lento que la 1
*)
  

