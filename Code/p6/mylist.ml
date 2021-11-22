let hd = function
    [] -> raise (Failure "hd")
    | h::_ -> h
;;

let tl = function
    [] -> raise (Failure "tl")
    | _::t -> t
;;

let length l =
    let rec aux l acum = match l with
        [] -> acum
        | h::t -> aux t (acum + 1)
      in aux l 0
;;

let rec compare_lengths l1 l2 = match l1, l2 with
    [], [] -> 0
    | [], h::t -> (-1)
    | h::t, [] -> 1
    | _::t1, _::t2 -> compare_lengths t1 t2
;;

let rec nth l n =
    if n < 0
    then raise (Invalid_argument"List.nth")
    else let auxl = function
        [] -> raise (Failure"nth")
        | h::t -> (function 0 -> h | n -> nth t (n - 1))
        in auxl l n
;;

function l -> function n ->
    if n < 0
    then hd l
    else nth(tl l)(n - 1)
;;
         
let rec append = function
    [] -> (function l2 -> l2)
    | h::t -> function l2 -> h::append t l2
;;

let rec find p l = match l with
    [] -> raise(Not_found)
    | h::t -> (if (p h) then h else find p t);;

let for_all f l =
    let rec aux f l vf = match l with
        [] -> vf
        | h::t -> (aux f t ((f h)&&(vf)))
      in aux f l true;;

let exists f l =
    let rec aux f l vf = match l with
        [] -> vf
        | h::t -> (aux f t ((f h)||(vf)))
    in aux f l false;; 

let rec mem a l = match l with
    [] -> false
    | h::t -> (if a == h then true else mem a t);;

let rev l =
    let rec aux v = function
        [] -> v
        | h::t -> aux (h::v) t
      in aux [] l;;

let rec filter f l =
    let rec aux f l auxl = match l with
        [] -> auxl
        | h::t -> if f h
          then aux f t (h::auxl)
          else aux f t auxl
        in aux f (rev l) [];;

let find_all = filter;;

let partition f l =
    let rec aux f l l1 l2 = match l with
        [] -> (rev l1, rev l2)
        | h::t -> if f h
          then aux f t (h::l1) l2
          else aux f t l1 (h::l2)
        in aux f l [] [];;

let rec split = function
    [] -> ([], [])
    | (h1, h2)::t -> let (l1, l2) = split t in (h1::l1, h2::l2);;

let rec combine l1 l2 = match (l1, l2) with
    ([], []) -> []
    | (h1::t1, h2::t2) -> (h1, h2)::combine t1 t2
    | (_, _) -> raise(Invalid_argument"combine");;

let init n f =
    if n < 0
       then raise (Invalid_argument"len < 0")
       else let rec aux (i, l) =
        if i = n then l else aux(i + 1, f i::l)
        in rev(aux(0, []));;

let rev_append l1 l2 =
    let rec aux l1 l = match l1 with
        [] -> l
        | h::t -> aux t (h::l)
      in aux l1 l2;;

let rec concat = function
    [] -> []
    | h::t -> append h (concat t);;

let flatten = concat;;

let rec map f l = match l with
    [] -> []
    | h::t -> f(h)::map f t;;

let rev_map f l =
    let rec aux l auxl = match l with
        [] -> auxl
        | h::t -> aux t (f(h)::auxl)
    in aux l [];;

let rec map2 f l1 l2 =
    if (length l1 != length l2)
        then raise (Invalid_argument"map2")
        else if (length l1 == 0)
          then []
          else (f(hd l1)(hd l2))::map2 f (tl l1)(tl l2);;

let rec fold_left f a l = match l with
    [] -> a
    | h::t -> fold_left f (f a h) t;;

let rec fold_right f l b = match l with
    [] -> b
    | h::t -> f h (fold_right f t b);;
