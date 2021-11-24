(* 
FALTA EL COMPRESS
*)


    
let to0from n = 
    let rec aux lol i =
        if i>n then lol
        else aux (i::lol) (i+1) in
    aux [] 0;;


let fromto m n =
    let rec aux lol i=
        if i<m then lol
        else aux (i::lol) (i-1) in
    aux [] n;;
     
     
let from1to n = fromto 1 n;; (* Hace lo mismo que la fromto pasandole el 1 como el menor numero*)


let map f lol = 
    let rec aux acc = function
        [] -> acc
      | h::t -> aux ((f h)::acc) t in
    aux [] (List.rev lol);;
  
let power x y =
    let rec aux acc i =
        if i=y then x*acc
        else aux (x*acc) (i+1) in
            if y<0 then raise (Invalid_argument "power")
            else if y=0 then 1
            else aux 1 1;;
  
let incseg lol = let rec aux accl accn = function
      [] -> accl
    | h::t -> aux ((h+accn)::accl) (h+accn) t in
            List.rev (aux [] 0 lol);;
  
let remove x lol = let rec aux lno = function
      [] -> lno
    | h::t -> if (x=h) then List.rev_append lno t
      else aux (h::lno) t in
           aux [] lol;;

let rec divide = function
      [] -> [], []
    | h::[] -> [h], []
    | h1::h2::t -> let t1, t2 = divide t in
                   (h1::t1, h2::t2);;
  
  

      
      
      
      
      
      

