let rec qsort1 ord = function
       [] -> []
     | h::t -> let after, before = List.partition (ord h) t in
               qsort1 ord before @ h :: qsort1 ord after;;
