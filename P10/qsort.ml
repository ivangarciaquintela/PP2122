let rec qsort1 ord = function
            [] -> []
          | h::t -> let after, before = List.partition (ord h) t in
                    qsort1 ord before @ h :: qsort1 ord after
                
;;

let rec qsort2 ord = 
    let append' l1 l2 =  List.rev_append (List.rev l1) l2 in
    function 
    [] -> []
    | h::t -> let after, before = List.partition (ord h) t
    in append' (qsort2 ord before) (h :: qsort2 ord after);;
    
let l1 = List.init 300_000 (function x -> Random.int 1000);;
(*
    qsort1 tiene peor rendimiento en el caso de que la mayoria de los elementos no cumplant la condicion or.
    qsort2 puede procesar mayores listas por ser recursiva terminal, aunque es mas lento porque las funciones terminales actuan como bucles infinitos.
    qsort2 puede ordenar listas mayores que qsort1 no.
    
    
    Cuando tiene tamaño 300_000
    qsort1 tarda 0.4215 segundos
    qsort2 tarda 2.1433 segundo
    
    
*)