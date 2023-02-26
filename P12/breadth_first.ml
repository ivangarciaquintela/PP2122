
open G_tree;;
let rec breadth_first = function
       Gt (x, []) -> [x]
     | Gt (x, (Gt (y, t2))::t1) -> x :: breadth_first (Gt (y, t1@t2));;
  
  let breadth_first_t a =
    let rec aux acc = function
        Gt (x, []) -> List.rev (x::acc)
      | Gt (x, Gt(r, h)::l) ->
        aux (x::acc) (Gt(r, List.rev_append (List.rev l) h))
    in
    aux [] a;;
    
    
    let init = function n -> function f ->
      let rec aux (f,i,l) = match (f,i,l) with
          (_,0,l) -> (f i)::l
        |(f,i,lst) -> aux(f,(i-1),(f i)::lst)
      in
      aux(f,n-1,[]);;

    let id x = x;;
    let leaf v = Gt(v,[]);;
    let init_tree n = Gt(n, List.rev_map leaf (List.init n id));;

    let t = init_tree 500_00;;