type 'a g_tree = Gt of 'a * 'a g_tree list;;

let rec size = function 
    Gt (_,[]) -> 1
  | Gt (r,h::t) -> size h + size (Gt (r,t));;

(* devuelve la "altura", como número de niveles, de un g_tree *)
  let rec height  = function
       Gt (_, []) -> 1
    | Gt (_, _::t) -> 
      let sum = List.fold_left (+) 0 in
      2 + sum (List.map height t)
  ;; 
(* devuelve las hojas de un g_tree, "de izquierda a derecha" *)
  let rec leaves = function
      Gt (r,[])->[r]
      |Gt (r,h::[]) -> leaves h
      |Gt(r,h::t)-> leaves h @ leaves (Gt(r,t))
  ;;
  
(* devuelve la imagen especular de un g_tree *)
 let rec mirror (Gt(r,l)) =  Gt(r, List.rev(List.map mirror l) );;
    

(* devuelve la lista de nodos de un g_tree en "preorden" *)
 let rec preorder =
 	let rec aux l = function
 		  [] -> l
 		| h::t -> aux ( l @ (preorder h) ) t
 	in function
 	  Gt (r, []) -> [r]
 	| Gt (r, t) -> r::aux [] t;;

(* devuelve la lista de nodos de un g_tree en "postorden" *)
  let rec postorder = function
      Gt(r,[]) -> [r]
    | Gt (r,h::t) ->postorder h @ postorder (Gt(r,t))
  ;;
