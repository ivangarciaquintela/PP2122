let hd l =
	match l with
	| [] -> raise (Failure "hd")
	| h :: _ -> h
;;
let tl = function
  [] -> raise (Failure "tl")
  | _ :: t -> t
  ;;
let rec length = function
    [] -> 0
    | h::t -> 1 + length t
    ;;

let compare_lengths l1 l2 = 
	if length(l1) > length(l2) then 1
	else 
		if length(l1) < length(l2) then -1
		else 0
;;	
	
let rec nth mylist index = match mylist with
	    | [] -> raise (Failure"nth")
	    | h::t ->
	        if index = 0 then h
	        else nth t (index-1)
;;

let append l1 l2 =
	let rec loop l1 l2 =
      match l1 with
      | [] -> l2
	  | h :: t -> h::loop t l2
  in  loop l1 l2
;;

let rec find a l =
    match l with
    | [] -> raise (Not_found)
    | h :: t -> if a(h) then 0
                else 1 + find a t
;;

let rec for_all a l =
    match l with
        []        -> true
      | h :: t -> a h && for_all (a) t
;;
	  
let rec exists a l =
	match l with
	[]        -> false
	| h :: t -> a h || exists a t
;;

let init n f = 
  let rec aux m o=
    if (m<n) then aux (m+1) (f(m)::o)
    else List.rev(o)
  in aux 0 [];;
  
let rec mem a l =
    match l with
    | [] -> false
    | h :: t -> if a = h then true
                else mem a t
;;

let rec filter a = function 
| [] -> [] 
| h::t -> (if a h then [h] else [])@(filter a t) 
;;

let rec find p = function
  | [] -> raise Not_found
  | h :: t -> if p h then h else find p t
;;

let rec find_all a l = match l with
| [] -> [] 
| h::t -> (if a h then [h] else [])@(filter a t) 
;;

let rev l =
  let rec rlist l2 = function
| [] -> l2
| h::t -> rlist (h::l2) 
t in rlist [] l;;

let partition f l =
  let rec aux i o = function
  | [] -> (rev i, rev o)
  | h :: t -> if f h then aux (h :: i) o t else aux i (h :: o) t in
  aux [] [] l
;;

let rec split = function
    [] -> ([], []) 
	| (x,y)::t -> let (x2, y2) = split t in (x::x2, y::y2)
  ;;

  let rec combine l1 l2 =
    match (l1, l2) with
      ([], []) -> []
    | (h1::t1, h2::t2) -> (h1, h2) :: combine t1 t2
    | (_, _) -> invalid_arg "List.combine"
;;

let rev l =
  let rec rlist l2 = function
| [] -> l2
| h::t -> rlist (h::l2) 
t in rlist [] l;;


let rev_append l1 l2 =
	let rec list a l1 l2 = 
		match l1 with
		[] -> a@l2
		| h::t -> list (h::a) t l2
	in list [] l1 l2
;;

let rec concat l =
  match l with
  [] -> []
  | h :: t -> h@(concat t)
  ;;
let flatten l =
        let rec g a = function
        | [] -> a
        | h :: t -> g (a@h) t
        in g [] l
;;
let rec map x = function
    | [] -> []
    | a::l -> x a :: map x l
;;

let rev_map x l =
  let rec aux o = function
    | [] -> o
    | h::t-> aux (x h :: o) t
  in
  aux [] l
;;

let rec map2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (h1::t1, h2::t2) -> let r = f h1 h2 in r :: map2 f t1 t2
  | (_, _) -> invalid_arg "List.map2"
;;



let rec fold_left op a l =
      match l with
        [] -> a
      | h::t -> fold_left op (op a h) t
      ;;

let rec fold_right op l a =
    match l with
    [] -> a
    | h::t -> op h (fold_right op t a)
;;