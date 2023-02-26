let hd = function
    [] -> failwith "hd"
  | h::t -> h
;;

let tl = function
    [] -> failwith "tl"
  | h::t -> t
;;

let length l = 
let rec length_aux len = function
    [] -> len
  | a::l -> length_aux (len + 1) l

in length_aux 0 l
;;

let nth l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux l n =
    match l with
    | [] -> failwith "nth"
    | h::t -> if n = 0 then h else nth_aux t (n-1)
  in nth_aux l n
;;

let append = (@)
;;

let rec find p = function
  | [] -> raise Not_found
  | h :: t -> if p h then h else find p t
;;

let rec for_all p = function
    [] -> true
  | h::t -> p h && for_all p t
;;

let rec exists p = function
    [] -> false
  | h::t -> p h || exists p t
;;

let rec mem x = function
    [] -> false
  | h::t -> compare h x = 0 || mem x t
;;

let init n f = 
  let rec aux m o=
    if (m<n) then aux (m+1) (f(m)::o)
    else List.rev(o)
  in aux 0 [];;

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | h :: t -> rev_append t (h :: l2)
;;


let rev l = rev_append l []
;;

let find_all p =
  let rec find accu = function
  | [] -> rev accu
  | x :: l -> if p x then find (x :: accu) l else find accu l in
  find []
;;

let filter = find_all
;;



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

let rec flatten = function
    [] -> []
  | h::t -> h @ flatten t
  ;;
  
let concat = flatten;;

let rec map f = function
[] -> []
| h::t -> let r = f h in r :: map f t
;;

let rev_map f l =
  let rec aux a = function
    | [] -> a
    | h::t -> aux (f h :: a) t
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

let compare_lengths l1 l2 =
if length(l1) < length(l2) then -1 
else
	Bool.to_int(length(l1) > length(l2))
;;