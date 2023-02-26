let to0from n =
      let rec aux i m =
      if i > n then m
      else aux (i+1) (i::m)
  in aux 0 [] ;;

  (* let to0from n =  if n < 0 then [] else List.init (n+1) (fun i -> n-1) *)
let fromto m n =
  let rec aux i x =
  if i < m then x
  else aux (i-1) (i::x)
  in aux n [];;

  (*
let rec from1to n =
  let rec aux i x =
  if i < 1 then x
  else aux (i-1) (i::x)
  in aux n [];;
*)
let rec from1to n = fromto 1 n;;
  
  (*
let map f l= 
    let rec aux p o=
    match p with
    []-> o
    |h::t -> aux t (f h::o)
    in aux (List.rev l) [];;
   *)
let map f l = List.rev(List.rev_map f l);;


let power x y =
    let rec innerpower i a =
        if i = 0 then a
        else innerpower (i-1) (a*x)
in 
    if y>=0 then innerpower y 1
    else invalid_arg "power"
;;

let incseg l =
    let rec aux a o c =
        match a with
        |[] -> List.rev(o)
        |h::t -> aux t ((h+c)::o) (h+c)
    in aux l [] 0;;

let remove x l = 
    let rec aux v o =
        match v with 
        [] -> List.rev(o)
    | h :: t -> if x = h then aux t o
    else aux t (h::o)
in aux l []
;;

let divide l=
let rec aux laux i l1 l2 =
    match laux with 
    |[] -> List.rev(l1),List.rev(l2)
    |h::t -> if (i mod 2) > 0 then aux t (i+1) l1 (h::l2)
                else aux t (i+1) (h::l1) l2
    in aux l 0 [] []
;;

let compress l =
    let rec aux laux o =
        match laux with
        |[] -> List.rev(o)
        |h1::h2::t -> if (h1 = h2) then aux (h2::t) (o)
        else aux (h2::t) (h1::o)
        |h::[] -> aux [] (h::o)
in aux l []
;;

