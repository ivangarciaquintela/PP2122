let move = function
 (x,y)->[ (x+2, y+1); (x+2, y-1); (x-2, y+1); (x-2, y-1);
				(x+1, y+2); (x-1, y+2); (x+1, y-2); (x-1, y-2) ]
;;


let dentro m n (x,y) = x > m || y > n || x < 1 || y < 1
;;


                
let tour m n ini fin =
	let rec valid r i =
		if i = fin
		then
			Some (i::r)
		else
			let x,y = i
			in
			if dentro m n (x,y) then None
			else
				if
					List.find_opt (function x -> x = i) r = None
				then
					let b =
						List.map (function x -> valid (i::r) x) (move i)
					in
					match
						List.find_opt (function x -> x <> None) b
					with
						  None -> None
						| Some x -> x
				else
					None
	in
		match
			valid [] ini
		with
			  None -> raise Not_found
			| Some x -> List.rev x
;;

                

tour 5 5 (1,1) (5,5);;
