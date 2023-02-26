let f n =
	if n mod 2 = 0 then n/2
else 3*n+1;;

let rec orbit =  function n ->
	if n == 1 then print_endline("1")
else begin
	print_int(n);
	print_string(", ");
	orbit (f(n))
end;;

(*
orbit (int_of_string(Sys.argv.(1)));;
*)

let length n = 
	let rec aux (i:int) (c:int) =
		if c == 1 then i
	else aux (i+1) (f(c))
in aux 0 n;;	

(*
length (int_of_string(Sys.argv.(1)));;
*)


let top n = 
	let rec aux (i:int) (c:int) =
		if c == 1 then i
	else
		begin
			if f(c) > i then aux (f(c)) (f(c))
		else aux (i) (f(c))
	end
in aux 0 n;;	
(*
top (int_of_string(Sys.argv.(1)));;
*)

let length'n'top n = 
	let rec aux (i,d) c =
		if c == 1 then (i,d)
	else
		begin
			if f(c) > d then aux (i+1,f(c)) (f(c))
					else aux (i+1,d) (f(c))
		end
	in aux (0,0) n;;
	
(*
length'n'top (int_of_string(Sys.argv.(1)));;
*)
	
let longest_in m n =
	let rec aux d c = 
		if(c > n) then d
	else begin
		if length(d) < length(c) then aux (c) (c+1)
	else aux (d) (c+1)
	end
	in aux 1 m;;
	
let highest_in m n =
		let rec aux d c = 
			if(c > n) then d
		else begin
			if top(d) < top(c) then aux (c) (c+1)
		else aux (d) (c+1)
		end
		in aux 1 m;;