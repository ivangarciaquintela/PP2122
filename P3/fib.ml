
let rec fib n =
if n <= 1 then n
else fib (n-1) + fib (n-2);;

let rec aux = function n ->
if n > (int_of_string(Sys.argv.(1)))  then ()
else begin
	print_int(fib n);
	print_newline ();
	aux (n+1)
end;;

aux 0;;
