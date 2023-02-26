let rec power x y =
if y==1 then x
else x * (power x (y-1) );;

print_endline(String.concat " " ["power ";(Sys.argv.(1));" "; (Sys.argv.(2));"= "; string_of_int(power (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2)))]);;

let rec power' x y =
if y==1 then x
else if (y mod 2 == 0) then (power' x ((y-1)/2) )*(power' x ((y-1)/2) )
else x * (power' x ((y-1)/2) ) * (power' x ((y-1)/2) );;

print_endline(String.concat " " ["power' ";(Sys.argv.(1));" "; (Sys.argv.(2));"= "; string_of_int(power' (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2)))]);;

let rec powerf (x:float) (y:int) =
if y==1 then x
else if (y mod 2 == 0) then (powerf x (y/2) )*.(powerf x (y/2) )
else x *. (powerf x ((y-1)/2) ) *. (powerf x ((y-1)/2) );;

print_endline(String.concat " " ["powerf' ";string_of_float(float_of_int(int_of_string(Sys.argv.(1))));" "; (Sys.argv.(2));"= "; string_of_float(powerf (float_of_int(int_of_string Sys.argv.(1))) (int_of_string Sys.argv.(2)))]);;
