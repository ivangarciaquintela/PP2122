let rec mcd (x,y) : int =
   if y == 0 then x
   else mcd (y,(x mod y));;
   
print_int(mcd ((int_of_string(Sys.argv.(1))),(int_of_string(Sys.argv.(2)))));;
print_newline();;


