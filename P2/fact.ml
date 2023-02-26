let rec fact = function 0 -> 1 | n -> n * fact (n-1);;
let metodo x = if ((Array.length(x))!= 2) then "fact: numero de argumentos invalido" else string_of_int (fact (int_of_string(x.(1))));;
print_endline (metodo (Sys.argv))