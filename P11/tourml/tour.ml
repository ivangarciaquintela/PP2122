let mTopR (x,y) = (x-1 , y+2);;

let mTopL (x,y) = (x+1 , y+2);;

let mLeftT (x,y) = (x+2,y+1);;

let mLeftD (x,y) = (x+2,y-1);;

let mRightT (x,y) = (x-2,y+1);;

let mRightD (x,y) = (x-2,y-1);;

let mDownR (x,y) = (x-1 , y-2);;

let mDownL (x,y) = (x+1 , y-2);;

let encontrado (a,b) l =
      List.exists (fun x -> x= (a,b)) l;;

let dentro n m x y =
          (x >= 0 && x < n) && (y >= 0 && y<m) ;;

let valid n m (x,y) l = not(encontrado ((x,y)) l) && dentro n m x y;;



      
let move n m (x,y) o= 
     if (valid n m (mTopR(x,y)) o) then mTopR(x,y)
else if (valid n m (mTopL(x,y)) o) then mTopL(x,y)
else if (valid n m (mLeftD(x,y)) o) then mLeftD(x,y)
else if (valid n m (mLeftT(x,y)) o) then mLeftT(x,y)
else if (valid n m (mRightD(x,y)) o) then mRightD(x,y)
else if (valid n m (mRightT(x,y)) o) then mRightT(x,y)
else if (valid n m (mDownL(x,y)) o) then mDownL(x,y)
else if (valid n m (mDownR(x,y)) o) then mDownR(x,y)
else (-1,-1);;

    
let tour n m (xi,yi) (xf, yf) =
    let rec aux (x,y) o =
        if (x == xf ) && (y == yf) then List.rev((x,y)::o)
    else  if((move n m (x,y) o) = (-1,-1)) then raise Not_found
    else begin print_endline(string_of_int(x));
    print_endline(string_of_int(y));
     aux (move n m (x,y) o) ((x,y)::o);
 end
in aux (xi,yi) [];;

tour 5 5 (1,1) (5,5);;