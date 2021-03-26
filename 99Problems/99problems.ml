let rec last = function
|[] -> "none"
|[x] -> x
|h::t -> last t;;

let rec last_two = function
| [] | [_] -> None
|[x;y] -> Some (x,y)
|_::t -> last_two t;;

let rec at x = function
|[] -> "None"
|h::t -> if x = 1 then h else at (x - 1) t;;

let rec length = function
|[] -> 0
|_::t -> 1 + length t;;

let rec test x = function
|[] -> [[x]]
|h::t -> (x::h)::(test x t);;

test 4 [[1];[2];[3]];;
