let rec slice lst f l =
match lst with
|[] -> []
|h::t -> if f <= 0 && l > 0 then h::slice t (f - 1) (l - 1) else slice t (f - 1) (l - 1);;
 