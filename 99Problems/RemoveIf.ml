let rec remove_if lst func = 
match lst with
|[] -> []
|h::t -> if func h then remove_if t func else h::remove_if t func;;