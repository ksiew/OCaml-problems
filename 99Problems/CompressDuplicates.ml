let compress lst = 
    let first = List.hd lst in
    let rec compress_help x = function
    |[] -> []
    |h::t -> if h = x then compress_help h t else h::compress_help h t
    in
    first::compress_help first (List.tl lst);;