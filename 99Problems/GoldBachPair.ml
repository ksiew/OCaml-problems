let prime x = 
    let rec prime_help x n = 
    if n >= x then true else if x mod n = 0 then false else prime_help x (n + 1)
    in
    prime_help x 2;;

let goldbachpair x = 
    let rec count_up n = 
    if prime n = true then n else count_up (n + 1)
    in
    let rec count_down n = 
    if prime n = true then n else count_down (n - 1) 
    in
    let rec goldbachpair_help (n1,n2) target =
    if n1 > n2 then goldbachpair_help (1,count_down (n2 - 1)) target else if n1 + n2 = target then (n1,n2) 
    else goldbachpair_help (count_up (n1 + 1), n2) target
    in
    goldbachpair_help (1,count_down (x - 1)) x;;

