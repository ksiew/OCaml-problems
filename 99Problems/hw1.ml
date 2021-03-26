let rec pow x = function
|0 -> 1
|n -> x * pow x (n -1);;

let compress lst = 
    let first = List.hd lst in
    let rec compress_help x = function
    |[] -> []
    |h::t -> if h = x then compress_help h t else h::compress_help h t
    in
    first::compress_help first (List.tl lst);;

let rec float_pow x = function
|0 -> 1.0
|n -> x *. float_pow x (n - 1);;

let rec remove_if lst func = 
match lst with
|[] -> []
|h::t -> if func h then remove_if t func else h::remove_if t func;;

let rec slice lst f l =
match lst with
|[] -> []
|h::t -> if f <= 0 && l > 0 then h::slice t (f - 1) (l - 1) else slice t (f - 1) (l - 1);; 


let rec equivs func lst = 
    let rec equivs_help x funct = function
    |[] -> []
    |h::t -> if funct x h = true then h::equivs_help x funct t else equivs_help x funct t
    in
    let rec remove x funct = function 
    |[] -> []
    |h::t -> if funct x h = false then h::remove x funct t else remove x funct t
    in
    match lst with
    |[] -> []
    |h::t ->  (h::equivs_help h func t)::equivs func (remove h func t);; 

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
    


let rec equiv_on f g = function
|[] -> true
|h::t -> if f h = g h then equiv_on f g t else false;;

let rec pairwisefilter func lst =
match lst with
|[] -> []
|[_] -> lst
|h::t -> (func h (List.hd t))::pairwisefilter func (List.tl t);;


let rec polynomial lst =
    let polynomial_help expr1 expr2 =
    fun x -> expr1 x + expr2 x
    in
    match lst with
    |[] -> fun x -> 0
    |h::t -> polynomial_help (fun x -> (pow x (snd h)) * (fst h)) (polynomial t);;

let rec powerset lst =
    let rec add_x x = function
    |[] -> [[x]]
    |h::t -> (x::h)::(add_x x t)
    in
    let rec powerset_help lst =
    match lst with
    |[] -> []
    |h::t -> (add_x h (powerset_help t))@powerset_help t
    in
    []::powerset_help lst;;



