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