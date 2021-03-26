let rec pow x = function
|0 -> 1
|n -> x * pow x (n -1);;

let rec float_pow x = function
|0 -> 1.0
|n -> x *. float_pow x (n - 1);;
