(*1a*)
let sum_two_smallest (a,b,c) =
  a+b+c- (max a (max b c))

(*b*)


(*c*)
let rec dot_product a b =
  match (a,b) with
  |([],[]) -> 0
  |(x::xs,y::ys) -> x*y + dot_product xs ys
  |_ -> failwith "neustrezna dolzina"

(*d*)
let rec smallest_modulo list d =
  let moduli = (List.map ((mod)d) list) and 
  let rec max_number_list l =
    match l with 
    |[] -> None
    |x::_ -> x
    |x::xs -> max x (max_number_list xs) in 
  max_number_list moduli

(*e*)
let rec target_product = function
  let l = List.length list in 
  let rec aux k list = 
    |[] -> None
    |x::xs -> if k*x = l then (k,x) else aux k xs 
     
  





(*let rec target_product list =
  let l = List.length list in
  for i = 0 to l do
    for j = 0 to l do
      if (List.nth list i) * (List.nth list j) = l then ((List.nth list i), (List.nth list j)) else con
    done
  done*)
  


  
  



  
   