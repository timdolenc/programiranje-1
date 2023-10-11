
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [square] vrne kvadrat podanega celega števila.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # square 2;;
 - : int = 4
[*----------------------------------------------------------------------------*)

(*ctrl shift p da pozenes, naj bo funkcija f z argumentom x*)
let f x =
  let a = x+x in
  let b = x*x in
  a*b
let square x = x * x (*vsaka stvar v ocamlu neki vrne*)

let rec dolzina list = match list with (*mors dat rec ce je rekurzivna*)
  |[] -> 0
  |x :: xs -> 1 + dolzina xs

(*let rec dolzina list = function (*to je isto neki caka argument alneki*)
  |[] -> 0
  |x :: xs -> 1 + dolzina xs*)

(*----------------------------------------------------------------------------*]
 Funkcija [middle_of_triple] vrne srednji element trojice.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # middle_of_triple (true, false, true);; to je tuple seznam ma ; vmes
 - : bool = false
[*----------------------------------------------------------------------------*)

let middle_of_triple (a, b, c) = b

(*let rec middle_of_triple trpl =
    let (a,b,c) = trpl
    in b*)

let middle_of_triple (a,b,c) = b
(*----------------------------------------------------------------------------*]
 Funkcija [starting_element] vrne prvi element danega seznama. V primeru
 prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # starting_element [1; 2; 3; 4];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let starting_element = function
  | x :: xs -> x 
  | [] -> failwith "Slaba"





let starting_element sez = 
  match sez with
  | x :: xs -> x
  | _ -> failwith "Prekratek seznam"

let starting_element2 = function
  | x :: _ -> x
  | _ -> failwith "bla"



(*----------------------------------------------------------------------------*]
 Funkcija [multiply] zmnoži vse elemente seznama. V primeru praznega seznama
 vrne vrednost, ki je smiselna za rekurzijo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # multiply [2; 4; 6];;
 - : int = 48
[*----------------------------------------------------------------------------*)




let rec multiply sez = 
  match sez with
  | [] -> 1
  | h :: tail -> h * multiply tail

let rec multiply2 = function
  | [] -> 1
  | h :: tail -> h * multiply tail

(*----------------------------------------------------------------------------*]
 Napišite funkcijo ekvivalentno python kodi:

  def sum_int_pairs(pair_list):
      if len(pair_list) == 0:
        return []
      else:
        x, y = pair_list[0]
        return [x + y] + sum_int_pairs(pair_list[1:])

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_int_pairs [(1, -2); (3, 4); (0, -0)];;
 - : int list = [-1; 7; 0]
[*----------------------------------------------------------------------------*)

let rec sum_int_pairs = function
  | [] -> []
  | (x,y) :: xs -> (x+y) :: sum_int_pairs xs



      

(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)
let rec get k = function
  | x :: xs -> if k <= 0 then x else get (k-1) xs
  | _ -> failwith "Prekr"




let rec get k list =
  match (k,list) with
  | (_, []) -> failwith "prekr"
  | (k, x :: tail) -> if k <= 0 then x else get (k-1) tail

let rec get k = function (*ker je function ve, da je še en parameter*)
| [] -> failwith "prekr"
| x :: tail -> if k <= 0 then x else get (k-1) tail


(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)
let rec double = function
  | x :: xs -> x :: (x :: double xs)
  | _ -> []





(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k = function
  | h :: t -> if k <= 0 then x :: h :: t else h :: insert x (k-1) t
  | [] -> [x]




(*let rec insert x k = function (*h kot head, tisti parameter brez imena*)
  | [] -> x :: []
  | h :: tail -> 
    if k <= 0 then x :: h :: tail
    else h :: insert x (k-1) tail *)

(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)
let rec divide k = function
  | x :: xs -> if k > 0 then
    let (prvi, drugi) = divide (k-1) xs in
    (x :: prvi, drugi) else
      ([], x::xs)
  | [] -> ([], [])

let rec divide k list =
  match (k, list) with
  | (_, []) -> ([], [])
  | (k, list) when k <= 0 -> ([], list)
  | (k, x :: xs) -> 
    let (prvi, drugi) = divide (k-1) xs in
    (x :: prvi, drugi)





(*let rec divide k list =
  match (k, list) with
  | (_,[]) ->  ([],[]) 
  | (k, list) when k<= 0 -> ([], list)
  | (k, x :: xs) -> 
    let (list1, list2) = divide (k-1) xs in
      (x::list1, list2)*)
  

(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)
let rec rotate n = function
  |[] -> []
  |list -> let (prvi, drugi) = divide n list in
    drugi @ prvi
  




(*let rec rotate n = function
  | [] -> []
  | x :: xs -> insert x n (rotate n xs)*)

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x = function
  |[] -> []
  |h :: tail -> if h = x then remove x tail else h :: (remove x tail)

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec zrcali = function
  | [] -> []
  | x::xs -> (zrcali xs) @ [x] 

let is_palindrome list = 
  list = zrcali list
  
  

(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components l1 l2 =
  match l1, l2 with
  | ([],[]) -> []
  | ([], l) -> []
  | (l, []) -> []
  | (x1 :: xs1, x2::xs2) -> (max x1 x2) :: (max_on_components xs1 xs2)



(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let rec max_list = function
  |[] -> failwith "prekr"
  |[x] -> x
  |x :: xs -> max x (max_list xs)



let rec second_largest list =
  let nov = remove (max_list list) list in
  max_list nov
   
  
