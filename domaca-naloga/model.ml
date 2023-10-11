(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)
   
(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid = (**)
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = grid.(row_ind) (*sprejme grid tipa 'a grid in element tipa row_ind vrne vrstico row ind*)

let rows grid = List.init 9 (get_row grid) (*ustvari list od 0 do 8 in vsak element preslika z get_row torej indexe vrstic preslika v vrstice*)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind)) (*Array init 9 f najprej ustvari array [0;...;8] vzame njegove elemente kot indexe vrstic in jih preslika v elemente pripadajočih prstiv podanega stolpca*)

let columns grid = List.init 9 (get_column grid) (*ustvari list 0 do 8 in vsak element zamenja z stolpcem tega indexa*)

let get_box (grid : 'a grid) (box_ind : int) =  
  (*print_endline (string_of_int box_ind);*)
  Array.init 9 (fun ind_v_boxu -> grid.(3 * (box_ind / 3) + ind_v_boxu / 3).(3 * (box_ind mod 3) + ind_v_boxu mod 3))

let boxes grid = 
  List.init 9 (fun box_ind -> get_box grid box_ind)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun row_ind -> Array.map f (get_row grid row_ind))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str = 
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid }

let print_problem problem : unit = 
  let string_of_el x = 
    match x with
    | None -> " "
    | Some st -> string_of_int st
  in
  print_grid string_of_el problem

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = 
  print_grid string_of_int solution



let is_valid_solution (problem : problem) (solution : solution) = 
  let seznam = (rows solution) @ (columns solution) @ (boxes solution) in (*rows je funkcija ki sprejme solution, ki je podoben tipu grid le da je sam iz intov in  ga razbije v seznam vrstic, podobno columns naredi seznam vseh stolpcev in nakoncu združi vse v en seznam*)
    let rec preveri_stevila seznam = 
      match seznam with
      | [] -> true (*prisu si do konca seznama, torej vse vrstice stlp in boxi so ustrezni*)
      | a :: b when (List.sort (Int.compare) (Array.to_list a)) = [1;2;3;4;5;6;7;8;9] -> (*a je en box ali row ali col: najprej ga pretvorimo v lst potem pa sortamo po velikosti, če štima gremo dalje*)
        preveri_stevila b 
      | a :: b -> false 

      in preveri_stevila seznam
    (*in
      let rec preveri_ujemanje_aux problem solution velikost i j  = 
        match (i, j) with
        | (0, 0) -> preveri_ujemanje problem solution i j
        | (0, b) -> (preveri_ujemanje problem solution i j) && (preveri_ujemanje_aux problem solution velikost velikost (j - 1))
        | (a, _) -> (preveri_ujemanje problem solution i j) && (preveri_ujemanje_aux problem solution velikost (i-1) j)
      in ((preveri_ujemanje_aux problem solution 8 8 8) && (preveri_stevila seznam))*)

let grid1 = Array.init 9 (fun row_int -> Array.init 9 (fun x -> x))



let grid2 : 'a grid =
  [|    [| Some 4; Some 8; Some 3; Some 9; Some 2; Some 1; Some 6; Some 5; Some 7 |];
    [| Some 9; Some 6; Some 7; Some 3; Some 5; None; Some 8; Some 2; Some 1 |];
    [| Some 2; Some 5; Some 1; Some 8; Some 7; Some 6; Some 9; Some 4; Some 3 |];
    [| Some 5; Some 4; Some 8; Some 1; Some 3; Some 2; Some 9; Some 7; Some 6 |];
    [| Some 7; Some 2; Some 9; Some 6; Some 4; Some 3; Some 8; Some 1; Some 5 |];
    [| Some 1; Some 3; Some 6; Some 7; Some 9; Some 8; Some 4; Some 5; Some 2 |];
    [| Some 3; Some 7; Some 2; Some 6; Some 8; Some 9; Some 5; Some 1; Some 4 |];
    [| Some 8; Some 1; Some 4; Some 2; Some 5; Some 3; Some 7; Some 6; Some 9 |];
    [| Some 6; Some 9; Some 5; Some 4; Some 1; Some 7; Some 3; Some 8; Some 2 |];
  |]




(*
┏━━━┯━━━┯━━━┓
┃483│921│657┃
┃967│3 5│821┃
┃251│876│493┃
┠───┼───┼───┨
┃548│132│976┃
┃729│ 64│ 38┃
┃136│798│ 45┃
┠───┼───┼───┨
┃372│689│514┃
┃814│253│769┃
┃695│417│382┃
┗━━━┷━━━┷━━━┛*)
(*let grid2 = [|[|4;8;3;9;2;1|];[|9;6;7;3;None;5;8;2;1|];[|2;5;1;8;7;6;4;9;3|];[|5;4;8;1;3;2;9;7;6|];[|7;2;9;None;6;4;None;3;8|];[|1;|];[||];[||];[||]|]*)