(* let print_list_item (x, y) =
  print_string (" (" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ") ");;

let print_list_items some_list =
  List.map print_list_item some_list;; *)

let (--) lower upper =
  let rec aux n acc =
    if n < lower then acc else aux (n-1) (n::acc)
  in aux upper [];;

let gen_pairs mn mx =
  let base_list = (mn--mx) in
  let range = List.nth base_list ((List.length base_list) - 1) in
  let create_count n =
    let rec aux curr acc =
      if curr > range then acc else aux (curr+1) ((curr, n)::acc)
    in aux mn []
  in List.flatten (List.map create_count base_list);;