type age_pair = int * int

let ( -- ) lower upper =
  let rec aux n acc = if n < lower then acc else aux (n - 1) (n :: acc) in
  aux upper []

let gen_pairs mn mx =
  let base_list = mn -- mx in
  let range = List.nth base_list (List.length base_list - 1) in
  let create_count n =
    let rec aux curr acc =
      if curr > range then acc else aux (curr + 1) ((curr, n) :: acc)
    in
    aux mn []
  in
  List.flatten (List.map create_count base_list)

let exchange k =
  let x = string_of_int k in
  let y = x.[1] and z = x.[0] in
  int_of_string (String.make 1 y ^ String.make 1 z)

let is_valid_answer (ans : age_pair) : bool =
  let grand_father_age, grand_son_age = ans in
  grand_son_age * 4 == grand_father_age
  && exchange grand_father_age * 3 == exchange grand_son_age

let find (answer : age_pair) : age_pair =
  let max_grand_father_age, min_grand_son_age = answer in
  let age_pair_range = gen_pairs min_grand_son_age max_grand_father_age in
  let rec check_ages (curr_age_pair : int * int)
      (age_pair_list : (int * int) list) : int * int =
    if is_valid_answer curr_age_pair then curr_age_pair
    else if List.length age_pair_list <= 1 then (-1, -1)
    else check_ages (List.hd age_pair_list) (List.tl age_pair_list)
  in
  check_ages (List.hd age_pair_range) (List.tl age_pair_range)
