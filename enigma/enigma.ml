type age_pair = int * int;;

let exchange k =
  let x = string_of_int k in
  let y = String.get x 1 and z = String.get x 0 in
  int_of_string(String.make 1 y ^ String.make 1 z);;

let is_valid_answer (ans : age_pair) : bool =
  let grand_father_age, grand_son_age = ans in
    grand_son_age * 4 == grand_father_age && (exchange(grand_father_age) * 3 == exchange(grand_son_age));;

let rec find (answer : age_pair) : age_pair =
  let (max_grand_father_age, min_grand_son_age) = answer in
    let (_, _), (grandfather_age, grandson_age) =
      let rec check_grandfather (gfa, gsa) max_gfa min_gsa =
        if is_valid_answer (gfa, gsa) then (gfa, gsa)
        else if ((gfa < min_gsa && (gsa > max_gfa)) && not (is_valid_answer (gfa, gsa))) then (-1, -1)
        else if ((gfa < min_gsa) && not (is_valid_answer (gfa, gsa))) then check_grandson(gfa, gsa) max_gfa min_gsa
        else check_grandfather (gfa - 1, gsa) max_gfa min_gsa
      and
      check_grandson (granf_age, grans_age) max_gf_age min_gs_age =
        if is_valid_answer (granf_age, grans_age) then (granf_age, grans_age)
        else if ((grans_age > max_gf_age && (granf_age < min_gs_age)) && not (is_valid_answer (granf_age, grans_age))) then (-1, -1)
        else if ((grans_age > max_gf_age) && not (is_valid_answer (granf_age, grans_age))) then check_grandfather (granf_age, grans_age) max_gf_age min_gs_age
        else check_grandson (granf_age, grans_age + 1) max_gf_age min_gs_age
      in
      check_grandfather (max_grand_father_age, min_grand_son_age) max_grand_father_age min_grand_son_age, check_grandson (max_grand_father_age, min_grand_son_age) max_grand_father_age min_grand_son_age
      in (grandfather_age, grandson_age);;
