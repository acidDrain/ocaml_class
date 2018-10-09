let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n::acc)
  in aux j [];;

(* let pairs = List.combine (10--99) (10--99);; *)

let gen_pairs start range =
  let rec make_pairs curr pacc =
    if curr > List.length range
      then pacc
    else
      let rec get_pairs nm acc =
        if nm > List.length range
          then make_pairs (curr+1) acc
        else get_pairs (nm+1) ((curr, nm) :: acc)
      in get_pairs curr pacc
  in make_pairs start [];;
