(*
Consider a non empty array of integers a.

Write a function min : int array -> int that returns the minimal element of a.

Write a function min_index : int array -> int that returns the index of the minimal element of a.

Do you think these functions work well on large arrays ?

Define a variable it_scales and set it to "yes" or "no".

*)
let min (int_array : int array) : int =
  let rec check_val n curr arr =
    if n < Array.length arr then
      if arr.(n) < curr then
        check_val (n+1) arr.(n) arr
      else
        check_val (n+1) curr arr
    else curr
  in check_val 0 int_array.(0) int_array;;

let min_index (int_array : int array) : int =
  let rec check_val n index curr arr =
    if n < Array.length arr then
      if arr.(n) < curr then
        check_val (n+1) n arr.(n) arr
      else
        check_val (n+1) index curr arr
    else index
  in check_val 0 0 int_array.(0) int_array;;

let it_scales = "no";;
