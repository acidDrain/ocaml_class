(*
Write a function is_sorted : string array -> bool which checks if the values of the input array are sorted in strictly increasing order, implying that its elements are unique (use String.compare).

Using the binary search algorithm, an element can be found very quickly in a sorted array.

Write a function find : string array -> string -> int such that find arr word is the index of the word in the sorted array arr if it occurs in arr or -1 if word does not occur in arr.

The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity. Beware that you really perform the minimal number of accesses. For instance, if your function has to test the contents of a cell twice, be sure to put the result of the access in a variable, and then perform the tests on that variable.
*)

let is_sorted (str_array : string array) : bool =
  if Array.length str_array <= 1 then true
  else
    let rec next n status last nxt sarr =
      if n < Array.length sarr - 1 then
        if String.compare last nxt == -1 then
          next (n + 1) (status && true) last sarr.(n + 1) sarr
        else next (n + 1) (status && false) nxt sarr.(n + 1) sarr
      else String.compare sarr.(n - 1) sarr.(n) == -1 && status
    in
    next 0 true str_array.(0) str_array.(1) str_array

let find (str_array : string array) (word : string) : int =
  let startIndex = 0 and endIndex = Array.length str_array - 1 in
  let rec binary_s starti endi =
    if starti > endi then -1
    else
      let middlei = starti + ((endi - starti) / 2) in
      let result = String.compare str_array.(middlei) word in
      if result == 0 then middlei
      else if result < 0 then binary_s (middlei + 1) endi
      else binary_s starti (middlei - 1)
  in
  binary_s startIndex endIndex
