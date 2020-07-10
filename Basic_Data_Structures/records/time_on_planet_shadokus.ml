(*
On planet Shadokus, a year has 5 months, each month has 4 days, each day has 3 hours and each hour has 2 minutes. A calendar date is therefore defined as the record type date of the given prelude.

A date is well-formed if its year index is >= 1, its month index is >= 1 and <= 5, its day index is >= 1 and <= 4, its hour index is >= 0 and <= 2, and its minute index is >= 0 and <= 1.

The start of year 12 would be:
{ year = 12; month = 1; day = 1; hour = 0; minute = 0 }

The end of year 12 would be:
{ year = 12; month = 5; day = 4; hour = 2; minute = 1 }

Write a function wellformed : date -> bool which checks that the input date is well formed.

On planet Shadokus, the origin of time is the discovery of the Big-Lambda-Machine, a magical computer that evaluates the infinite lambda-term of time. It is defined by value the_origin_of_time of the given prelude.

Write a function next : date -> date which computes the date which comes one minute after the input date.

In this computer, the time is represented by an integer that counts the number of minutes since 1/1/1 0:0 (the origin of time).

Write a function of_int : int -> date that converts such an integer into a date.

The given prelude

type date =
{ year : int; month : int; day : int;
hour : int; minute : int }

let the_origin_of_time =
{ year = 1; month = 1; day = 1;
hour = 0; minute = 0 }
 *)

type date = { year : int; month : int; day : int; hour : int; minute : int };;

let the_origin_of_time = { year = 1; month = 1; day = 1; hour = 0; minute = 0 };;

let wellformed (d : date) : bool =
  ((d.year >=1) && (d.month >= 1 && d.month <= 5) && (d.day >= 1 && d.day <= 4) && (d.hour >= 0 && d.hour <= 2) && (d.minute >= 0 && d.minute <= 1));;

let check_month (d: date) =
  if (d.month > 5) then {year = d.year + 1; month = 1 ; day = d.day; hour = d.hour; minute = d.minute} else {year = d.year; month = d.month; day = d.day; hour = d.hour; minute = d.minute};;

let check_day (d : date) =
  if (d.day > 4) then check_month { year = d.year; month = d.month + 1; day = 1; hour = d.hour; minute = d.minute } else { year = d.year; month = d.month; day = d.day; hour = d.hour; minute = d.minute };;

let check_hour (d : date) =
  if (d.hour > 2) then check_day { year = d.year; month = d.month; day = d.day + 1; hour = 0; minute = d.minute } else { year = d.year; month = d.month; day = d.day; hour = d.hour; minute = d.minute };;

let check_minute (d : date) =
  if (d.minute >= 1) then check_hour { year = d.year; month = d.month; day = d.day; hour = d.hour + 1; minute = 0} else { year = d.year; month = d.month; day = d.day; hour = d.hour; minute = 1};;

let next (d : date) : date =
  let is_good_date = wellformed d in
    if is_good_date then
     check_minute d else
     d;;

let of_int (mins : int) : date =
  let rec count n next_date =
    if n == 0 then next_date else count (n-1) (next next_date) in
  count mins { year = 1; month = 1; day = 1; hour = 0; minute = 0 };;