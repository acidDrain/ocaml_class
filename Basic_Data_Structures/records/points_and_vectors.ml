(* The given prelude defines three types, one for three dimensional points, another for velocity vectors in three dimensions, and another one representing moving objects in space.

Write a function move : point -> dpoint -> point such that move p dp is the point p whose coordinates have been updated according to dp.
(x is now x +. dx, y is now y +. dy, z is now z +. dz.

Write a function next : physical_object -> physical_object such that next o is the physical object o at time t + dt.
The position of next o is the position of o moved according to its velocity vector.
Suppose that these objects are spheres whose radius is 1.0.

Write a function will_collide_soon : physical_object -> physical_object -> bool that tells if at the next instant, the two spheres will intersect. *)

(* r = d/t *)
(* d = r * t *)
(* t = d/r *)

(* let t = 1.0;; *)

type point  = { x : float; y : float; z : float };;
type dpoint = { dx : float; dy : float; dz : float };;
type physical_object = { position : point; velocity : dpoint };;

let move (p : point) (dp : dpoint) : point =
  { x = p.x +. dp.dx; y = p.y +. dp.dy ; z = p.z +. dp.dz } ;;

let next (obj : physical_object) : physical_object =
  { position = move obj.position obj.velocity; velocity = obj.velocity };;

let get_sphere (sphere : point) : float =
  (sphere.x ** 2.0) +. (sphere.y ** 2.0) +. (sphere.z ** 2.0);;

let check_intersect (s1 : point) (s2 : point) : bool =
  ((s1.x -. s2.x) ** 2.0) +. ((s1.y -. s2.y) ** 2.0) +. ((s1.z -. s2.z) ** 2.0) <= get_sphere s2;;

let will_collide_soon (p1 : physical_object) (p2 : physical_object) : bool =
    let nextsphere1 = next p1 and nextsphere2 = next p2 in
    check_intersect nextsphere1.position nextsphere2.position || check_intersect nextsphere2.position nextsphere1.position;;
