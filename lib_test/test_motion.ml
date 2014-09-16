(* test the motion estimator *)

open Printf
open Ovideo
open Frame.U8
module P = Plane

(* mv bounds checking assertions *)
let () = begin
  let bw, bh = 4, 4 in
  let ref = P.make ~w:6 ~h:6 in
  let b = Motion.mv_in_bounds ~ref ~bw ~bh in
  assert (b 0 0 = true);
  assert (b (-1) 0 = false);
  assert (b 0 (-1) = false);
  assert (b (-1) (-1) = false);
  assert (b 2 2 = true);
  assert (b 2 3 = false);
  assert (b 3 2 = false);
  assert (b 3 3 = false);
  let b = Motion.mv_in_bounds_umv ~ref ~bw ~bh in
  assert (b (-3) (-3) = true);
  assert (b (-4) (-3) = false);
  assert (b (-3) (-4) = false);
  assert (b (-4) (-4) = false);
  assert (b 5 5 = true);
  assert (b 5 6 = false);
  assert (b 6 5 = false);
  assert (b 6 6 = false);
  let b = Motion.mv_clip_bounds ~ref ~bw ~bh in
  assert (b 0 0 = (0,0));
  assert (b (-1) 1 = (0,1));
  assert (b 2 (-1) = (2,0));
  assert (b (-1) (-1) = (0,0));
  assert (b 2 2 = (2,2));
  assert (b 1 3 = (1,2));
  assert (b 3 0 = (2,0));
  assert (b 3 4 = (2,2));
  let b = Motion.mv_clip_bounds_umv ~ref ~bw ~bh in
  assert (b 0 0 = (0,0));
  assert (b (-3) (-3) = (-3,-3));
  assert (b 2 (-4) = (2,-3));
  assert (b (-4) (-5) = (-3,-3));
  assert (b 5 5 = (5,5));
  assert (b 1 6 = (1,5));
  assert (b 6 (-2) = (5,-2));
  assert (b 7 8 = (5,5));
end

module Mf = Motion.Full_search(Motion.Sad)
module Mt = Motion.Three_step_search(Motion.Sad)

let bw, bh = 4,4
let umv = false

let cur = P.make ~w:bw ~h:bh
let ref = P.make ~w:12 ~h:12

let () = P.clear cur 0
let () = P.clear ref 0

let () = begin
  cur.{1,0} <- 1;
  cur.{1,1} <- 1;
  ref.{3,1} <- 1;
  ref.{3,2} <- 1
end

let print_mv r = 
  Motion.(printf "mv=[%i, %i] metric=%i\n" r.mx r.my r.metric)

let window = 3, 2
let cx, cy = 0, 0
let sx, sy = 4, 4

let r = Mf.eval ~init:window ~bw ~bh ~umv ~cur ~ref ~cx ~cy ~sx ~sy
let () = print_mv r

let steps = 2

let r = Mt.eval ~init:steps ~bw ~bh ~umv ~cur ~ref ~cx ~cy ~sx ~sy
let () = print_mv r


