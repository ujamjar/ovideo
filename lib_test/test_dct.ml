(* test the chen dct/idct *)

open Printf
open Ovideo
open Dct

let dump d = 
  printf "********************\n";
  for i=0 to 7 do
    for j=0 to 7 do
      printf "%6i " d.{i,j}
    done;
    printf "\n%!"
  done

let rnd x = 
  if x >= 0 then (x+2)/4 else (x-2)/4

let din = Frame.SInt.Plane.init ~w:8 ~h:8 (fun ~x ~y -> Random.int 512 - 256)
let d = Frame.SInt.Plane.map (fun x -> x) din
let d' = Frame.SInt.Plane.make ~w:8 ~h:8

let () = dump d
let () = Chen.fdct d d'
let () = dump d'
let () = Chen.idct d' d
let () = dump d
let () = dump (Frame.SInt.Plane.map2 (fun x y -> y - (rnd x)) d din)

