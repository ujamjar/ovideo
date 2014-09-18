(* RGB <-> YUV *)

let clip255 x = 
  if x < 0. then 0.
  else if x > 255. then 255.
  else x
let clip255 x = int_of_float (clip255 x) 

let rgb2yuv r g b = 
  let r, g, b = float_of_int r, float_of_int g, float_of_int b in
  clip255 ((( 0.299  ) *. r) +. (( 0.587  ) *. g) +. (( 0.114  ) *. b)),
  clip255 (((-0.16874) *. r) +. ((-0.33126) *. g) +. (( 0.5    ) *. b)),
  clip255 ((( 0.5    ) *. r) +. ((-0.41869) *. g) +. ((-0.08131) *. b))

let yuv2rgb y u v =
  let y, u, v = float_of_int y, float_of_int u -. 128.0, float_of_int v -. 128.0 in
  clip255 (y +.  (( 1.4020) *. v)),
  clip255 (y +.  ((-0.3441) *. u) +. ((-0.7141) *. v)),
  clip255 (y +.  (( 1.7720) *. u))
