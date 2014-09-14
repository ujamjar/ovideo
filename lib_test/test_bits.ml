(***************************************************************)
(* Test the bitstream function by writing bits then reading 
 * them back and checking they are the same *)

open Printf
open Ovideo.Bits

module R = Reader(Byte_source)

module W = Ovideo.Bits.Writer(Ovideo.Bits.Buffer_sink)
let w' = Ovideo.Bits.Buffer_sink.init ()
let w = W.init w'

let test_data = 
  Array.init 1024 (fun _ ->
    let n = 1 + Random.int 29 in
    let b = Random.int (1 lsl n) in
    (n, b)
  )

let () = Array.iter (fun (n,b) -> W.put w n b) test_data
let () = W.put w 8 0 (* flush *)
let r = R.init (Byte_source.init (Buffer.contents w'))

let () = 
  Array.iteri (fun i (n,b) -> 
    let b' = R.get r n in
    printf "[%6i] %2i] %10i %10i\n" i n b b';
    assert (b' = b)
  ) test_data



