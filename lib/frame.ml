open Bigarray

module type S = sig
  type ot
  type et
  val k : (ot,et) kind
end

module type Frame = sig

  module Plane : sig

    type ot
    type et
    type t = (ot, et, c_layout) Array2.t
    type t1d = (ot, et, c_layout) Array1.t

    val width : t -> int
    val height : t -> int

    val make : w:int -> h:int -> t
    val to_1d : t -> t1d
    val of_1d : t1d -> w:int -> h:int -> t
    val iter : t -> (ot -> ot) -> unit
    val iteri : t -> (x:int -> y:int -> ot -> ot) -> unit
  end

  type chroma = C420 | C422

  type t = 
    {
      width : int;
      height : int;
      chroma : chroma;
      y : Plane.t;
      u : Plane.t;
      v : Plane.t;
    }

  val make : chroma:chroma -> w:int -> h:int -> t

  val cwidth : t -> int
  val cheight : t -> int
  
end

module Make(S : S) = struct

  module Plane = struct

    type ot = S.ot
    type et = S.et
    type t = (ot, et, c_layout) Array2.t
    type t1d = (ot, et, c_layout) Array1.t

    let width = Array2.dim2 
    let height = Array2.dim1

    let make ~w ~h = Array2.create S.k c_layout h w
    
    let to_1d p = 
      reshape_1 (genarray_of_array2 p) ((height p)*(width p))

    let of_1d p ~w ~h = 
      reshape_2 (genarray_of_array1 p) h w
    
    let iter p f = 
      for y=0 to height p - 1 do
        for x=0 to width p - 1 do
          p.{y,x} <- f p.{y,x}
        done
      done
    
    let iteri p f = 
      for y=0 to height p - 1 do
        for x=0 to width p - 1 do
          p.{y,x} <- f ~x ~y p.{y,x}
        done
      done
  end

  type chroma = C420 | C422

  type t = 
    {
      width : int;
      height : int;
      chroma : chroma;
      y : Plane.t;
      u : Plane.t;
      v : Plane.t;
  }

  let cwidth f = f.width / 2 
  let cheight f = if f.chroma = C420 then f.height/2 else f.height

  let make ~chroma ~w ~h = 
    let cwidth = w/2 in
    let cheight = 
      if chroma = C420 then h/2 else h
    in
    let y = Plane.make w h in
    let u = Plane.make cwidth cheight in
    let v = Plane.make cwidth cheight in
    { width=w; height=h; chroma; y; u; v }

end

module U8 = Make(struct
  type ot = int
  type et = int8_unsigned_elt
  let k = int8_unsigned
end)

module S16 = Make(struct
  type ot = int
  type et = int16_signed_elt
  let k = int16_signed
end)

