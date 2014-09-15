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
    val init : w:int -> h:int -> (x:int -> y:int -> ot) -> t
    val to_1d : t -> t1d
    val of_1d : t1d -> w:int -> h:int -> t
    val map : ?alloc:bool -> (ot -> ot) -> t -> t
    val mapi : ?alloc:bool -> (x:int -> y:int -> ot -> ot) -> t -> t
    val map2 : (ot -> ot -> ot) -> t -> t -> t
    val map2i : (x:int -> y:int -> ot -> ot -> ot) -> t -> t -> t
    val iter : (ot -> unit) -> t -> unit
    val iteri : (x:int -> y:int -> ot -> unit) -> t -> unit
    val clear : t -> ot -> unit
    val blit : x:int -> y:int -> w:int -> h:int -> dx:int -> dy:int -> t -> t -> unit
    val sub : x:int -> y:int -> w:int -> h:int -> t -> t
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
 
    let map ?(alloc=true) f p = 
      let q = if alloc then make ~w:(width p) ~h:(height p) else p in
      for y=0 to height p - 1 do
        for x=0 to width p - 1 do
          q.{y,x} <- f p.{y,x}
        done
      done;
      q

    let mapi ?(alloc=true) f p = 
      let q = if alloc then make ~w:(width p) ~h:(height p) else p in
      for y=0 to height p - 1 do
        for x=0 to width p - 1 do
          p.{y,x} <- f ~x ~y p.{y,x}
        done
      done;
      q

    let map2 f p q = 
      let r = make ~w:(width p) ~h:(height p) in
      for y=0 to height p - 1 do
        for x=0 to width p - 1 do
          r.{y,x} <- f p.{y,x} q.{y,x}
        done
      done;
      r

    let map2i f p q = 
      let r = make ~w:(width p) ~h:(height p) in
      for y=0 to height p - 1 do
        for x=0 to width p - 1 do
          r.{y,x} <- f ~x ~y p.{y,x} q.{y,x}
        done
      done;
      r

    let iter f p = 
      for y=0 to height p - 1 do
        for x=0 to width p - 1 do
          f p.{y,x}
        done
      done
    
    let iteri f p = 
      for y=0 to height p - 1 do
        for x=0 to width p - 1 do
          f ~x ~y p.{y,x}
        done
      done
      
    let init ~w ~h f = 
      let p = make ~w ~h in
      mapi ~alloc:false (fun ~x ~y _ -> f ~x ~y) p

    let clear p v = Array2.fill p v

    let blit ~x ~y ~w ~h ~dx ~dy src dst =
      for j=0 to h - 1 do
        for i=0 to w - 1 do
          dst.{j,i} <- src.{y+j,x+i}
        done
      done

    let sub ~x ~y ~w ~h p = 
      let p' = make ~w ~h in
      blit ~x ~y ~w ~h ~dx:0 ~dy:0 p p';
      p'

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

module SInt = Make(struct
  type ot = int
  type et = int_elt
  let k = int
end)


