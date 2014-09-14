open Bigarray

module type S = sig
  type ot
  type et
  val k : (ot,et) kind
end

module type Frame = sig

  (** planes are 2d bigarrays of pixels.
   
      They are layed out such that the x index (column) is contiguous ie

        p.{ y, x }
      
      This allows them to be converted to 1d arrays easily.
  *)
  module Plane : sig

    (* ocaml type of elements *)
    type ot
    (* c type of elements *)
    type et
    (* plane as 2d array *)
    type t = (ot, et, c_layout) Array2.t
    (* plane as 1d array *)
    type t1d = (ot, et, c_layout) Array1.t

    val width : t -> int
    val height : t -> int

    (** make a plane *)
    val make : w:int -> h:int -> t
    (** convert to 1d array *)
    val to_1d : t -> t1d
    val of_1d : t1d -> w:int -> h:int -> t
    (** iter over elements *)
    val iter : t -> (ot -> ot) -> unit
    (** iter over elements with index *)
    val iteri : t -> (x:int -> y:int -> ot -> ot) -> unit
  end

  (** type of chroma subsampling *)
  type chroma = C420 | C422

  (** YUV frame with seperate planes *)
  type t = 
    {
      width : int;
      height : int;
      chroma : chroma;
      y : Plane.t;
      u : Plane.t;
      v : Plane.t;
    }

  (** create a YUV 420 or 422 frame with given width and height *)
  val make : chroma:chroma -> w:int -> h:int -> t
  
  (** width of chroma plane *)
  val cwidth : t -> int

  (** height of chroma plane *)
  val cheight : t -> int

end

module Make(S : S) : Frame

module U8 : Frame with type Plane.ot = int
module S16 : Frame with type Plane.ot = int

