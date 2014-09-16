(** full and logsearch motion estimation. *)

type 'a metric_calc = 
  bw:int -> bh:int ->
  cur:Frame.U8.Plane.t -> 
  cx:int -> cy:int ->
  ref:Frame.U8.Plane.t -> 
  mx:int -> my:int -> 
  'a

module type Metric = sig
  type t 
  val init : t
  val compare : t -> t -> int
  val metric : t metric_calc
  val metric_umv : t metric_calc
end

module Sad : Metric with type t = int
module Sse : Metric with type t = int

type 'a mv_bounds = 
  ref:Frame.U8.Plane.t -> bw:int -> bh:int -> 
  mx:int -> my:int -> 'a

val mv_in_bounds : bool mv_bounds
val mv_in_bounds_umv : bool mv_bounds

val mv_clip_bounds : (int * int) mv_bounds
val mv_clip_bounds_umv : (int * int) mv_bounds

type 'a search_result =
  {
    mx : int;
    my : int;
    metric : 'a;
  }

type 'a search = 
  bw:int -> bh:int -> umv:bool ->
  cur:Frame.U8.Plane.t -> ref:Frame.U8.Plane.t -> 
  cx:int -> cy:int -> 
  sx:int -> sy:int -> 'a search_result

module type Search = sig
  type t 
  val logsearch : steps:int -> t search
  val fullsearch : window:(int*int) -> t search 
end

module Make(M : Metric) : Search with type t = M.t

module Search_sad : Search with type t = int
module Search_sse : Search with type t = int
