(** full and logsearch motion estimation. *)

type 'a metric_calc = 
  bw:int -> bh:int ->
  cur:Frame.U8.Plane.t -> 
  cx:int -> cy:int ->
  ref:Frame.U8.Plane.t -> 
  mx:int -> my:int -> 
  'a

module type Metric = sig
  (** type of metrics *)
  type t 
  (** init value for metric calculations *)
  val init : t
  (** comparison of metrics *)
  val compare : t -> t -> int
  (** metric calculation *)
  val metric : t metric_calc
  (** metric calculation with unrestricted motion vectors *)
  val metric_umv : t metric_calc
end

(** Sum of absolute differences metric *)
module Sad : Metric with type t = int
(** Sum of square differences metric *)
module Sse : Metric with type t = int

type 'a mv_bounds = 
  ref:Frame.U8.Plane.t -> bw:int -> bh:int -> 
  mx:int -> my:int -> 'a

(** is a motion vector, given a block size, fully within the frame *)
val mv_in_bounds : bool mv_bounds
(** is a motion vector, given a block size, at least partially within the frame *)
val mv_in_bounds_umv : bool mv_bounds

(** clip motion vector to be within the frame *)
val mv_clip_bounds : (int * int) mv_bounds
(** clip motion vector to be at least partially within the frame *)
val mv_clip_bounds_umv : (int * int) mv_bounds

(** search results - motion vectors and metric *)
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

  (*
module type Search = sig
  type t 
  (** three step search *)
  val threestep : steps:int -> t search
  (** full search *)
  val full : window:(int*int) -> t search 
end *)

module type Estimator = sig
  type t 
  type init
  val eval : init:init -> t search
end

module Three_step_search(M : Metric) : Estimator 
  with type init = int
   and type t = M.t

module Full_search(M : Metric) : Estimator
  with type init = int*int
   and type t = M.t

(*module Make(M : Metric) : Search with type t = M.t*)

module Search_sad : Estimator with type t = int
module Search_sse : Estimator with type t = int

