module Chen : sig
  val idct : Frame.SInt.Plane.t -> Frame.SInt.Plane.t -> unit
  val fdct : Frame.SInt.Plane.t -> Frame.SInt.Plane.t -> unit
end
