module type Source = sig
  type t
  val next_byte : t -> int option
end

module Byte_source : sig
  type t = int ref * Bytes.t
  val next_byte : t -> int option
  val init : Bytes.t -> t
end

module File_source : sig
  type t = in_channel
  val next_byte : t -> int option
  val init : string -> t
end

module type Reader = sig

  type source
  type t

  val init : source -> t
  val show : t -> int -> int
  val advance : t -> int -> unit
  val get : t -> int -> int
  val pos : t -> int

end

module Reader(S : Source) : Reader
  with type source = S.t

module type Sink = sig
  type t 
  val put_byte : t -> int -> unit
end

module Buffer_sink : sig
  type t = Buffer.t
  val put_byte : t -> int -> unit
  val init : unit -> t
end

module File_sink : sig
  type t = out_channel
  val put_byte : t -> int -> unit
  val init : string -> t
end

module type Writer = sig

  type sink
  type t

  val init : sink -> t
  val put : t -> int -> int -> unit
  val pos : t -> int

end

module Writer(S : Sink) : Writer
  with type sink = S.t



