(* bitstream reader *)

module type Source = sig
  type t
  val next_byte : t -> int option
end

module Byte_source = struct
  type t = int ref * Bytes.t
  let init b = ref 0, b
  let next_byte (pos,bytes) = 
    try
      let b = Bytes.get bytes !pos in
      pos := !pos + 1;
      Some (Char.code b)
    with _ ->
      None
end

module File_source = struct
  type t = in_channel
  let init fname = open_in_bin fname
  let next_byte c = try Some(input_byte c) with _ -> None
end

module Reader( S : Source ) = struct

  type source = S.t
  type t = 
    {
      mutable buffer : int64;
      mutable bufbits : int;
      mutable pos : int;
      source : source;
    }

  let rec fill_buffer b = 
    if b.bufbits < 32 then 
      match S.next_byte b.source with
      | None -> ()
      | Some(byte) -> begin
        b.buffer <- Int64.(logor (shift_left b.buffer 8) (Int64.of_int byte));
        b.bufbits <- b.bufbits + 8;
        fill_buffer b
      end

  let init source = 
    {
      buffer = 0L;
      bufbits = 0;
      pos = 0;
      source;
    }

  let masks = Array.init 33 (fun i -> Int64.( sub (shift_left 1L i) 1L ))

  let show b n = 
    fill_buffer b;
    let v = Int64.(logand (shift_right_logical b.buffer (b.bufbits - n)) masks.(n)) in
    Int64.to_int v

  let advance b n = 
    b.bufbits <- b.bufbits - n;
    b.pos <- b.pos + n

  let get b n = 
    let v = show b n in
    advance b n;
    v

  let pos b = b.pos

end

module type Sink = sig
  type t 
  val put_byte : t -> int -> unit
end

module Buffer_sink = struct
  type t = Buffer.t
  let init () = Buffer.create 1024
  let put_byte t v = Buffer.add_char t (Char.chr v)
end

module File_sink = struct
  type t = out_channel
  let init fname = open_out_bin fname
  let put_byte t v = output_byte t v
end

module Writer(S : Sink) = struct

  type sink = S.t
  type t = 
    {
      mutable buffer : int64;
      mutable bufbits : int;
      mutable pos : int;
      sink : sink;
    }

  let init sink = 
    {
      buffer = 0L;
      bufbits = 0;
      pos = 0;
      sink;
    }

  let put b n v = 
    (* pack into buffer *)
    b.buffer <- Int64.(logor (shift_left b.buffer n) (of_int v));
    b.bufbits <- b.bufbits + n;
    while b.bufbits >= 8 do
      (* unpack a byte at a time *)
      let bb = b.bufbits - 8 in
      let byte = Int64.(to_int (logand (shift_right_logical b.buffer bb) 255L)) in
      (* put the byte into the sink *)
      S.put_byte b.sink byte;
      (* mask out byte *)
      b.buffer <- Int64.( logand b.buffer (lognot (shift_left 255L bb)) );
      b.bufbits <- b.bufbits - 8;
      b.pos <- b.pos + 8
    done

  let pos b = b.pos + b.bufbits

end

