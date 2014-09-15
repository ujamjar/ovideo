type 'a code = 
  {
    length : int;
    code : int;
    data : 'a;
  }

type 'a table = (int * 'a) option array

let lookup (codes : 'a code list) = 
  (* max code size *)
  let max_length = List.fold_left (fun m c -> max c.length m) 0 codes in
  let size = 1 lsl max_length in
  (* table *)
  let table = Array.make size None in
  (* initialize table *)
  List.iter (fun code ->
    let zero_bits = max_length-code.length in
    let first = code.code lsl zero_bits in
    let count = 1 lsl zero_bits in
    for i = first to first + count - 1 do
      table.(i) <- Some (code.length, code.data)
    done
  ) codes;
  table, max_length


