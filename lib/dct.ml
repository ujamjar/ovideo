module Chen = struct

  (* ******************************************************* *)
  (* idct *)
  (* ******************************************************* *)
  let idct din dout = 

    let w1 = 2841 in (* 2048*sqrt(2)*cos(1*pi/16) *)
    let w2 = 2676 in (* 2048*sqrt(2)*cos(2*pi/16) *)
    let w3 = 2408 in (* 2048*sqrt(2)*cos(3*pi/16) *)
    let w5 = 1609 in (* 2048*sqrt(2)*cos(5*pi/16) *)
    let w6 = 1108 in (* 2048*sqrt(2)*cos(6*pi/16) *)
    let w7 = 565  in (* 2048*sqrt(2)*cos(7*pi/16) *)  

    let idct_row row din dout = 
    
      let x0 =(din.{row, 0} lsl 11) + 128 in (* for proper rounding in the fourth stage *)
      let x1 = din.{row, 4} lsl 11 in
      let x2 = din.{row, 6} in  
      let x3 = din.{row, 2} in  
      let x4 = din.{row, 1} in  
      let x5 = din.{row, 7} in  
      let x6 = din.{row, 5} in  
      let x7 = din.{row, 3} in

      (* first stage *)
      let x8 = w7 * (x4 + x5) in
      let x4 = x8 + (w1 - w7) * x4 in
      let x5 = x8 - (w1 + w7) * x5 in
      let x8 = w3 * (x6 + x7) in
      let x6 = x8 - (w3 - w5) * x6 in
      let x7 = x8 - (w3 + w5) * x7 in
      
      (* second stage *)
      let x8 = x0 + x1 in
      let x0 = x0 - x1 in
      let x1 = w6 * (x3 + x2) in
      let x2 = x1 - (w2 + w6) * x2 in
      let x3 = x1 + (w2 - w6) * x3 in
      let x1 = x4 + x6 in
      let x4 = x4 - x6 in
      let x6 = x5 + x7 in
      let x5 = x5 - x7 in
      
      (* third stage *)
      let x7 = x8 + x3 in
      let x8 = x8 - x3 in
      let x3 = x0 + x2 in
      let x0 = x0 - x2 in
      let x2 = (181 * (x4 + x5) + 128) asr 8 in
      let x4 = (181 * (x4 - x5) + 128) asr 8 in
      
      (* fourth stage *)
      dout.{row, 0} <- (x7 + x1) asr 8;
      dout.{row, 1} <- (x3 + x2) asr 8;
      dout.{row, 2} <- (x0 + x4) asr 8;
      dout.{row, 3} <- (x8 + x6) asr 8;
      dout.{row, 4} <- (x8 - x6) asr 8;
      dout.{row, 5} <- (x0 - x4) asr 8;
      dout.{row, 6} <- (x3 - x2) asr 8;
      dout.{row, 7} <- (x7 - x1) asr 8
    in

    let idct_col col din dout = 
    
      let x0 =(din.{0, col} lsl 8) + 8192 in
      let x1 = din.{4, col} lsl 8  in
      let x2 = din.{6, col} in
      let x3 = din.{2, col} in
      let x4 = din.{1, col} in
      let x5 = din.{7, col} in
      let x6 = din.{5, col} in
      let x7 = din.{3, col} in
                    
      (* first stage *)
      let x8 =  w7 * (x4 + x5) + 4 in
      let x4 = (x8 + (w1 - w7) * x4) asr 3 in
      let x5 = (x8 - (w1 + w7) * x5) asr 3 in
      let x8 =  w3 * (x6 + x7) + 4 in
      let x6 = (x8 - (w3 - w5) * x6) asr 3 in
      let x7 = (x8 - (w3 + w5) * x7) asr 3 in
      
      (* second stage *)
      let x8 = x0 + x1 in
      let x0 = x0 - x1 in
      let x1 = w6 * (x3 + x2) + 4 in
      let x2 = (x1 - (w2 + w6) * x2) asr 3 in
      let x3 = (x1 + (w2 - w6) * x3) asr 3 in
      let x1 = x4 + x6 in
      let x4 = x4 - x6 in
      let x6 = x5 + x7 in
      let x5 = x5 - x7 in
      
      (* third stage *)
      let x7 = x8 + x3 in
      let x8 = x8 - x3 in
      let x3 = x0 + x2 in
      let x0 = x0 - x2 in
      let x2 = (181 * (x4 + x5) + 128) asr 8 in
      let x4 = (181 * (x4 - x5) + 128) asr 8 in
      
      (* fourth stage *)
      dout.{0, col} <- (x7 + x1) asr 14;
      dout.{1, col} <- (x3 + x2) asr 14;
      dout.{2, col} <- (x0 + x4) asr 14;
      dout.{3, col} <- (x8 + x6) asr 14;
      dout.{4, col} <- (x8 - x6) asr 14;
      dout.{5, col} <- (x0 - x4) asr 14;
      dout.{6, col} <- (x3 - x2) asr 14;
      dout.{7, col} <- (x7 - x1) asr 14
    in

    for i=0 to 7 do idct_row i din dout done;
    for i=0 to 7 do idct_col i dout dout done


  (* ******************************************************* *)
  (* dct *)
  (* ******************************************************* *)
  let fdct din dout = 

    let c4  (f, g) = ( 362 * (f + g)) asr 9 in
    let c62 (f, g) = ((196 * f) + (473 * g)) asr 9 in
    let c71 (f, g) = ((100 * f) + (502 * g)) asr 9 in
    let c35 (f, g) = ((426 * f) + (284 * g)) asr 9 in

    let dct_col col din dout = 

      let a0 = din.{0, col} + din.{7, col} in
      let c3 = din.{0, col} - din.{7, col} in

      let a1 = din.{1, col} + din.{6, col} in
      let c2 = din.{1, col} - din.{6, col} in

      let a2 = din.{2, col} + din.{5, col} in
      let c1 = din.{2, col} - din.{5, col} in

      let a3 = din.{3, col} + din.{4, col} in
      let c0 = din.{3, col} - din.{4, col} in

      let b0 = a0 + a3 in
      let b1 = a1 + a2 in
      let b2 = a1 - a2 in
      let b3 = a0 - a3 in

      dout.{0, col} <- c4(b0,b1);
      dout.{4, col} <- c4(b0,-b1);
      dout.{2, col} <- c62(b2,b3);
      dout.{6, col} <- c62(b3,-b2);

      let b0 = c4(c2,-c1) in
      let b1 = c4(c2,c1) in

      let a0 = c0 + b0 in
      let a1 = c0 - b0 in
      let a2 = c3 - b1 in
      let a3 = c3 + b1 in

      dout.{1, col} <- c71(a0,a3);
      dout.{5, col} <- c35(a1,a2);
      dout.{3, col} <- c35(a2,-a1);
      dout.{7, col} <- c71(a3,-a0)
    in

    let dct_row row din dout = 

      let a0 = din.{row, 0} + din.{row, 7} in
      let c3 = din.{row, 0} - din.{row, 7} in

      let a1 = din.{row, 1} + din.{row, 6} in
      let c2 = din.{row, 1} - din.{row, 6} in

      let a2 = din.{row, 2} + din.{row, 5} in
      let c1 = din.{row, 2} - din.{row, 5} in

      let a3 = din.{row, 3} + din.{row, 4} in
      let c0 = din.{row, 3} - din.{row, 4} in

      let b0 = a0 + a3 in
      let b1 = a1 + a2 in
      let b2 = a1 - a2 in
      let b3 = a0 - a3 in

      dout.{row, 0} <- c4(b0,b1);
      dout.{row, 4} <- c4(b0,-b1);
      dout.{row, 2} <- c62(b2,b3);
      dout.{row, 6} <- c62(b3,-b2);

      let b0 = c4(c2,-c1) in
      let b1 = c4(c2,c1) in

      let a0 = c0 + b0 in
      let a1 = c0 - b0 in
      let a2 = c3 - b1 in
      let a3 = c3 + b1 in

      dout.{row, 1} <- c71(a0,a3);
      dout.{row, 5} <- c35(a1,a2);
      dout.{row, 3} <- c35(a2,-a1);
      dout.{row, 7} <- c71(a3,-a0)
    in

    for i=0 to 7 do dct_col i din dout done;
    for i=0 to 7 do dct_row i dout dout done

end


