(* TODO:
  * other algotihms
  * abstract over metrics (sad, sse, rdo etc)
 *)

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

module Sad = struct
  type t = int
  let init = max_int
  let compare = compare

  let metric
    ~bw ~bh 
    ~cur ~cx ~cy 
    ~ref ~mx ~my 
    = 
    let sad = Pervasives.ref 0 in
    for j=0 to bh-1 do
      for i=0 to bw-1 do
        sad := !sad + abs ( cur.{cy+j,cx+i} - ref.{my+j,mx+i} )
      done
    done;
    !sad

  let metric_umv 
    ~bw ~bh 
    ~cur ~cx ~cy 
    ~ref ~mx ~my 
    = 
    let w, h = Frame.U8.Plane.(width ref - 1, height ref - 1) in
    let sad = Pervasives.ref 0 in
    let ref y x = 
      let clip x n = max 0 (min x n) in
      ref.{ clip y h, clip x w }
    in
    for j=0 to bh-1 do
      for i=0 to bw-1 do
        sad := !sad + abs ( cur.{cy+j,cx+i} - ref (my+j) (mx+i) )
      done
    done;
    !sad
end

module Sse = struct
  type t = int
  let init = max_int
  let compare = compare

  let metric
    ~bw ~bh 
    ~cur ~cx ~cy 
    ~ref ~mx ~my 
    = 
    let sse = Pervasives.ref 0 in
    for j=0 to bh-1 do
      for i=0 to bw-1 do
        let d = cur.{cy+j,cx+i} - ref.{my+j,mx+i} in
        sse := !sse + (d * d)
      done
    done;
    !sse

  let metric_umv 
    ~bw ~bh 
    ~cur ~cx ~cy 
    ~ref ~mx ~my 
    = 
    let w, h = Frame.U8.Plane.(width ref - 1, height ref - 1) in
    let sse = Pervasives.ref 0 in
    let ref y x = 
      let clip x n = max 0 (min x n) in
      ref.{ clip y h, clip x w }
    in
    for j=0 to bh-1 do
      for i=0 to bw-1 do
        let d = cur.{cy+j,cx+i} - ref (my+j) (mx+i) in
        sse := !sse + (d * d)
      done
    done;
    !sse
end

type 'a mv_bounds = 
  ref:Frame.U8.Plane.t -> bw:int -> bh:int -> 
  mx:int -> my:int -> 'a

let mv_in_bounds ~ref ~bw ~bh = 
  let max_mx, max_my = Frame.U8.Plane.(width ref - bw, height ref - bh) in
  (fun ~mx ~my -> mx >= 0 && mx < max_mx && my >= 0 && my < max_my)

let mv_in_bounds_umv ~ref ~bw ~bh = 
  let w, h = Frame.U8.Plane.(width ref, height ref) in
  (fun ~mx ~my -> mx > (-bw) && mx < w && my > (-bh) && my < h)

let mv_clip_bounds ~ref ~bw ~bh = 
  let w, h = Frame.U8.Plane.(width ref - 1 - bw, height ref - 1 - bh) in
  (fun ~mx ~my -> max 0 (min w mx), max 0 (min h my))

let mv_clip_bounds_umv ~ref ~bw ~bh = 
  let bw, bh = 1-bw,1-bh in
  let w, h = Frame.U8.Plane.(width ref - 1, height ref - 1) in
  (fun ~mx ~my -> max bw (min w mx), max bh (min h my))

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

module Make(M : Metric) = struct

  type t = M.t

  let logsearch 
    (* search parameters *)
    ~steps
    (* block size *)
    ~bw ~bh
    (* enable umv *)
    ~umv
    (* reference frames *)
    ~cur ~ref =
    let best_mx, best_my, best_metric = Pervasives.(ref 0, ref 0, ref M.init) in
    let patterns = 
      [| (-1,-1); (0,-1); (1,-1);
         (-1, 0);         (1, 0);
         (-1, 1); (0, 1); (1, 1); |] 
    in

    (* current block location *)
    let search ~cx ~cy =
      let metric = M.metric ~bw ~bh ~cur ~cx ~cy ~ref in
      let metric_umv = M.metric_umv ~bw ~bh ~cur ~cx ~cy ~ref in
      let mv_in_bounds = mv_in_bounds ~ref ~bw ~bh in
      let mv_clip_bounds = mv_clip_bounds ~ref ~bw ~bh in
      let mv_clip_bounds_umv = mv_clip_bounds_umv ~ref ~bw ~bh in

      (* search start position *)
      let search ~sx ~sy = 
        let check ~mx ~my = 
            (* check mv is fully in bounds of the reference frame *)
            if mv_in_bounds ~mx ~my then begin
              let metric = metric ~mx ~my in
              if M.compare metric !best_metric < 0 then begin
                best_metric := metric;
                best_mx := mx;
                best_my := my;
              end
            end
            (* if umv is enabled *)
            else if umv then begin
              let mx, my = mv_clip_bounds_umv ~mx ~my in
              let metric = metric_umv ~mx ~my in
              if M.compare metric !best_metric < 0 then begin
                best_metric := metric;
                best_mx := mx;
                best_my := my;
            end
            (* clip bounds to within frame*)
            end else begin
              let mx, my = mv_clip_bounds ~mx ~my in
              let metric = metric ~mx ~my in
              if M.compare metric !best_metric < 0 then begin
                best_metric := metric;
                best_mx := mx;
                best_my := my;
              end
            end
        in

        let step ~range ~sx ~sy = 
          for i=0 to Array.length patterns - 1 do
            let ox, oy = patterns.(i) in
            let mx, my = sx + (ox*range), sy + (oy*range) in
            check ~mx ~my
          done
        in

        (* set origin of search *)
        best_mx := sx;
        best_my := sy;
        best_metric := M.init;
        check ~mx:sx ~my:sy;

        let range = 1 lsl (steps-1) in
        let rec loop ~range ~sx ~sy = 
          if range=0 then 
            {
              mx = !best_mx - sx;
              my = !best_my - sy;
              metric = !best_metric;
            }
          else begin
            step ~range ~sx ~sy;
            loop ~range:(range/2) ~sx:(!best_mx) ~sy:(!best_my)
          end
        in
        loop ~range ~sx ~sy 
      in
      search
    in
    search

  let fullsearch 
    (* search parameters *)
    ~window
    (* block size *)
    ~bw ~bh
    (* enable umv *)
    ~umv
    (* reference frames *)
    ~cur ~ref =
    let wx,wy = window in
    let best_mx, best_my, best_metric = Pervasives.(ref 0, ref 0, ref M.init) in
    
    (* current block location *)
    let search ~cx ~cy =
      let metric = M.metric ~bw ~bh ~cur ~cx ~cy ~ref in
      let metric_umv = M.metric_umv ~bw ~bh ~cur ~cx ~cy ~ref in
      let mv_in_bounds = mv_in_bounds ~ref ~bw ~bh in
      let mv_in_bounds_umv = mv_in_bounds_umv ~ref ~bw ~bh in

      (* search start position *)
      let search ~sx ~sy = 
        best_mx := 0;
        best_my := 0;
        best_metric := M.init;
        for my=(sy-wy) to (sy+wy) do
          for mx=(sx-wx) to (sx+wx) do
            (* check mv is fully in bounds of the reference frame *)
            if mv_in_bounds ~mx ~my then begin
              let metric = metric ~mx ~my in
              if M.compare metric !best_metric < 0 then begin
                best_metric := metric;
                best_mx := mx;
                best_my := my;
              end
            end
            (* if umv is enabled, check if the search block is at least
            * partially within the frame *)
            else if umv && mv_in_bounds_umv ~mx ~my then begin
              let metric = metric_umv ~mx ~my in
              if M.compare metric !best_metric < 0 then begin
                best_metric := metric;
                best_mx := mx;
                best_my := my;
              end
            end
          done
        done;
        {
          mx = !best_mx - sx;
          my = !best_my - sy;
          metric = !best_metric;
        }
      in
      search
    in
    search

end

module Search_sad = Make(Sad)
module Search_sse = Make(Sse)

