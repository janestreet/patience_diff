(* This files comes from camlp5 (ocaml_src/lib/diff.ml). *)

(*
 * Copyright (c) 2007-2013, INRIA (Institut National de Recherches en
 * Informatique et Automatique). All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of INRIA, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software without
 *       specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY INRIA AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL INRIA AND
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
*)

(* $Id: diff.ml,v 1.2 2013-02-26 08:15:06 deraugla Exp $ *)

(* Parts of Code of GNU diff (analyze.c) translated from C to OCaml and adjusted. Basic
   algorithm described by Eugene W.Myers in: "An O(ND) Difference Algorithm and Its
   Variations" *)

open Base

exception DiagReturn of int

let diag fd bd sh xv yv xoff xlim yoff ylim = begin
  let dmin = xoff - ylim in
  let dmax = xlim - yoff in
  let fmid = xoff - yoff in
  let bmid = xlim - ylim in
  let odd = (fmid - bmid) land 1 <> 0 in
  fd.(sh+fmid) <- xoff;
  bd.(sh+bmid) <- xlim;
  try
    let rec loop fmin fmax bmin bmax = begin
      let fmin =
        if fmin > dmin then begin fd.(sh+fmin-2) <- -1; fmin - 1 end
        else fmin + 1
      in
      let fmax =
        if fmax < dmax then begin fd.(sh+fmax+2) <- -1; fmax + 1 end
        else fmax - 1
      in
      begin
        let rec loop d =
          if d < fmin then ()
          else begin
            let tlo = fd.(sh+d-1) in
            let thi = fd.(sh+d+1) in
            let x = if tlo >= thi then tlo + 1 else thi in
            let x =
              let rec loop xv yv xlim ylim x y =
                if x < xlim && y < ylim && phys_equal (xv x) (yv y) then
                  loop xv yv xlim ylim (x + 1) (y + 1)
                else x
              in
              loop xv yv xlim ylim x (x - d)
            in
            fd.(sh+d) <- x;
            if odd && bmin <= d && d <= bmax && bd.(sh+d) <= fd.(sh+d) then
              raise (DiagReturn d)
            else loop (d - 2)
          end
        in loop fmax
      end;
      let bmin =
        if bmin > dmin then begin bd.(sh+bmin-2) <- Int.max_value; bmin - 1 end
        else bmin + 1
      in
      let bmax =
        if bmax < dmax then begin bd.(sh+bmax+2) <- Int.max_value; bmax + 1 end
        else bmax - 1
      in
      begin
        let rec loop d =
          if d < bmin then ()
          else begin
            let tlo = bd.(sh+d-1) in
            let thi = bd.(sh+d+1) in
            let x = if tlo < thi then tlo else thi - 1 in
            let x =
              let rec loop xv yv xoff yoff x y =
                if x > xoff && y > yoff && phys_equal (xv (x - 1)) (yv (y - 1)) then
                  loop xv yv xoff yoff (x - 1) (y - 1)
                else x
              in
              loop xv yv xoff yoff x (x - d)
            in
            bd.(sh+d) <- x;
            if not odd && fmin <= d && d <= fmax && bd.(sh+d) <= fd.(sh+d) then
              raise (DiagReturn d)
            else loop (d - 2)
          end
        in
        loop bmax
      end;
      loop fmin fmax bmin bmax
    end in
    loop fmid fmid bmid bmid
  with DiagReturn i -> i
end

let diff_loop a ai b bi n m = begin
  let fd = Array.create ~len:(n + m + 3) 0 in
  let bd = Array.create ~len:(n + m + 3) 0 in
  let sh = m + 1 in
  let xvec i = a.(ai.(i)) in
  let yvec j = b.(bi.(j)) in
  let chng1 = Array.create ~len:(Array.length a) true in
  let chng2 = Array.create ~len:(Array.length b) true in
  for i = 0 to n - 1 do chng1.(ai.(i)) <- false done;
  for j = 0 to m - 1 do chng2.(bi.(j)) <- false done;
  let rec loop xoff xlim yoff ylim =
    let (xoff, yoff) =
      let rec loop xoff yoff =
        if xoff < xlim && yoff < ylim && phys_equal (xvec xoff) (yvec yoff) then
          loop (xoff + 1) (yoff + 1)
        else (xoff, yoff)
      in loop xoff yoff
    in
    let (xlim, ylim) =
      let rec loop xlim ylim =
        if xlim > xoff && ylim > yoff && phys_equal (xvec (xlim - 1)) (yvec (ylim - 1))
        then
          loop (xlim - 1) (ylim - 1)
        else (xlim, ylim)
      in loop xlim ylim
    in
    if xoff = xlim then
      for y = yoff to ylim - 1 do chng2.(bi.(y)) <- true done
    else if yoff = ylim then
      for x = xoff to xlim - 1 do chng1.(ai.(x)) <- true done
    else begin
      let d = diag fd bd sh xvec yvec xoff xlim yoff ylim in
      let b = bd.(sh+d) in
      loop xoff b yoff (b - d);
      loop b xlim (b - d) ylim
    end
  in loop 0 n 0 m;
  (chng1, chng2)
end

(* [make_indexer a b] returns an array of index of items of [a] which are also present in
   [b]; this way, the main algorithm can skip items which, anyway, are different. This
   improves the speed much.  The same time, this function updates the items of so that all
   equal items point to the same unique item. All items comparisons in the main algorithm
   can therefore be done with [==] instead of [=], what can improve speed much. *)
let make_indexer hashable a b = begin
  let n = Array.length a in
  let htb = Hashtbl.create hashable ~size:(10 * Array.length b) in
  Array.iteri
    ~f:(fun i e ->
      match Hashtbl.find htb e with
      | Some v ->
        b.(i) <- v
      | None ->
        Hashtbl.add_exn htb ~key:e ~data:e)
    b;
  let ai = Array.create ~len:n 0 in
  let k =
    let rec loop i k =
      if i = n then k
      else
        let k =
          match Hashtbl.find htb a.(i) with
          | Some v ->
            a.(i) <- v;
            ai.(k) <- i;
            k + 1
          | None ->
            k
        in
        loop (i + 1) k
    in loop 0 0
  in
  Array.sub ai ~pos:0 ~len:k
end

let f ~hashable a b =
  let ai = make_indexer hashable a b in
  let bi = make_indexer hashable b a in
  let n = Array.length ai in
  let m = Array.length bi in
  diff_loop a ai b bi n m

let iter_matches ~f:ff ~hashable a b =
  let d1, d2 = f ~hashable a b in
  let rec aux i1 i2 =
    if i1 >= Array.length d1 || i2 >= Array.length d2 then ()
    else
    if not d1.(i1) then
      if not d2.(i2) then begin
        ff (i1, i2);
        aux (i1 + 1) (i2 + 1)
      end else aux i1 (i2 + 1)
    else
    if not d2.(i2) then aux (i1 + 1) i2
    else aux (i1 + 1) (i2 + 1)
  in
  aux 0 0
