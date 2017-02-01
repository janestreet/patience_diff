open Core_kernel
open Core_kernel.Int.Replace_polymorphic_compare

let ( <|> ) ar (i,j) =
  if j <= i then [||]
  else Array.slice ar i j

module Ordered_sequence : sig
  type elt = int * int [@@deriving compare]
  (* A [t] has its second coordinates in increasing order *)
  type t = private elt array [@@deriving sexp_of]

  val create : (int * int) list -> t

  val is_empty : t -> bool
end = struct
  type elt = int * int [@@deriving sexp_of]

  let compare_elt = Comparable.lexicographic [
    (fun (_,y0) (_,y1) -> Int.compare y0 y1);
    (fun (x0,_) (x1,_) -> Int.compare x0 x1);
  ]

  type t = elt array [@@deriving sexp_of]
  let create l =
    let t = Array.of_list l in
    Array.sort t ~cmp:compare_elt;
    t

  let is_empty = Array.is_empty
end

(* This is an implementation of the patience sorting algorithm as explained at
   http://en.wikipedia.org/wiki/Patience_sorting *)
module Patience : sig
  val longest_increasing_subsequence : Ordered_sequence.t -> (int * int) list
end = struct

  module Pile = struct
    type 'a t = 'a Stack.t
    let create x = let t = Stack.create () in Stack.push t x; t
    let top t = Stack.top t |> Option.value_exn
    let put_on_top t x = Stack.push t x
  end

  module Piles = struct
    type 'a t = 'a Pile.t Deque.t

    let empty () : 'a t = Deque.create ~never_shrink:true ()

    let get_ith_pile t i dir =
      let get index offset =
        Option.bind (index t) ~f:(fun index -> Deque.get_opt t (index + offset))
      in
      match dir with
      | `From_left  -> get Deque.front_index i
      | `From_right -> get Deque.back_index (-i)

    let new_rightmost_pile t pile = Deque.enqueue_back t pile
  end

  module Backpointers = struct
    (* in the terminology of the Wikipedia article, this corresponds to a card together
       with its backpointers *)
    type 'a tag = 'a t
    and 'a t =
      { value : 'a
      ; tag   : 'a tag option
      }

    let to_list t =
      let rec to_list acc t =
        match t.tag with
        | None -> t.value::acc
        | Some t' -> to_list (t.value::acc) t'
      in
      to_list [] t
  end

  module Play_patience : sig
    val play_patience
      :  Ordered_sequence.t
      -> get_tag:(
        pile_opt:int option
        -> piles:Ordered_sequence.elt Backpointers.t Piles.t
        -> Ordered_sequence.elt Backpointers.tag option
      )
      -> Ordered_sequence.elt Backpointers.t Piles.t
  end = struct
    let optimized_findi_from_left piles x =
      let (>>=) = Option.(>>=) in
      (* first see if any work *)
      let last_pile = Piles.get_ith_pile piles 0 `From_right in
      (* [x_pile] is a dummy pile just used for comparisons *)
      let x_pile = Pile.create {Backpointers.value=(x,0); tag = None} in
      let compare_top_values pile1 pile2 =
        let top pile = fst (Pile.top pile).Backpointers.value in
        Int.compare (top pile1) (top pile2)
      in
      last_pile >>= fun last_pile ->
      if compare_top_values last_pile x_pile < 0 then None
      else
        (* do binary search *)
        Deque.binary_search piles `First_strictly_greater_than x_pile
          ~compare:compare_top_values
    ;;
    (* [play_patience ar ~get_tag] plays patience with the greedy algorithm as described
       in the Wikipedia article, taking [ar] to be the deck of cards.  It returns the
       resulting [Piles.t].  Before putting an element of [ar] in a pile, it tags it using
       [get_tag].  [get_tag] takes as its arguments the full [Piles.t] in its current
       state, and also the specific [Pile.t] that the element of [ar] is being added to.
    *)
    let play_patience ar ~get_tag =
      let ar = (ar : Ordered_sequence.t :> Ordered_sequence.elt array) in
      if Array.length ar = 0 then raise (Invalid_argument "Patience_diff.play_patience");
      let piles = Piles.empty () in
      Array.iter ar ~f:(fun x ->
        let pile_opt = optimized_findi_from_left piles  (fst x) in
        let tagged_x = {Backpointers.value = x;tag = get_tag ~pile_opt ~piles} in
        match pile_opt with
        | None -> Piles.new_rightmost_pile piles (Pile.create tagged_x)
        | Some i -> let pile = Deque.get piles i in Pile.put_on_top pile tagged_x
      );
      piles
  end

  let longest_increasing_subsequence ar =
    if Ordered_sequence.is_empty ar then [] else begin
      let module P = Play_patience in
      let get_tag ~pile_opt ~piles =
        match pile_opt with
        | None -> Piles.get_ith_pile piles 0 `From_right |> Option.map ~f:Pile.top
        | Some i ->
          if i = 0 then None
          else Piles.get_ith_pile piles (i-1) `From_left |> Option.value_exn |> Pile.top |> Option.some
      in
      let piles = P.play_patience ar ~get_tag in
      Piles.get_ith_pile piles 0 `From_right |> Option.value_exn |> Pile.top |>
      Backpointers.to_list
    end
end

let compare_int_pair = Tuple.T2.compare ~cmp1:Int.compare ~cmp2:Int.compare

let _longest_increasing_subsequence ar =
  let ar = (ar : Ordered_sequence.t :> (int * int) array) in
  let len = Array.length ar in
  if len <= 1 then
    Array.to_list ar
  else begin
    let maxlen = ref 0 in
    let m = Array.create ~len:(len + 1) (-1) in
    let pred = Array.create ~len:(len + 1) (-1) in
    for i = 0 to len - 1 do
      let p =
        Array.binary_search ~compare:Ordered_sequence.compare_elt
          ar
          `First_greater_than_or_equal_to ar.(i) ~len:(max (!maxlen - 1) 0) ~pos:1
        |> Option.value ~default:0
      in
      pred.(i) <- m.(p);
      if (p = !maxlen) || (compare_int_pair ar.(i) ar.(p + 1) < 0) then begin
        m.(p + 1) <- i;
        if (p + 1) > !maxlen then maxlen := p + 1;
      end;
    done;
    let rec loop ac p =
      if p = (-1) then ac
      else
        loop (ar.(p) :: ac) pred.(p)
    in
    loop [] m.(!maxlen)
  end
;;

module Matching_block = struct
  type t =
    { mine_start  : int
    ; other_start : int
    ; length      : int
    }
end

module Range = struct
  type 'a t =
    | Same of ('a * 'a) array
    | Old of 'a array
    | New of 'a array
    | Replace of 'a array * 'a array
    | Unified of 'a array
  [@@deriving sexp]

  let all_same ranges =
    List.for_all ranges ~f:(fun range ->
      match range with
      | Same _ -> true
      | _ -> false)
  ;;

  let old_only ranges =
    let f = function
      | Replace (l_range, _) -> [Old l_range]
      | New _ -> []
      | range -> [range]
    in
    List.concat_map ranges ~f
  ;;

  let new_only ranges =
    let f = function
      | Replace (_, r_range) -> [New r_range]
      | Old _ -> []
      | range -> [range]
    in
    List.concat_map ranges ~f
  ;;
end

module Hunk = struct
  type 'a t =
    { mine_start  : int
    ; mine_size   : int
    ; other_start : int
    ; other_size  : int
    ; ranges      : 'a Range.t list
    }
  [@@deriving sexp_of]

  (* Does the nitty gritty of turning indexes into
     line numbers and reversing the ranges, returning a nice new hunk *)
  let create mine_start mine_stop other_start other_stop ranges =
    { mine_start = mine_start + 1
    ; mine_size = mine_stop - mine_start
    ; other_start = other_start + 1
    ; other_size = other_stop - other_start
    ; ranges = List.rev ranges
    }

  let all_same hunk = Range.all_same hunk.ranges
end

module Hunks = struct
  type 'a t = 'a Hunk.t list

  let concat_map_ranges hunks ~f =
    let f hunk =
      { hunk with
        Hunk.ranges = List.concat_map hunk.Hunk.ranges ~f
      }
    in
    List.map hunks ~f
  ;;

  let unified hunks =
    let module R = Range in
    let f = function
      | R.Replace (l_range, r_range) -> [R.Old l_range; R.New r_range]
      | range -> [range]
    in
    concat_map_ranges hunks ~f
  ;;

  let ranges hunks =
    List.concat_map hunks ~f:(fun hunk -> hunk.Hunk.ranges)
  ;;
end

module type S = sig
  type elt

  val get_matching_blocks
    :  transform: ('a -> elt)
    -> mine:'a array
    -> other:'a array
    -> Matching_block.t list

  val matches : elt array -> elt array -> (int * int) list

  val match_ratio : elt array -> elt array -> float

  val get_hunks
    :  transform: ('a -> elt)
    -> context: int
    -> mine: 'a array
    -> other: 'a array
    -> 'a Hunk.t list

  type 'a segment =
    | Same of 'a array
    | Different of 'a array array

  type 'a merged_array = 'a segment list

  val merge : elt array array -> elt merged_array
end

module Make (Elt : Hashtbl.Key) = struct

  module Table = Hashtbl.Make(Elt)

  type elt = Elt.t

  (* This is an implementation of the patience diff algorithm by Bram Cohen as seen in
     Bazaar version 1.14.1 *)

  let unique_lcs (alpha,alo,ahi) (bravo,blo,bhi) =
    (* Create a hashtable which takes elements of a to their index in a iff they're
       unique. *)
    let unique = Table.create ~size:(Int.min (ahi - alo) (bhi - blo)) () in
    for x's_pos_in_a = alo to ahi - 1 do
      let x = alpha.(x's_pos_in_a) in
      match Hashtbl.find unique x with
      | None -> Hashtbl.set unique ~key:x ~data:(`Unique_in_a x's_pos_in_a)
      | Some _ -> Hashtbl.set unique ~key:x ~data:`Not_unique
    done;

    for x's_pos_in_b = blo to bhi - 1 do
      let x = bravo.(x's_pos_in_b) in
      Hashtbl.find unique x |> Option.iter ~f:(fun pos ->
        match pos with
        | `Not_unique -> ()
        | `Unique_in_a x's_pos_in_a ->
          Hashtbl.set unique
            ~key:x ~data:(`Unique_in_a_b (x's_pos_in_a, x's_pos_in_b))
        | `Unique_in_a_b _ ->
          Hashtbl.set unique ~key:x ~data:`Not_unique);
    done;
    let a_b =
      let unique = Hashtbl.filter_map unique ~f:(function
        | `Not_unique
        | `Unique_in_a _ -> None
        | `Unique_in_a_b pos_in_a_b -> Some pos_in_a_b)
      in
      Ordered_sequence.create (Hashtbl.data unique)
    in
    Patience.longest_increasing_subsequence a_b
  ;;

  (* [matches a b] returns a list of pairs (i,j) such that a.(i) = b.(j) and such that
     the list is strictly increasing in both its first and second coordinates.

     This is done by first applying unique_lcs to find matches from a to b among those
     elements which are unique in both a and b, and then recursively applying [matches] to
     each subinterval determined by those matches.  The uniqueness requirement is waived
     for blocks of matching lines at the beginning or end.

     I couldn't figure out how to do this efficiently in a functional way, so
     this is pretty much a straight translation of the original Python code. *)
  let matches alpha bravo =
    let matches_ref_length = ref 0 in
    let matches_ref = ref [] in
    let add_match m =
      incr matches_ref_length;
      matches_ref := m :: !matches_ref
    in
    let rec recurse_matches alo blo ahi bhi =
      (*    printf "alo %d blo %d ahi %d bhi %d\n%!" alo blo ahi bhi; *)
      let old_length = !matches_ref_length in
      if not (alo >= ahi || blo >= bhi) then begin
        let last_a_pos = ref (alo - 1) in
        let last_b_pos = ref (blo - 1) in
        unique_lcs (alpha,alo,ahi) (bravo,blo,bhi)
        |> List.iter ~f:(fun (apos,bpos) ->
          (*           printf "found apos %d bpos %d\n%!" apos bpos; *)
          if !last_a_pos + 1 <> apos || !last_b_pos + 1 <> bpos
          then begin
            (*printf "recurse last_a_pos %d last_b_pos %d\n%!" !last_a_pos !last_b_pos;*)
            recurse_matches (!last_a_pos + 1) (!last_b_pos + 1) apos bpos;
          end;
          last_a_pos := apos;
          last_b_pos := bpos;
          add_match (apos,bpos));
        if !matches_ref_length > old_length
        then recurse_matches (!last_a_pos+1) (!last_b_pos+1) ahi bhi
        else if (Elt.compare alpha.(alo) bravo.(blo) = 0)
        then
          begin
            let alo = ref alo in
            let blo = ref blo in
            while
              (!alo < ahi && !blo < bhi
               && (Elt.compare alpha.(!alo) bravo.(!blo) = 0))
            do
              add_match (!alo,!blo);
              incr alo; incr blo;
            done;
            recurse_matches !alo !blo ahi bhi;
          end
        else if (Elt.compare alpha.(ahi - 1) bravo.(bhi - 1) = 0)
        then
          begin
            let nahi = ref (ahi - 1) in
            let nbhi = ref (bhi - 1) in
            while (!nahi > alo && !nbhi > blo
                   && Elt.compare alpha.(!nahi-1) bravo.(!nbhi - 1) = 0)
            do
              decr nahi; decr nbhi;
            done;
            recurse_matches (!last_a_pos+1) (!last_b_pos+1) !nahi !nbhi;
            for i = 0 to (ahi - !nahi - 1) do
              add_match (!nahi + i,!nbhi + i)
            done;
          end
        else
          Plain_diff.iter_matches
            (Array.sub alpha ~pos:alo ~len:(ahi - alo))
            (Array.sub bravo ~pos:blo ~len:(bhi - blo))
            ~f:(fun (i1, i2) -> add_match (alo + i1, blo + i2))
      end
    in
    recurse_matches 0 0 (Array.length alpha) (Array.length bravo);
    List.rev !matches_ref
  ;;

  let collapse_sequences matches =
    let collapsed = ref [] in
    let start_a = ref None in
    let start_b = ref None in
    let length = ref 0 in
    List.iter matches ~f:(fun (i_a,i_b) ->
      begin
        match !start_a, !start_b with
        | Some start_a_val , Some start_b_val when
            (i_a = start_a_val + !length && i_b = start_b_val + !length) -> incr length
        | _ -> begin
            begin
              match !start_a, !start_b with
              | Some start_a_val, Some start_b_val ->
                let matching_block = {
                  Matching_block.
                  mine_start = start_a_val;
                  other_start = start_b_val;
                  length = !length;
                }
                in
                collapsed := matching_block::!collapsed
              | _ -> ()
            end;
            start_a := Some i_a;
            start_b := Some i_b;
            length := 1
          end
      end);
    begin
      match !start_a, !start_b with
      | Some start_a_val, Some start_b_val when !length <> 0 ->
        let matching_block = {
          Matching_block.
          mine_start = start_a_val;
          other_start = start_b_val;
          length = !length;
        }
        in
        collapsed := matching_block :: !collapsed
      | _ -> ()
    end;
    List.rev !collapsed

  let get_matching_blocks ~transform ~mine ~other =
    let mine = Array.map mine ~f:transform in
    let other = Array.map other ~f:transform in
    let matches = matches mine other |> collapse_sequences in
    let last_match =
      { Matching_block.
        mine_start  = Array.length mine
      ; other_start = Array.length other
      ; length = 0
      }
    in
    List.append matches [last_match]

  let get_ranges_rev ~transform ~mine ~other =
    let module R = Range in
    let module M = Matching_block in
    let rec aux matching_blocks i j l =
      match matching_blocks with
      | current_block :: remaining_blocks -> (
          let mine_index, other_index, size =
            current_block.M.mine_start, current_block.M.other_start,
            current_block.M.length
          in
          (* Throw away crossover matches *)
          if mine_index < i || other_index < j then
            aux remaining_blocks i j l
          else
            let range_opt = (
              if i < mine_index && j < other_index then
                let mine_range = mine <|> (i, mine_index) in
                let other_range = other <|> (j, other_index) in
                Some (R.Replace (mine_range, other_range))
              else if i < mine_index then
                let mine_range = mine <|> (i, mine_index) in
                Some (R.Old mine_range)
              else if j < other_index then
                let other_range = other <|> (j, other_index) in
                Some (R.New other_range)
              else None)
            in
            let l = match range_opt with
              | Some range -> range :: l
              | None -> l
            in
            let mine_stop, other_stop = mine_index + size, other_index + size in
            let l =
              if size = 0 then l
              else
                let mine_range = mine <|> (mine_index, mine_stop) in
                let other_range = other <|> (other_index, other_stop) in
                let range = Array.map2_exn mine_range other_range
                              ~f:(fun x y -> (x, y)) in
                R.Same range :: l
            in
            aux remaining_blocks mine_stop other_stop l
        )
      | [] -> List.rev l
    in
    let matching_blocks = get_matching_blocks ~transform ~mine ~other in
    aux matching_blocks 0 0 []

  let get_hunks ~transform ~context ~mine ~other =
    let ranges = get_ranges_rev ~transform ~mine ~other in
    let module R = Range in
    let a = mine in
    let b = other in
    if context < 0 then
      let singleton_hunk =
        Hunk.create 0 (Array.length a) 0 (Array.length b) (List.rev ranges) in
      [singleton_hunk]
    else
      let rec aux ranges_remaining curr_ranges alo ahi blo bhi acc_hunks =
        match ranges_remaining with
        | [] ->
          (* Finish the last hunk *)
          let new_hunk = Hunk.create alo ahi blo bhi curr_ranges in
          (* Add it to the accumulator *)
          let acc_hunks = new_hunk :: acc_hunks in
          (* Finished! Return the accumulator *)
          List.rev acc_hunks
        | R.Same range :: [] ->
          (* If the last range is a Same, we might need to crop to context. *)
          let stop = min (Array.length range) (context) in
          let new_range = R.Same (range <|> (0, stop)) in
          let curr_ranges = new_range :: curr_ranges in
          (* Finish the current hunk *)
          let ahi = ahi + stop in
          let bhi = bhi + stop in
          let new_hunk = Hunk.create alo ahi blo bhi curr_ranges in
          (* Add it to the accumulator *)
          let acc_hunks = new_hunk :: acc_hunks in
          (* Finished! Return the accumulator *)
          List.rev acc_hunks
        | R.Same range :: rest ->
          let size = Array.length range in
          if size > context * 2 then (
            (* If this Same range is sufficiently large, split off a new hunk *)
            let new_range = R.Same (range <|> (0, context)) in
            let curr_ranges = new_range :: curr_ranges in
            (* Advance both hi's by context *)
            let ahi = ahi + context in
            let bhi = bhi + context in
            (* Finish the current hunk *)
            let new_hunk = Hunk.create alo ahi blo bhi curr_ranges in
            (* Add it to the accumulator *)
            let acc_hunks = new_hunk :: acc_hunks in
            (* Calculate ranges for the next hunk *)
            let alo = ahi + size - 2 * context in
            let ahi = alo in
            let blo = bhi + size - 2 * context in
            let bhi = blo in
            (* Push the remainder of the Equal range back onto the remaining_ranges *)
            let rest = R.Same (range <|> (size - context, size)) :: rest in
            aux rest [] alo ahi blo bhi acc_hunks
          ) else (
            (* Otherwise, this range is small enough that it qualifies as context for
               the both the previous and forthcoming range, so simply add it to
               curr_ranges untouched *)
            let curr_ranges = R.Same range :: curr_ranges in
            let ahi = ahi + size in
            let bhi = bhi + size in
            aux rest curr_ranges alo ahi blo bhi acc_hunks
          )
        | range :: rest ->
          (* Any range that isn't an Equal is important and not just context, so keep
             it in curr_ranges *)
          let curr_ranges = range :: curr_ranges in
          (* rest could be anything, so extract hunk_info from range *)
          let ahi, bhi =
            match range with
            | R.Same _ ->
              (* We eliminate the possibility of a Same above *)
              assert false
            | R.Unified _ ->
              (* get_ranges_rev never returns a Unified range *)
              assert false
            | R.New range ->
              let stop = bhi + (Array.length range) in
              (ahi, stop)
            | R.Old range ->
              let stop = ahi + (Array.length range) in
              (stop, bhi)
            | R.Replace (a_range, b_range) ->
              let mine_stop = ahi + Array.length a_range in
              let other_stop = bhi + Array.length b_range in
              (mine_stop, other_stop)
          in
          aux rest curr_ranges alo ahi blo bhi acc_hunks
      in
      let ranges, alo, ahi, blo, bhi =
        match ranges with
        (* If the first range is an Equal, shave off the front of the range, according to
           context.  Keep it on the ranges list so hunk construction can see where the range
           begins *)
        | R.Same range :: rest ->
          let stop = Array.length range in
          let start = max 0 (stop - context) in
          let new_range = R.Same (range <|> (start, stop)) in
          (new_range :: rest, start, start, start, start)
        | rest -> (rest, 0, 0, 0, 0)
      in
      aux ranges [] alo ahi blo bhi []
  ;;

  let match_ratio a b =
    (matches a b |> List.length |> ( * ) 2 |> float)
    /. (Array.length a + Array.length b |> float)
  ;;

  let collapse_multi_sequences matches =
    let collapsed = ref [] in
    let value_exn x = Option.value_exn x in
    if List.is_empty matches then [] else
      let start = Array.create ~len:(List.length (List.hd_exn matches)) None in
      let length = ref 0 in
      List.iter matches ~f:(fun il ->
        begin
          if Array.for_all start ~f:Option.is_some && (
            List.mapi il ~f:(fun i x -> x = value_exn start.(i) + !length)
            |> List.for_all ~f:(fun x -> x))
          then incr length
          else begin if Array.for_all start ~f:Option.is_some then
              collapsed := ((Array.map start ~f:value_exn |> Array.to_list),
                            !length)::!collapsed;
            List.iteri il ~f:(fun i x -> start.(i) <- Some x);
            length := 1;
          end
        end);
      if Array.for_all start ~f:Option.is_some && !length <> 0 then
        collapsed := ((Array.map start ~f:value_exn |> Array.to_list),!length) ::
                     !collapsed;
      List.rev !collapsed

  type 'a segment =
    | Same of 'a array
    | Different of 'a array array

  type 'a merged_array = 'a segment list

  let array_mapi2 ar1 ar2 ~f =
    Array.zip_exn ar1 ar2
    |> Array.mapi ~f:(fun i (x,y) -> f i x y)

  let merge ar =
    if Array.length ar = 0 then [] else
    if Array.length ar = 1 then [Same ar.(0)] else
      let matches's =
        Array.map (ar <|> (1,(Array.length ar))) ~f:(matches ar.(0))
      in
      let len = Array.length ar in
      let hashtbl = Int.Table.create () ~size:0 in
      Array.iteri matches's ~f:(fun i matches ->
        List.iter matches ~f:(fun (a,b) ->
          match Hashtbl.find hashtbl a with
          | None -> Hashtbl.set hashtbl ~key:a ~data:[(i,b)]
          | Some l -> Hashtbl.set hashtbl ~key:a ~data:((i,b)::l)));
      let list =
        Hashtbl.to_alist hashtbl
        |> List.filter_map ~f:(fun (a,l) ->
          if List.length l = len - 1
          then Some (a::(List.sort l ~cmp:compare_int_pair |> List.map ~f:snd))
          else None)
        |> List.sort ~cmp:(List.compare Int.compare)
      in
      let matching_blocks = collapse_multi_sequences list in
      let last_pos = Array.create ~len:(Array.length ar) 0 in
      let merged_array = ref [] in
      List.iter matching_blocks ~f:(fun (l,len) ->
        let ar' = Array.of_list l in
        begin
          if Array.compare Int.compare last_pos ar' <> 0
          then merged_array :=
              (Different (array_mapi2 last_pos ar' ~f:(fun i n m -> ar.(i) <|>
                                                                    (n,m))))::!merged_array
        end;
        merged_array := Same (ar.(0) <|> (ar'.(0),ar'.(0) + len)):: !merged_array;
        Array.iteri last_pos ~f:(fun i _ -> last_pos.(i) <- ar'.(i) + len);
      );
      List.rev !merged_array
end

let%test_module _ =
  (module struct

    module P = Make (Int)

    let%test_unit _ =
      let check a b ~expect =
        [%test_result: (int * int) list]
          (P.matches a b) ~expect
      in
      check [||] [||] ~expect:[];
      check [|0|] [|0|] ~expect:[0,0];
      check [|0;1;1;2|] [|3;1;4;5|] ~expect:[1,1] (* Needs the plain diff section *)
    ;;

    let rec is_increasing a = function
      | [] -> true
      | hd :: tl ->
        Int.compare a hd <= 0 && is_increasing hd tl
    ;;

    let check_lis a =
      let b = Patience.longest_increasing_subsequence (Ordered_sequence.create a) in
      if is_increasing (-1) (List.map b ~f:fst)
      && is_increasing (-1) (List.map b ~f:snd)
      then ()
      else
        failwiths "invariant failure" (a, b)
          [%sexp_of: (int * int) list * (int*int) list]
    ;;

    let%test_unit _ =
      check_lis [(2,0);(5,1);(6,2);(3,3);(0,4);(4,5);(1,6)]
    ;;

    let%test_unit _ =
      check_lis [(0,0);(2,0);(5,1);(6,2);(3,3);(0,4);(4,5);(1,6)]
    ;;

    let%test_unit _ =
      check_lis [(5,1);(6,2);(3,3);(0,4);(4,5);(1,6)]
    ;;

    let%test_unit _ =
      let check a b =
        let matches = P.matches a b in
        if is_increasing (-1) (List.map matches ~f:fst)
        && is_increasing (-1) (List.map matches ~f:snd)
        then ()
        else
          failwiths "invariant failure" (a, b, matches)
            [%sexp_of: int array * int array * (int*int) list]
      in
      check [|0;1;2;3;4;5;6|] [|2;5;6;3;0;4;1|]
    ;;
  end)

module String = Make (String)
