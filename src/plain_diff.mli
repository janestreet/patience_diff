val iter_matches
  :  f:((int*int) -> unit)
  -> hashable:(module Base.Hashtbl.Key with type t = 'a)
  -> 'a array
  -> 'a array
  -> unit
