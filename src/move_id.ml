module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type t = int [@@deriving sexp, bin_io, compare]
  end
end

open! Core
include Stable.V1

let zero = 0
let succ = Int.succ
let to_string = Int.to_string
