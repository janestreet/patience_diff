module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type t =
      | Move of Move_id.Stable.V1.t
      | Within_move of Move_id.Stable.V1.t
    [@@deriving sexp, bin_io, compare ~localize]
  end
end

open! Core
include Stable.V1
