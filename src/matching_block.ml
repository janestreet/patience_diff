module Stable = struct
  open! Core_kernel.Core_kernel_stable

  module V1 = struct
    type t =
      { prev_start : int
      ; next_start : int
      ; length : int
      }
    [@@deriving sexp, bin_io]
  end
end

open! Core_kernel
include Stable.V1
