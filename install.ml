#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"patience_diff"
  [ oasis_lib "patience_diff_lib"
  ; file "META" ~section:"lib"
  ]
