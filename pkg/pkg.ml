#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "bitrel" @@ fun c ->
  Ok [ Pkg.mllib "src/bitrel.mllib";
       Pkg.clib "src/libbitrel.clib";
       Pkg.test "test/test"; ]
