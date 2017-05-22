(*---------------------------------------------------------------------------
  Copyright (c) 2017 Vasil Diadov. All rights reserved.
  Distributed under the ISC license, see terms at the end of the file.
  %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(*---------------------------------------------------------------------------
  Copyright (c) 2017 Vasil Diadov

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

module type UNIVERSE = sig
  type t
  val universe : t list
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t
  type rel = { src : elt; dst : elt}

  val make : unit -> t
  val of_list : rel list -> t
  val to_list : t -> rel list
  val of_list_by_src : (elt * elt list) list -> t
  val to_list_by_src : t -> (elt * elt list) list
  val get_destinations : t -> elt -> elt list
  val copy : t -> t
  val get_sources : t -> elt -> elt list
  val add_relations : t -> rel list -> t
  val del_relations : t -> rel list -> t
  val add_relations_inplace : t -> rel list -> unit
  val del_relations_inplace : t -> rel list -> unit
  val make_transitive_closure_inplace : t -> unit
  val make_reflexive_inplace : t -> unit
  val make_symmetric_inplace : t -> unit
  val make_transitive_closure : t -> t
  val make_reflexive : t -> t
  val make_symmetric : t -> t

(*
  val is_reflexive : t -> bool
  val is_symmetric : t -> bool
  val is_transitive : t -> bool

  val map : ( rel -> rel ) -> t -> t
  val map_for_sources_of : elt -> (src:elt -> elt) -> t -> t
  val map_for_destinations_of: elt -> (dst:elt -> elt) -> t -> t
*)

  val iter : (rel -> unit) -> t -> unit
  val clear : t -> unit
end

module Make (U : UNIVERSE) : (S with type elt = U.t) = struct
  type elt = U.t
  type t = Bits.bits ref array
  type rel = { src : elt; dst : elt}

  let u = ref (Array.of_list (List.sort_uniq U.compare U.universe))
  let u_len = Array.length !u

  exception Not_in_universe of elt

  (** helpers *)

  let check e =
    let n = ref 0 in
    if Array.exists
         (fun el -> U.compare e el == 0 || (incr n; false))
         !u
    then !n
    else raise (Not_in_universe e)

  let ( ?! ) e = check e
  let ( ??! ) { src : elt; dst : elt } = ?! src, ?! dst

  let set_rel_bit r (s, d) = Bits.set !(r.(s)) d true
  let clear_rel_bit r (s, d) = Bits.set !(r.(s)) d false

  let make () = Array.init u_len (fun _ -> ref (Bits.make u_len false))
  let add_relations_inplace rels rels_list = List.iter (fun r -> ??! r |> set_rel_bit rels ) rels_list
  let del_relations_inplace rels rels_list = List.iter (fun r -> ??! r |> clear_rel_bit rels) rels_list
  let of_list rels_list = let r = make () in add_relations_inplace r rels_list; r
  let to_list rels =
    let rels_list = ref [] in
    Array.iteri
      (fun s b ->
        Bits.iteri_on_val
          (fun d -> rels_list := {src = !u.(s); dst = !u.(d)} :: !rels_list)
          !b
          true)
      rels;
    !rels_list

  let make_transitive_closure_inplace r =
    let or_all b = Bits.iteri_on_val (fun n -> ignore (Bits.lor_inplace b !(r.(n)))) b true in
    let rec close_aux orig b =
      or_all b;
      if not (Bits.all_zeros (Bits.lxor_inplace (Bits.lor_inplace b orig) orig))
        then close_aux (Bits.lor_inplace orig b) b in
    let close b = close_aux b (Bits.copy b) in
    Array.iter (fun b -> close !b) r

  let make_symmetric_inplace r =
    let set_dst_in_sources n_dst b =
      Bits.iteri_on_val (fun s -> Bits.set !(r.(s)) n_dst true) !b true in
    Array.iteri set_dst_in_sources r

  let make_reflexive_inplace r =
    Array.iteri (fun n b -> Bits.set !b n true) r

  let of_list_by_src l =
    let r = make () in
    let set_destinations_for_source (src, dsts) =
      let src_n = ?! src in
      List.iter (fun dst -> let dst_n = ?! dst in set_rel_bit r (src_n,dst_n)) dsts in
    List.iter set_destinations_for_source l;
    r

  let get_destinations_for_src_n r s =
    let l = ref [] in
      Bits.iteri_on_val (fun d -> l := !u.(d) :: !l) !(r.(s)) true;
      !l

  let get_destinations r src = ?! src |> get_destinations_for_src_n r

  let get_sources_for_dst_n r d =
    let l = ref [] in
      Array.iteri (fun s b -> if (Bits.get !b d) then l := !u.(s) :: !l) r;
      !l

  let get_sources r dst = ?! dst |> get_sources_for_dst_n r

  let to_list_by_src r =
    let l = ref [] in
      Array.iteri
        (fun s b ->
          if not (Bits.all_zeros !b)
          then l := (!u.(s), get_destinations_for_src_n r s) :: !l) r;
      !l

  let copy r = Array.map (fun b -> ref (Bits.copy !b)) r

  let with_copy_of r f = let new_r = copy r in f new_r; new_r

  let add_relations r rels = with_copy_of r (fun nr -> add_relations_inplace nr rels)
  let del_relations r rels = with_copy_of r (fun nr -> del_relations_inplace nr rels)

  let make_transitive_closure r = with_copy_of r make_transitive_closure_inplace
  let make_reflexive r = with_copy_of r make_reflexive_inplace
  let make_symmetric r = with_copy_of r make_symmetric_inplace

(*
  val is_reflexive : t -> bool
  val is_symmetric : t -> bool
  val is_transitive : t -> bool

  val map : ( rel -> rel ) -> t -> t
  val map_for_sources_of : elt -> (src:elt -> elt) -> t -> t
  val map_for_destinations_of : elt -> (dst:elt -> elt) -> t -> t
  *)

  let iter f r =
    Array.iteri
      (fun s b ->
        Bits.iteri_on_val (fun d -> f {src = !u.(s); dst = !u.(d)}) !b true)
      r

  let clear r = Array.iter (fun b -> Bits.fill !b 0 u_len false) r

end