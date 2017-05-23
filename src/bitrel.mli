(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vasil Diadov. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Library for manipulating of bit vectors

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Bits} *)

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
  val make_transitive_inplace : t -> unit
  val make_reflexive_inplace : t -> unit
  val make_symmetrical_inplace : t -> unit
  val make_transitive : t -> t
  val make_reflexive : t -> t
  val make_symmetrical : t -> t
  val map : ( rel -> rel ) -> t -> t
  val map_for_sources_of : elt -> (src:elt -> elt) -> t -> t
  val map_for_destinations_of : elt -> (dst:elt -> elt) -> t -> t
  val is_reflexive : t -> bool
  val is_symmetrical : t -> bool
  val is_transitive : t -> bool
  val iter : (rel -> unit) -> t -> unit
  val clear : t -> unit

end

module Make (U : UNIVERSE) : (S with type elt = U.t)
