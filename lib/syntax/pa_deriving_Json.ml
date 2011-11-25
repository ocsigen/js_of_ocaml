(* Js_of_ocaml
 * http://www.ocsigen.org
 * Copyright Grégoire Henry 2010.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Pa_deriving_common
open Utils

module Description = struct
  let classname = "Json"
  let runtimename = "Deriving_Json"
  let default_module = Some "Defaults"
  let alpha = None
  let allow_private = false
  let predefs = [
    ["int"      ], ["Deriving_Json";"int"];
    ["bool"     ], ["Deriving_Json";"bool"];
    ["unit"     ], ["Deriving_Json";"unit"];
    ["char"     ], ["Deriving_Json";"char"];
    ["int32"    ], ["Deriving_Json";"int32"];
    ["Int32";"t"], ["Deriving_Json";"int32"];
    ["int64"    ], ["Deriving_Json";"int64"];
    ["Int64";"t"], ["Deriving_Json";"int64"];
    ["nativeint"], ["Deriving_Json";"nativeint"];
    ["float"    ], ["Deriving_Json";"float"];
    ["string"   ], ["Deriving_Json";"string"];
    ["list"     ], ["Deriving_Json";"list"];
    ["ref"      ], ["Deriving_Json";"ref"];
    ["option"   ], ["Deriving_Json";"option"];
    ["array"    ], ["Deriving_Json";"array"];
  ]
  let depends = []
end

module Builder(Loc : Defs.Loc) = struct

  module Helpers = Base.AstHelpers(Loc)
  module Generator = Base.Generator(Loc)(Description)

  open Loc
  open Camlp4.PreCast
  open Description

  let wrap
      ?(read_variant = [ <:match_case< _ -> assert false >> ])
      ?(hashes = <:expr< assert false >>)
      ~write ~read () =
    [ <:str_item< let write buffer = function $list:write$ >>;
      <:str_item< let match_variant hash = $hashes$ >>;
      <:str_item<
	let read_variant buf hash = match hash with $list:read_variant$	>>;
      <:str_item< let read buf = $read$ >> ]

  let generator = (object (self)

    inherit Generator.generator

    method proxy unit =
      None, [ <:ident< t >>;
	      <:ident< write >>;
	      <:ident< read >>;
	      <:ident< to_string >>;
	      <:ident< from_string >>;
	      <:ident< match_variant >>;
	      <:ident< read_variant >>; ]

    (* Generate code that write a block with [tag].*)
    method do_dump_blk ctxt tag contents =
      let args_dumpers = List.map
	  (fun (var, ty) ->
	    <:expr<
	      Buffer.add_string buffer ",";
	      $self#call_expr ctxt ty "write"$ buffer $lid:var$ >>)
	   contents in
      <:expr<
        Buffer.add_string buffer $str:"["^string_of_int tag$;
        $Helpers.seq_list args_dumpers$;
        Buffer.add_string buffer "]"
      >>

    method tuple ctxt tys =
      let size = List.length tys in
      let vars, patt, expr = Helpers.tuple size in
      let contents = List.map2 (fun var ty -> (var, ty)) vars tys in
      let dumper =
	<:match_case< $patt$ -> $self#do_dump_blk ctxt 0 contents$ >> in
      let readers =
	List.fold_right2
	  (fun var ty expr -> <:expr<
	    Deriving_Json_lexer.read_comma buf;
	    let $lid:var$ = $self#call_expr ctxt ty "read"$ buf in $expr$ >>)
	  vars tys
	  <:expr<
	    Deriving_Json_lexer.read_rbracket buf;
            $expr$ >> in
      let read = <:expr<
	Deriving_Json_lexer.read_lbracket buf;
        ignore(Deriving_Json_lexer.read_bounded_int buf ~min:0 ~max:0);
	$readers$ >> in
      wrap ~write:[dumper] ~read ()

    method case ctxt (cst_tag, ncst_tag, dumpers, readers) (ctor, tys) =
      match tys with
      | [] ->
	  let dumper = <:match_case< $uid:ctor$ ->
	    Buffer.add_string buffer $str:string_of_int cst_tag$ >> in
	  let reader = <:match_case< `Cst $int:string_of_int cst_tag$ -> $uid:ctor$ >> in
	  (succ cst_tag, ncst_tag, dumper::dumpers, reader::readers)
      | tys ->
	  let size = List.length tys in
	  let vars, patt, expr = Helpers.tuple size in
	  let contents = List.map2 (fun var ty -> (var, ty)) vars tys in
	  let dumper =
	    <:match_case< $uid:ctor$ $patt$ ->
	      $self#do_dump_blk ctxt ncst_tag contents$ >> in
	  let reader =
	    List.fold_right2
	      (fun var ty expr -> <:expr<
		Deriving_Json_lexer.read_comma buf;
		let $lid:var$ = $self#call_expr ctxt ty "read"$ buf in $expr$ >>)
	      vars tys
	      <:expr<
	        Deriving_Json_lexer.read_rbracket buf;
	        $uid:ctor$ $expr$ >> in
	  let reader =
	    <:match_case< `NCst $int:string_of_int ncst_tag$ -> $reader$ >> in
	  (cst_tag, succ ncst_tag, dumper::dumpers, reader::readers)

    method sum ?eq ctxt name params _ summands =
      let msg = Printf.sprintf "Json_%s: Unexpected constructor." classname in
      let failover = <:match_case< _ -> failwith $str:msg$ >> in
      let _, _, dumpers, readers =
	List.fold_left (self#case ctxt) (0,0,[],[failover]) summands in
      let read = <:expr<
	match Deriving_Json_lexer.read_case buf with
	$list:readers$ >> in
      wrap ~write:dumpers ~read ()

    method record ?eq ctxt name params _ fields =
      if List.exists (fun (_, _, mut) -> mut = `Mutable) fields then
	failwith "Can't derive Json serializer for mutable records.";
      if List.exists (fun (_, (vars, _), _) -> vars <> []) fields then
	failwith "Can't derive Json serializer with polymorphic records.";
      let patt = Helpers.record_pattern fields in
      let contents = List.map (fun (name, (_,ty), _) -> name, ty) fields in
      let dumper =
	<:match_case< $patt$ -> $self#do_dump_blk ctxt 0 contents$ >> in
      let readers =
	List.fold_right
	  (fun (var, ty, _) expr ->
	    <:expr<
	      Deriving_Json_lexer.read_comma buf;
	      let $lid:var$ = $self#call_poly_expr ctxt ty "read"$ buf in $expr$ >>)
	  fields
	  <:expr<
            Deriving_Json_lexer.read_rbracket buf;
	    $Helpers.record_expression fields$ >> in
      let read = <:expr<
	Deriving_Json_lexer.read_lbracket buf;
        ignore(Deriving_Json_lexer.read_bounded_int buf ~min:0 ~max:0);
	$readers$ >> in
      wrap ~write:[dumper] ~read ()

    method polycase ctxt tagspec =
      match tagspec with
      | Type.Tag (name, []) ->
	  let hash = Pa_deriving_common.Utils.tag_hash name in
	  <:match_case< `$uid:name$ ->
	    Buffer.add_string buffer $str:string_of_int hash$ >>,
	  <:match_case< `Cst $int:string_of_int hash$ -> `$name$ >>,
	  <:expr< hash = `Cst $int:string_of_int hash$ >>
      | Type.Tag (name, [ty]) ->
	  let hash = Pa_deriving_common.Utils.tag_hash name in
	  let contents = ["tag", `Constr(["int"],[]) ; "x", ty ] in
	  <:match_case< `$uid:name$ x ->
	    let tag = $int:string_of_int hash$ in
	    $self#do_dump_blk ctxt 0 contents$ >>,
	  <:match_case< `NCst $int:string_of_int hash$ ->
	    Deriving_Json_lexer.read_comma buf;
	    let c = $self#call_expr ctxt ty "read"$ buf in
	    Deriving_Json_lexer.read_rbracket buf;
	    `$name$ c >>,
	    <:expr< hash = `NCst $int:string_of_int hash$ >>
      | Type.Tag (name, tys) ->
	  let hash = Pa_deriving_common.Utils.tag_hash name in
	  let ty = `Tuple tys in
	  let contents = ["tag", `Constr(["int"],[]) ; "x", ty ] in
	  <:match_case< `$uid:name$ x ->
	    let tag = $int:string_of_int hash$ in
	    $self#do_dump_blk ctxt 0 contents$ >>,
	  <:match_case< `NCst $int:string_of_int hash$ ->
	    Deriving_Json_lexer.read_comma buf;
	    let c = $self#call_expr ctxt ty "read"$ buf in
	    Deriving_Json_lexer.read_rbracket buf;
	    `$name$ c >>,
	    <:expr< hash = `NCst $int:string_of_int hash$ >>
      | Type.Extends t ->
          let patt, guard, cast = Generator.cast_pattern ctxt t in
          <:match_case< $patt$ when $guard$ ->
            $self#call_expr ctxt t "write"$ buffer $cast$ >>,
          <:match_case< hash when $self#call_expr ctxt t "match_variant"$ hash ->
            ($self#call_expr ctxt t "read_variant"$ buf hash :> a) >>,
          <:expr< $self#call_expr ctxt t "match_variant"$ hash >>

    method variant ctxt name params _ (_,tags) =
      let msg = Printf.sprintf "Json_%s: Unexpected constructor." classname in
      let failover = <:match_case< _ -> failwith $str:msg$ >> in
      let dumpers, readers, hashes = List.split3 (List.map (self#polycase ctxt) tags) in
      let read = <:expr< read_variant buf (Deriving_Json_lexer.read_vcase buf) >> in
      let hashes =
	List.fold_right
	  (fun e1 e2 -> <:expr< $e1$ || $e2$ >>)
	  hashes <:expr< false >> in
      wrap
	~read_variant:(readers @ [failover]) ~hashes
	~write:(dumpers@[failover]) ~read ()

  end :> Generator.generator)

  let classname = Description.classname
  let runtimename = Description.runtimename
  let generate = Generator.generate generator
  let generate_sigs = Generator.generate_sigs generator
  let generate_expr = Generator.generate_expr generator

end

module Json = Base.Register(Description)(Builder)

let depends = (module Builder : Defs.FullClassBuilder)
