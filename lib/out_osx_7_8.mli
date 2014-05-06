(*
 * Copyright (c) 2014 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Ctypes

module Struct : module type of Struct.Osx_7_8

module Entry : sig
  type t
  val t : t structure typ

  val nodeid           : (int64, t structure) field
  val generation       : (int64, t structure) field
  val entry_valid      : (int64, t structure) field
  val attr_valid       : (int64, t structure) field
  val entry_valid_nsec : (int32, t structure) field
  val attr_valid_nsec  : (int32, t structure) field
  val attr             : (Struct.Attr.t structure, t structure) field

  val store :
    nodeid:int64 ->
    generation:int64 ->
    entry_valid:int64 ->
    attr_valid:int64 ->
    entry_valid_nsec:int32 ->
    attr_valid_nsec:int32 ->
    store_attr:(Struct.Attr.t structure -> unit) ->
    t structure -> 'b In_common.request -> unit
  val create :
    nodeid:int64 ->
    generation:int64 ->
    entry_valid:int64 ->
    attr_valid:int64 ->
    entry_valid_nsec:int32 ->
    attr_valid_nsec:int32 ->
    store_attr:(Struct.Attr.t structure -> unit) ->
    'b In_common.request -> char CArray.t
  val describe : host:Fuse.host -> t structure -> string
end

module Attr : sig
  type t
  val t : t structure typ

  val attr_valid      : (int64, t structure) field
  val attr_valid_nsec : (int32, t structure) field
  val dummy           : (int32, t structure) field
  val attr            : (Struct.Attr.t structure, t structure) field

  val create :
    attr_valid:int64 ->
    attr_valid_nsec:int32 ->
    store_attr:(Struct.Attr.t structure -> unit) ->
    'b In_common.request -> char CArray.t
end

module Create : sig
  type t
  val t : t structure typ

  val entry  : (Entry.t structure, t structure) field
  val open_  : (Out_common.Open.t structure, t structure) field
  val create :
    store_entry:(Entry.t structure -> 'b In_common.request -> unit) ->
    store_open:(Out_common.Open.t structure -> 'b In_common.request -> unit) ->
    'b In_common.request -> char CArray.t
end
