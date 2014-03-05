open Ctypes

module Struct : module type of Struct.Linux_7_8

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
