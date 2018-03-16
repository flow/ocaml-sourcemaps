(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Sourcemaps

type json =
  | JSON_String of string
  | JSON_Object of (string * json) list
  | JSON_Array of json list
  | JSON_Number of string
  | JSON_Null
type json' = json

exception Json_error

module Json_writer : (
  Sourcemap.Json_writer_intf with type t = json
) = struct
  type t = json
  let of_string x = JSON_String x
  let of_obj props = JSON_Object props
  let of_array arr = JSON_Array arr
  let of_number x = JSON_Number x
  let null = JSON_Null
end

module Json_reader : (
  Sourcemap.Json_reader_intf with type t = json
) = struct
  type t = json

  let to_string t =
    match t with
    | JSON_String x -> x
    | _ -> raise Json_error

  let to_obj t =
    match t with
    | JSON_Object x -> x
    | _ -> raise Json_error

  let to_array t =
    match t with
    | JSON_Array x -> x
    | _ -> raise Json_error

  let to_number t =
    match t with
    | JSON_Number x -> x
    | _ -> raise Json_error

  let is_null t = t = JSON_Null
end

module W = Sourcemap.Make_json_writer (Json_writer)
open W
module R = Sourcemap.Make_json_reader (Json_reader)
open R

let tests = "json" >::: [
  "json_of_sourcemap_empty" >:: begin fun ctxt ->
    let map = Sourcemap.create () in
    let actual = json_of_sourcemap map in
    let expected = JSON_Object [
      "version", JSON_Number "3";
      "sources", JSON_Array [];
      "names", JSON_Array [];
      "mappings", JSON_String "";
    ] in
    assert_equal ~ctxt expected actual
  end;

  "json_of_sourcemap_with_file" >:: begin fun ctxt ->
    let map = Sourcemap.create ~file:"foo.js" () in
    let actual = json_of_sourcemap map in
    let expected = JSON_Object [
      "version", JSON_Number "3";
      "sources", JSON_Array [];
      "names", JSON_Array [];
      "mappings", JSON_String "";
      "file", JSON_String "foo.js";
    ] in
    assert_equal ~ctxt expected actual
  end;

  "json_of_sourcemap_with_source_root" >:: begin fun ctxt ->
    let map = Sourcemap.create ~source_root:"http://example.com/" () in
    let actual = json_of_sourcemap map in
    let expected = JSON_Object [
      "version", JSON_Number "3";
      "sources", JSON_Array [];
      "names", JSON_Array [];
      "mappings", JSON_String "";
      "sourceRoot", JSON_String "http://example.com/";
    ] in
    assert_equal ~ctxt expected actual
  end;

  "json_of_sourcemap" >:: begin fun ctxt ->
    let original = { Sourcemap.line = 1; col = 1 } in
    let generated = { Sourcemap.line = 3; col = 1 } in
    let map =
      Sourcemap.create ()
      |> Sourcemap.add_mapping ~source:"bar.js" ~original ~generated
      |> Sourcemap.add_mapping ~source:"foo.js" ~original ~generated
    in
    let actual = json_of_sourcemap map in
    let expected = JSON_Object [
      "version", JSON_Number "3";
      "sources", JSON_Array [JSON_String "bar.js"; JSON_String "foo.js"];
      "names", JSON_Array [];
      "mappings", JSON_String ";;CAAC,ACAA";
    ] in
    assert_equal ~ctxt expected actual
  end;

  "sourcemap_of_json" >:: begin fun ctxt ->
    let json = JSON_Object [
      "version", JSON_Number "3";
      "sources", JSON_Array [JSON_String "bar.js"; JSON_String "foo.js"];
      "names", JSON_Array [JSON_String "y"];
      "mappings", JSON_String ";;CAAC,ACAAA";
    ] in
    let expected =
      let original = { Sourcemap.line = 1; col = 1 } in
      let generated = { Sourcemap.line = 3; col = 1 } in
      Sourcemap.create ()
      |> Sourcemap.add_mapping ~source:"bar.js" ~original ~generated
      |> Sourcemap.add_mapping ~source:"foo.js" ~original ~generated ~name:"y"
    in
    let actual = sourcemap_of_json json in

    assert_equal ~ctxt ~msg:"Versions not equal"
      (Sourcemap.version expected) (Sourcemap.version actual);
    assert_equal ~ctxt ~msg:"Sources not equal" ~printer:(String.concat "; ")
      (Sourcemap.sources expected) (Sourcemap.sources actual);
    assert_equal ~ctxt ~msg:"Source root not equal"
      (Sourcemap.source_root expected) (Sourcemap.source_root actual);
    assert_equal ~ctxt ~msg:"Names not equal"
      (Sourcemap.names expected) (Sourcemap.names actual);
    assert_equal ~ctxt ~msg:"Mappings not equal" ~printer:(fun x -> x)
      (Sourcemap.string_of_mappings expected) (Sourcemap.string_of_mappings actual);
    assert_equal ~ctxt ~msg:"Source content not equal"
      (Sourcemap.sources_contents expected) (Sourcemap.sources_contents actual);
    assert_equal ~ctxt expected actual
  end
]
