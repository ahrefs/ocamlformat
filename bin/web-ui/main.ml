open Js_of_ocaml
module Html = Dom_html

let debug fmt =
  Printf.ksprintf
    (fun s -> Js_of_ocaml.Firebug.console##log (Js.string s))
    fmt

let config_path = Fpath.v ".ocamlformat"

let make_option name values insert =
  let insert conf value =
    if String.equal value "--" then conf
    else (
      debug "updating %s with %s" name value ;
      let v = List.Assoc.find_exn values ~equal:String.equal value in
      insert conf v )
  in
  let values = List.map ~f:fst values in
  (name, values, insert)

let config_options =
  let open Ocamlformat in
  [ make_option "assignment_operator"
      [("begin line", `Begin_line); ("end line", `End_line)]
      (fun conf assignment_operator ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with assignment_operator} } )
  ; make_option "break_before_in"
      [("fit or vertical", `Fit_or_vertical); ("auto", `Auto)]
      (fun conf break_before_in ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with break_before_in}}
        )
  ; make_option "break_cases"
      [ ("fit", `Fit)
      ; ("nested", `Nested)
      ; ("toplevel", `Toplevel)
      ; ("fit or vertical", `Fit_or_vertical)
      ; ("all", `All) ]
      (fun conf break_cases ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with break_cases}} )
  ; make_option "break_collection_expressions"
      [("wrap", `Wrap); ("fit or vertical", `Fit_or_vertical)]
      (fun conf break_collection_expressions ->
        { conf with
          Conf.fmt_opts=
            {conf.Conf.fmt_opts with break_collection_expressions} } )
  ; make_option "break_infix"
      [("wrap", `Wrap); ("fit or vertical", `Fit_or_vertical)]
      (fun conf break_infix ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with break_infix}} )
  ; make_option "break_infix_before_func"
      [("true", true); ("false", false)]
      (fun conf break_infix_before_func ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with break_infix_before_func} }
        )
  ; make_option "break_fun_decl"
      [ ("wrap", `Wrap)
      ; ("fit or vertical", `Fit_or_vertical)
      ; ("smart", `Smart) ]
      (fun conf break_fun_decl ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with break_fun_decl}}
        )
  ; make_option "break_fun_sig"
      [ ("wrap", `Wrap)
      ; ("fit or vertical", `Fit_or_vertical)
      ; ("smart", `Smart) ]
      (fun conf break_fun_sig ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with break_fun_sig}} )
  ; make_option "break_separators"
      [("before", `Before); ("after", `After)]
      (fun conf break_separators ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with break_separators}}
        )
  ; make_option "break_sequences"
      [("true", true); ("false", false)]
      (fun conf break_sequences ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with break_sequences}}
        )
  ; make_option "break_string_literals"
      [("auto", `Auto); ("never", `Never)]
      (fun conf break_string_literals ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with break_string_literals} } )
  ; make_option "break_struct"
      [("true", true); ("false", false)]
      (fun conf break_struct ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with break_struct}} )
  ; make_option "cases_matching_exp_indent"
      [("normal", `Normal); ("compact", `Compact)]
      (fun conf cases_matching_exp_indent ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with cases_matching_exp_indent}
        } )
  ; make_option "disambiguate_non_breaking_match"
      [("true", true); ("false", false)]
      (fun conf disambiguate_non_breaking_match ->
        { conf with
          Conf.fmt_opts=
            {conf.Conf.fmt_opts with disambiguate_non_breaking_match} } )
  ; make_option "doc_comments"
      [ ("before", `Before)
      ; ("before except val", `Before_except_val)
      ; ("after whe possible", `After_when_possible) ]
      (fun conf doc_comments ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with doc_comments}} )
  ; make_option "doc_comments_tag_only"
      [("fit", `Fit); ("default", `Default)]
      (fun conf doc_comments_tag_only ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with doc_comments_tag_only} } )
  ; make_option "dock_collection_brackets"
      [("true", true); ("false", false)]
      (fun conf dock_collection_brackets ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with dock_collection_brackets}
        } )
  ; make_option "exp_grouping"
      [("parens", `Parens); ("preserve", `Preserve)]
      (fun conf exp_grouping ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with exp_grouping}} )
  ; make_option "field_space"
      [("tight", `Tight); ("loose", `Loose); ("tight decl", `Tight_decl)]
      (fun conf field_space ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with field_space}} )
  ; make_option "function_indent_nested"
      [("always", `Always); ("auto", `Auto); ("never", `Never)]
      (fun conf function_indent_nested ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with function_indent_nested} }
        )
  ; make_option "if_then_else"
      [ ("compact", `Compact)
      ; ("fit or vertical", `Fit_or_vertical)
      ; ("keyword first", `Keyword_first)
      ; ("KR", `K_R) ]
      (fun conf if_then_else ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with if_then_else}} )
  ; make_option "indicate_multiline_delimiters"
      [ ("no", `No)
      ; ("space", `Space)
      ; ("closing on separate line", `Closing_on_separate_line) ]
      (fun conf indicate_multiline_delimiters ->
        { conf with
          Conf.fmt_opts=
            {conf.Conf.fmt_opts with indicate_multiline_delimiters} } )
  ; make_option "indicate_nested_or_patterns"
      [("space", `Space); ("unsafe no", `Unsafe_no)]
      (fun conf indicate_nested_or_patterns ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with indicate_nested_or_patterns}
        } )
  ; make_option "infix_precedence"
      [("indent", `Indent); ("parens", `Parens)]
      (fun conf infix_precedence ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with infix_precedence}}
        )
  ; make_option "leading_nested_match_parens"
      [("true", true); ("false", false)]
      (fun conf leading_nested_match_parens ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with leading_nested_match_parens}
        } )
  ; make_option "let_and"
      [("compact", `Compact); ("sparse", `Sparse)]
      (fun conf let_and ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with let_and}} )
  ; make_option "let_binding_spacing"
      [ ("compact", `Compact)
      ; ("sparse", `Sparse)
      ; ("double semicolon", `Double_semicolon) ]
      (fun conf let_binding_spacing ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with let_binding_spacing} } )
  ; make_option "let_module"
      [("compact", `Compact); ("sparse", `Sparse)]
      (fun conf let_module ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with let_module}} )
  ; make_option "line_endings"
      [("lf", `Lf); ("crlf", `Crlf)]
      (fun conf line_endings ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with line_endings}} )
  ; make_option "match_indent_nested"
      [("always", `Always); ("auto", `Auto); ("never", `Never)]
      (fun conf match_indent_nested ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with match_indent_nested} } )
  ; make_option "module_item_spacing"
      [("compact", `Compact); ("preserve", `Preserve); ("sparse", `Sparse)]
      (fun conf module_item_spacing ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with module_item_spacing} } )
  ; make_option "nested_match"
      [("wrap", `Wrap); ("align", `Align)]
      (fun conf nested_match ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with nested_match}} )
  ; make_option "ocp_indent_compat"
      [("true", true); ("false", false)]
      (fun conf ocp_indent_compat ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with ocp_indent_compat}}
        )
  ; make_option "parens_ite"
      [("true", true); ("false", false)]
      (fun conf parens_ite ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with parens_ite}} )
  ; make_option "parens_tuple"
      [("always", `Always); ("multi line only", `Multi_line_only)]
      (fun conf parens_tuple ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with parens_tuple}} )
  ; make_option "parens_tuple_patterns"
      [("always", `Always); ("multi line only", `Multi_line_only)]
      (fun conf parens_tuple_patterns ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with parens_tuple_patterns} } )
  ; make_option "parse_docstrings"
      [("true", true); ("false", false)]
      (fun conf parse_docstrings ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with parse_docstrings}}
        )
  ; make_option "parse_toplevel_phrases"
      [("true", true); ("false", false)]
      (fun conf parse_toplevel_phrases ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with parse_toplevel_phrases} }
        )
  ; make_option "sequence_blank_line"
      [("compact", `Compact); ("preserve one", `Preserve_one)]
      (fun conf sequence_blank_line ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with sequence_blank_line} } )
  ; make_option "sequence_style"
      [ ("before", `Before)
      ; ("separator", `Separator)
      ; ("terminator", `Terminator) ]
      (fun conf sequence_style ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with sequence_style}}
        )
  ; make_option "single_case"
      [("compact", `Compact); ("sparse", `Sparse)]
      (fun conf single_case ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with single_case}} )
  ; make_option "space_around_arrays"
      [("true", true); ("false", false)]
      (fun conf space_around_arrays ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with space_around_arrays} } )
  ; make_option "space_around_lists"
      [("true", true); ("false", false)]
      (fun conf space_around_lists ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with space_around_lists} } )
  ; make_option "space_around_records"
      [("true", true); ("false", false)]
      (fun conf space_around_records ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with space_around_records} } )
  ; make_option "space_around_variants"
      [("true", true); ("false", false)]
      (fun conf space_around_variants ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with space_around_variants} } )
  ; make_option "type_decl"
      [("compact", `Compact); ("sparse", `Sparse)]
      (fun conf type_decl ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with type_decl}} )
  ; make_option "wrap_comments"
      [("true", true); ("false", false)]
      (fun conf wrap_comments ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with wrap_comments}} )
  ; make_option "wrap_fun_args"
      [("true", true); ("false", false)]
      (fun conf wrap_fun_args ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with wrap_fun_args}} )
  ]

let config_options_numbers =
  let open Ocamlformat in
  [ ( "cases_exp_indent"
    , fun conf cases_exp_indent ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with cases_exp_indent}}
    )
  ; ( "doc_comments_padding"
    , fun conf doc_comments_padding ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with doc_comments_padding} } )
  ; ( "extension_indent"
    , fun conf extension_indent ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with extension_indent}}
    )
  ; ( "function_indent"
    , fun conf function_indent ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with function_indent}}
    )
  ; ( "indent_after_in"
    , fun conf indent_after_in ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with indent_after_in}}
    )
  ; ( "let_binding_indent"
    , fun conf let_binding_indent ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with let_binding_indent} } )
  ; ( "margin"
    , fun conf margin ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with margin}} )
  ; ( "match_indent"
    , fun conf match_indent ->
        {conf with Conf.fmt_opts= {conf.Conf.fmt_opts with match_indent}} )
  ; ( "max_indent"
    , fun conf max_indent ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with max_indent= Some max_indent}
        } )
  ; ( "stritem_extension_indent"
    , fun conf stritem_extension_indent ->
        { conf with
          Conf.fmt_opts= {conf.Conf.fmt_opts with stritem_extension_indent}
        } ) ]

let make_config str =
  let open Ocamlformat in
  let lines = String.split_lines str in
  let rec loop conf errors i lines =
    match lines with
    | [] -> (conf, errors)
    | l :: lines -> (
      match Conf.parse_line conf ~from:(`File (config_path, i)) l with
      | Ok conf -> loop conf errors (Int.succ i) lines
      | Error e ->
          debug "unable to parse config line %d: %s" i l ;
          loop conf (e :: errors) (Int.succ i) lines )
  in
  loop Conf.default [] 1 lines

let format source conf =
  let open Ocamlformat in
  match
    Translation_unit.parse_and_format Syntax.Use_file conf
      ~input_name:"input.ml" ~source
  with
  | Ok formatted -> Ok formatted
  | Error e ->
      let error_buf = Buffer.create 100 in
      let fmt = Format.formatter_of_buffer error_buf in
      Translation_unit.Error.print fmt ~debug:conf.opr_opts.debug
        ~quiet:conf.opr_opts.quiet e ;
      Format.pp_print_flush fmt () ;
      let error_msg = Buffer.contents error_buf in
      debug "error during formatting: %s" error_msg ;
      Error error_msg

let get_element_exn element_id coerce =
  match Html.getElementById_coerce element_id coerce with
  | None ->
      debug "unable to find the element with id %s" element_id ;
      failwith
        (Printf.sprintf "unable to find element with id %s" element_id)
  | Some e -> e

let onload _event =
  let () =
    let d = Html.document in
    let code_input = get_element_exn "code" Html.CoerceTo.textarea in
    let config_input = get_element_exn "config" Html.CoerceTo.textarea in
    let config_options_div = get_element_exn "options" Html.CoerceTo.div in
    let format_button = get_element_exn "format" Html.CoerceTo.button in
    let config_options =
      List.map config_options ~f:(fun (name, values, update) ->
          let div = Html.createDiv d in
          Dom.appendChild config_options_div div ;
          let l = Html.createLabel d in
          l##.innerText := Js.string name ;
          Dom.appendChild div l ;
          let s = Html.createSelect ~name:(Js.string name) d in
          Dom.appendChild div s ;
          let values = "--" :: values in
          let () =
            List.iter values ~f:(fun v ->
                let o = Html.createOption d in
                o##.label := Js.string v ;
                s##add o Js.null )
          in
          fun conf ->
            let i = s##.selectedIndex in
            let v = List.nth_exn values i in
            update conf v )
    in
    let config_options_numbers =
      List.map config_options_numbers ~f:(fun (name, update) ->
          let div = Html.createDiv d in
          Dom.appendChild config_options_div div ;
          let l = Html.createLabel d in
          l##.innerText := Js.string name ;
          Dom.appendChild div l ;
          let input =
            Html.createInput ~_type:(Js.string "number")
              ~name:(Js.string name) d
          in
          Dom.appendChild div input ;
          fun conf ->
            match Js.to_string input##.value with
            | "" -> conf
            | s ->
                debug "updating %s" name ;
                let v = Int.of_string s in
                update conf v )
    in
    format_button##.onclick :=
      Html.handler (fun _event ->
          let code_to_format = Js.to_string code_input##.value in
          let config = Js.to_string config_input##.value in
          let config, _config_errors = make_config config in
          let config =
            List.fold config_options ~init:config ~f:(fun conf update ->
                update conf )
          in
          let config =
            List.fold config_options_numbers ~init:config
              ~f:(fun conf update -> update conf)
          in
          Ocamlformat.Conf.print_config config ;
          let code_formatted = format code_to_format config in
          let () =
            match code_formatted with
            | Ok code_formatted ->
                code_input##.value := Js.string code_formatted
            | Error _e -> ()
          in
          Js._true ) ;
    ()
  in
  Js._false

let () = Html.window##.onload := Html.handler onload
