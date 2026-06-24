open Variable_analysis

let src = ref ""
let opt_pp = ref false
let opt_tab = ref false
let opt_tintp = ref false
let opt_dintp = ref false
let opt_analyze = ref false
let opt_analyze_detail = ref false
let opt_prov = ref false
let opt_report = ref false
let report_out = ref None
let opt_summary = ref false
let opt_optoff = ref false
let opt_selectoff = ref false
let opt_compileoff = ref false

let read_all (ic : in_channel) : string =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  Buffer.contents buf

let write_all (path : string) (contents : string) : unit =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

let default_report_path () : string =
  if !src = "" then "codex_bug_report.md"
  else Filename.remove_extension !src ^ ".report.md"

let main () =
  Arg.parse
    [
      ("-pp", Arg.Unit (fun _ -> opt_pp := true), "print a labeled program");
      ("-tab", Arg.Unit (fun _ -> opt_tab := true), "print a label table");
      ( "-tintp",
        Arg.Unit (fun _ -> opt_tintp := true),
        "D transitional interpreter" );
      ( "-dintp",
        Arg.Unit (fun _ -> opt_dintp := true),
        "D definitional interpreter" );
      ( "-analyze",
        Arg.Unit (fun _ -> opt_analyze := true),
        "Watercheck analyzer" );
      ( "-analyzedetail",
        Arg.Unit (fun _ -> opt_analyze_detail := true),
        "Watercheck analyzer - Detail version" );
      ( "-prov",
        Arg.Unit (fun _ -> opt_prov := true),
        "provenance analysis: trace bug origins (no LLM)" );
      ( "-report",
        Arg.Unit (fun _ -> opt_report := true),
        "write LLM bug report markdown: provenance analysis + Codex explanation" );
      ( "-report-out",
        Arg.String (fun path -> report_out := Some path),
        "FILE write -report markdown to FILE" );
      ( "-summary",
        Arg.Unit (fun _ -> opt_summary := true),
        "print pre-compiled handler summaries" );
      ( "-optoff",
        Arg.Unit (fun _ -> opt_optoff := true),
        "disable all handler optimizations: selective application and compiled fixpoint (use with -prov or -report)" );
      ( "-selectoff",
        Arg.Unit (fun _ -> opt_selectoff := true),
        "disable selective handler application at yield points only (use with -prov or -report)" );
      ( "-compileoff",
        Arg.Unit (fun _ -> opt_compileoff := true),
        "disable compiled handler fixpoint optimization only (use with -prov or -report)" );
    ]
    (fun x -> src := x)
    ("Usage : " ^ Filename.basename Sys.argv.(0) ^ " [-option] [filename] ");
  let source_code =
    if !src = "" then read_all stdin
    else
      let ic = open_in !src in
      Fun.protect
        ~finally:(fun () -> close_in ic)
        (fun () -> read_all ic)
  in
  let lexbuf =
    Lexing.from_string source_code
  in
  let pgm = Parser.prog Lexer.read lexbuf in
  let open Syntax.Program in
  if !opt_pp then string_of_t pgm |> print_endline;
  if !opt_tab then (
    let open Syntax in
    let print_tbl (title : string) (tbl : Exp.t Exp.Lbl_map.t) : unit =
      Printf.printf "==== %s ====\n" title;
      tbl |> Exp.Lbl_map.bindings
      |> List.iter (fun (k, v) ->
          Printf.printf "%s -> %s\n"
            (Exp.Lbl_map.string_of_t k)
            (Exp.string_of_t v));
      print_endline ""
    in
    print_tbl "TABULATE: global" (Exp.tabulate pgm.global);
    pgm.handler
    |> List.iter (fun (h : Syntax.Handler.t) ->
        let title =
          Printf.sprintf "TABULATE: handler %d" (Syntax.Handler.get_iid h)
        in
        print_tbl title (Exp.tabulate (Syntax.Handler.get_body h)));
    print_tbl "TABULATE: main" (Exp.tabulate pgm.main));
  (if !opt_dintp then
     Interp.(def_intp pgm |> Domain.Mem.string_of_t |> print_endline));
  (* (if !opt_tintp then Analyzer.(trans_intp pgm |> Mem.string_of_t |>
     print_endline)); (if !opt_dintp then Analyzer.(def_intp pgm |>
     Mem.string_of_t |> print_endline)); (if !opt_analyze_detail then
     Analyzer.(analysis pgm |> Abs_sem.string_of_t |> print_endline)); (if
     !opt_analyze then Analyzer.(analysis pgm |> find_watermark |>
     print_endline)); *)
  (if !opt_analyze_detail then
     Analyzer.(abs_def_intp pgm |> Abs_dom.Abs_Sem.string_of_t |> print_endline));
  (if !opt_summary then
     let _ = Analyzer.init_confa pgm in
     ());
  (if !opt_prov || !opt_report then begin
     let t0 = Unix.gettimeofday () in
     let use_compile_opt = not (!opt_optoff || !opt_compileoff) in
     let use_selective_opt = not (!opt_optoff || !opt_selectoff) in
     let asem, errs =
       Analyzer.abs_analyze ~use_compile_opt ~use_selective_opt pgm
     in
     let t1 = Unix.gettimeofday () in
     let merged_errs = Trace.merge_errors errs in
     let trace_chains = Trace.trace_warnings merged_errs asem pgm in
     let t2 = Unix.gettimeofday () in
     Printf.eprintf "[time] abs_analyze : %.3fs\n" (t1 -. t0);
     Printf.eprintf "[time] trace_analyze: %.3fs\n" (t2 -. t1);
     Printf.eprintf "[count] oob_candidates: %d\n" (Abs_dom.ErrorSet.cardinal merged_errs);
     Printf.eprintf "[count] warnings      : %d\n" (List.length trace_chains);
     if !opt_prov then
       print_endline (Trace.string_of_report trace_chains);
     if !opt_report then
       let path = Option.value !report_out ~default:(default_report_path ()) in
       let t3 = Unix.gettimeofday () in
       let markdown = Reporter.explain ~source_code trace_chains in
       let t4 = Unix.gettimeofday () in
       Printf.eprintf "[time] codex_exec  : %.3fs\n" (t4 -. t3);
       write_all path markdown;
       Printf.eprintf "[report] wrote %s\n" path
   end);
  if
    not
      (!opt_pp || !opt_tab || !opt_tintp || !opt_dintp || !opt_analyze
     || !opt_analyze_detail || !opt_prov || !opt_report || !opt_summary)
  then print_endline "Please provide an option! (-pp, -tab, -intp, -analyze, -prov, -report)"

let () = main ()
