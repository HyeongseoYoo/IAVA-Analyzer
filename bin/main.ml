open Variable_analysis

let src = ref ""
let opt_pp = ref false
let opt_tab = ref false
let opt_tintp = ref false
let opt_dintp = ref false
let opt_analyze = ref false
let opt_analyze_detail = ref false

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
    ]
    (fun x -> src := x)
    ("Usage : " ^ Filename.basename Sys.argv.(0) ^ " [-option] [filename] ");
  let lexbuf =
    Lexing.from_channel (if !src = "" then stdin else open_in !src)
  in
  let pgm = Parser.prog Lexer.read lexbuf in
  let open Syntax.Program in
  if !opt_pp then string_of_t pgm |> print_endline;
  if !opt_tab then (
    let open Syntax in
    let open Exp in
    let print_tbl (title : string) (tbl : Exp.t Exp.Lbl_map.t) : unit =
      Printf.printf "==== %s ====\n" title;
      tbl |> Exp.Lbl_map.bindings |> List.iter (fun (k, v) ->
             Printf.printf "%s -> %s\n" (Exp.Lbl_map.string_of_t k) (Exp.string_of_t v));
      print_endline ""
    in
    print_tbl "TABULATE: global" (Exp.tabulate pgm.global);
    pgm.handler
    |> List.iter (fun (h : Syntax.Handler.t) ->
           let title = Printf.sprintf "TABULATE: handler %d" (Syntax.Handler.get_iid h) in
           print_tbl title (Exp.tabulate (Syntax.Handler.get_body h)));
    print_tbl "TABULATE: main" (Exp.tabulate pgm.main)
  );
  (if !opt_dintp then
     Analyzer.(def_intp pgm |> Domain.Mem.string_of_t |> print_endline));
  (* (if !opt_tintp then
     Analyzer.(trans_intp pgm |> Mem.string_of_t |> print_endline));
  (if !opt_dintp then
     Analyzer.(def_intp pgm |> Mem.string_of_t |> print_endline));
  (if !opt_analyze_detail then
     Analyzer.(analysis pgm |> Abs_sem.string_of_t |> print_endline));
  (if !opt_analyze then
     Analyzer.(analysis pgm |> find_watermark |> print_endline)); *)
  if
    not
      (!opt_pp || !opt_tab || !opt_tintp || !opt_dintp || !opt_analyze
     || !opt_analyze_detail)
  then print_endline "Please provide an option! (-pp, -tab, -intp, -analyze)"

let () = main ()
