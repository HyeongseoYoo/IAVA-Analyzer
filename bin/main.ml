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
  if !opt_tab then
    Syntax.Tabulate.(pgm |> tabulate_all |> iter
      (fun k e -> Label.string_of_t k ^ " " ^ Syntax.Exp.string_of_t e |> print_endline));
  (* (if !opt_tab then
     tabulate_all pgm
     |> Lbl_map.(
          iter (fun l c ->
              string_of_key l ^ " ↦ " ^ string_of_t c |> print_endline))); *)
  (* (if !opt_tintp then
     Analyzer.(trans_intp pgm |> Mem.string_of_t |> print_endline));
  (if !opt_dintp then
     Analyzer.(def_intp pgm |> Mem.string_of_t |> print_endline));
  (if !opt_analyze_detail then
     Analyzer.(analysis pgm |> Abs_sem.string_of_t |> print_endline));
  (if !opt_analyze then
     Analyzer.(analysis pgm |> find_watermark |> print_endline)); *)
  if not (!opt_pp || !opt_tab || !opt_tintp || !opt_dintp || !opt_analyze || !opt_analyze_detail) then
    print_endline "Please provide an option! (-pp, -tab, -intp, -analyze)"

let () = main ()

