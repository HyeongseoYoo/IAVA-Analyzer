open Variable_analysis

let pp n = Domain.ProgramPoint.Label (Syntax.Exp.Lbl.Main n)
let int_v n = (Itv.alpha n, Abs_dom.Abs_Unit.bot, Abs_dom.Abs_Loc.bot)

let assert_itv msg expected actual =
  if Itv.compare expected actual <> 0 then
    failwith
      (Printf.sprintf "%s: expected %s, got %s" msg (Itv.string_of_t expected)
         (Itv.string_of_t actual))

let assert_int_value msg expected ((actual, _, _) : Abs_dom.Abs_Val.t) =
  assert_itv msg (Itv.alpha expected) actual

let assert_loc_value msg expected ((_, _, actual) : Abs_dom.Abs_Val.t) =
  if Abs_dom.Abs_Loc.compare expected actual <> 0 then
    failwith
      (Printf.sprintf "%s: expected %s, got %s" msg
         (Abs_dom.Abs_Loc.string_of_t expected)
         (Abs_dom.Abs_Loc.string_of_t actual))

let heap_loc lbl n =
  Abs_dom.Abs_Loc.AHeapLoc { lbl; offset = Itv.alpha n }

let test_heap_strong_singleton_write () =
  let lbl = Syntax.Exp.Lbl.Main 10 in
  let cell0 = heap_loc lbl 0 in
  let m0 = Abs_dom.Abs_Mem.bot in
  let m1 = Abs_dom.Abs_Mem.write m0 cell0 (int_v 0) (pp 1) in
  let m2 = Abs_dom.Abs_Mem.write m1 cell0 (int_v 7) (pp 2) in
  let v, pps = Abs_dom.Abs_Mem.find m2 cell0 in
  assert_int_value "singleton heap write should overwrite old value" 7 v;
  if not (Abs_dom.PPSet.equal pps (Abs_dom.PPSet.singleton (pp 2))) then
    failwith "singleton heap write should replace provenance"

let test_heap_interval_write_updates_overlapping_cells () =
  let lbl = Syntax.Exp.Lbl.Main 20 in
  let cell0 = heap_loc lbl 0 in
  let cell1 = heap_loc lbl 1 in
  let cell2 = heap_loc lbl 2 in
  let target =
    Abs_dom.Abs_Loc.AHeapLoc
      { lbl; offset = Itv.Itv (Itv.Bound.Z 1, Itv.Bound.Z 10) }
  in
  let m0 = Abs_dom.Abs_Mem.bot in
  let m1 = Abs_dom.Abs_Mem.write m0 cell0 (int_v 0) (pp 1) in
  let m2 = Abs_dom.Abs_Mem.write m1 cell1 (int_v 1) (pp 1) in
  let m3 = Abs_dom.Abs_Mem.write m2 cell2 (int_v 2) (pp 1) in
  let m4 = Abs_dom.Abs_Mem.write m3 target (int_v 9) (pp 2) in
  let v0, _ = Abs_dom.Abs_Mem.find m4 cell0 in
  let v1, _ = Abs_dom.Abs_Mem.find m4 cell1 in
  let v2, _ = Abs_dom.Abs_Mem.find m4 cell2 in
  assert_int_value "non-overlapping cell should be unchanged" 0 v0;
  assert_itv "overlapping cell 1 should be weakly joined"
    (Itv.Itv (Itv.Bound.Z 1, Itv.Bound.Z 9))
    (let i, _, _ = v1 in
     i);
  assert_itv "overlapping cell 2 should be weakly joined"
    (Itv.Itv (Itv.Bound.Z 2, Itv.Bound.Z 9))
    (let i, _, _ = v2 in
     i)

let test_top_write_updates_existing_locations () =
  let lbl = Syntax.Exp.Lbl.Main 30 in
  let cell0 = heap_loc lbl 0 in
  let var_x = Abs_dom.Abs_Loc.get "X" in
  let m0 = Abs_dom.Abs_Mem.bot in
  let m1 = Abs_dom.Abs_Mem.write m0 cell0 (int_v 0) (pp 1) in
  let m2 = Abs_dom.Abs_Mem.write m1 var_x (int_v 1) (pp 1) in
  let m3 = Abs_dom.Abs_Mem.write m2 Abs_dom.Abs_Loc.Top (int_v 9) (pp 2) in
  let v_cell, _ = Abs_dom.Abs_Mem.find m3 cell0 in
  let v_x, _ = Abs_dom.Abs_Mem.find m3 var_x in
  let v_top, _ = Abs_dom.Abs_Mem.find m3 Abs_dom.Abs_Loc.Top in
  assert_itv "Top write should join into existing heap cell"
    (Itv.Itv (Itv.Bound.Z 0, Itv.Bound.Z 9))
    (let i, _, _ = v_cell in
     i);
  assert_itv "Top write should join into existing variable"
    (Itv.Itv (Itv.Bound.Z 1, Itv.Bound.Z 9))
    (let i, _, _ = v_x in
     i);
  assert_int_value "Top write should keep a Top entry" 9 v_top

let parse s = Parser.prog Lexer.read (Lexing.from_string s)

let run_abs s =
  let pgm = parse s in
  let c0 = Analyzer.init_confa pgm in
  let _, c_final = Analyzer.evalA c0 pgm.main in
  c_final

let test_analyzer_reads_singleton_cell () =
  let c =
    run_abs
      {|
init {
  A := malloc(2, 0)
}

main {
  *A[0] := 1;
  *A[1] := 2;
  X := *A[0]
}
|}
  in
  let v, _ = Abs_dom.Abs_Mem.find c.Analyzer.amem (Abs_dom.Abs_Loc.get "X") in
  assert_int_value "reading A[0] should not join A[1]" 1 v

let test_analyzer_reads_through_address_of () =
  let c =
    run_abs
      {|
init {
  X := 1;
  P := &X
}

main {
  Y := *P[0]
}
|}
  in
  let v, _ = Abs_dom.Abs_Mem.find c.Analyzer.amem (Abs_dom.Abs_Loc.get "Y") in
  assert_int_value "reading through &X should read X" 1 v

let test_analyzer_writes_through_address_of () =
  let c =
    run_abs
      {|
init {
  X := 1;
  P := &X
}

main {
  *P[0] := 2;
  Y := X
}
|}
  in
  let vx, _ = Abs_dom.Abs_Mem.find c.Analyzer.amem (Abs_dom.Abs_Loc.get "X") in
  let vy, _ = Abs_dom.Abs_Mem.find c.Analyzer.amem (Abs_dom.Abs_Loc.get "Y") in
  assert_int_value "writing through &X should update X" 2 vx;
  assert_int_value "reading X after address write should see update" 2 vy

let test_analyzer_compares_address_of () =
  let c =
    run_abs
      {|
init {
  X := 1
}

main {
  Same := &X = &X;
  Diff := &X = &Same
}
|}
  in
  let same, _ =
    Abs_dom.Abs_Mem.find c.Analyzer.amem (Abs_dom.Abs_Loc.get "Same")
  in
  let diff, _ =
    Abs_dom.Abs_Mem.find c.Analyzer.amem (Abs_dom.Abs_Loc.get "Diff")
  in
  assert_int_value "&X = &X should be true" 1 same;
  assert_int_value "&X = &Same should be false" 0 diff

let test_compiled_handler_preserves_address_of_scalar_assignment () =
  let c =
    run_abs
      {|
init {
  X := 0;
  Y := 0;
  handler 0 {
    X := &Y
  }
}

main {
  Tick := 0;
  Addr := X
}
|}
  in
  let addr, _ =
    Abs_dom.Abs_Mem.find c.Analyzer.amem (Abs_dom.Abs_Loc.get "Addr")
  in
  assert_loc_value "compiled handler scalar assignment should preserve &Y"
    (Abs_dom.Abs_Loc.get "Y") addr

let test_nonzero_offset_through_address_of_raises () =
  match
    run_abs
      {|
init {
  X := 0;
  P := &X
}

main {
  Y := *P[1]
}
|}
  with
  | exception Interp.Runtime_error _ -> ()
  | _ ->
      failwith
        "expected Runtime_error for non-zero variable-address dereference"

let test_compiled_handler_preserves_address_of_array_write () =
  let c =
    run_abs
      {|
init {
  X := 0;
  Book := malloc(1, 0);
  *Book[0] := &X;

  handler 0 {
    *Book[0] := &X
  }
}

main {
  Tick := 0;
  Addr := *Book[0]
}
|}
  in
  let addr, _ =
    Abs_dom.Abs_Mem.find c.Analyzer.amem (Abs_dom.Abs_Loc.get "Addr")
  in
  assert_loc_value "compiled handler should preserve &X value"
    (Abs_dom.Abs_Loc.get "X") addr

let () =
  test_heap_strong_singleton_write ();
  test_heap_interval_write_updates_overlapping_cells ();
  test_top_write_updates_existing_locations ();
  test_analyzer_reads_singleton_cell ();
  test_analyzer_reads_through_address_of ();
  test_analyzer_writes_through_address_of ();
  test_analyzer_compares_address_of ();
  test_compiled_handler_preserves_address_of_array_write ();
  test_compiled_handler_preserves_address_of_scalar_assignment ();
  test_nonzero_offset_through_address_of_raises ()
