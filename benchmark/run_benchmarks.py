#!/usr/bin/env python3
"""
run_benchmarks.py — run the interrupt-aware analyzer on every *.si benchmark
and print a result table suitable for inclusion in a paper.

Usage (from project root):
    python3 benchmark/run_benchmarks.py            # ASCII table to stdout
    python3 benchmark/run_benchmarks.py --md       # GitHub-Flavored Markdown
    python3 benchmark/run_benchmarks.py --latex    # LaTeX tabular block
    python3 benchmark/run_benchmarks.py --csv      # CSV
    python3 benchmark/run_benchmarks.py --all      # all three formats at once

    python3 benchmark/run_benchmarks.py --optoff           # side-by-side opt vs no-opt
    python3 benchmark/run_benchmarks.py --optoff --latex   # same, LaTeX output

Options:
    --md        GitHub-Flavored Markdown table
    --latex     LaTeX tabular (requires booktabs)
    --csv       CSV with header row
    --all       print ASCII + Markdown + LaTeX together
    --optoff    also run each benchmark with -optoff and append result columns
                (Warnings, Left OOB, Right OOB, Time) for the no-opt run plus
                a Speedup column — lets you compare correctness and performance
    --dir DIR   benchmark directory  [default: benchmark/]
    --bin PATH  analyzer binary      [default: ./_build/default/bin/main.exe]

Notes:
  * The analyzer is run with the -prov flag, which reports only
    interrupt-caused out-of-bounds accesses.
  * micro4_nonhandler_oob and the composite true-negative scans are
    intentionally non-handler-caused; they will show 0 interrupt warnings,
    which is the correct (passing) result.
  * Analysis of composite files can take ~30-40 seconds each.
  * --optoff doubles the number of runs; composite files take ~30-40 s each pass.
"""

import argparse
import re
import subprocess
import sys
import time
from pathlib import Path

# ── configuration ─────────────────────────────────────────────────────────────

BINARY_DEFAULT    = "./_build/default/bin/main.exe"
BENCH_DIR_DEFAULT = "benchmark"

_ORDER = [
    "micro1_direct_buggy",
    "micro1_direct_fixed",
    "micro2_alias1_buggy",
    "micro2_alias1_fixed",
    "micro3_aliasmulti_buggy",
    "micro3_aliasmulti_fixed",
    "micro4_nonhandler_oob",
    "composite1_nvme_sqcq",
    "composite1_nvme_sqcq_fixed",
    "composite2_uecc_dma",
    "composite2_uecc_dma_fixed",
]


def _rank(path: Path) -> int:
    name = path.stem
    for i, prefix in enumerate(_ORDER):
        if name == prefix:
            return i
    for i, prefix in enumerate(_ORDER):
        if name.startswith(prefix):
            return i
    return len(_ORDER)


# ── column schemas ─────────────────────────────────────────────────────────────
# Each schema is (headers, row-keys, alignments).  "l" = left, "r" = right.

_SCHEMA_BASE = (
    ["Benchmark",  "LOC", "Warnings", "Caused by", "Left OOB",  "Right OOB",  "Time (s)"],
    ["name",       "loc", "bugs",     "handlers",  "left_oob",  "right_oob",  "time"],
    ["l",          "r",   "r",        "l",         "l",         "l",          "r"],
)

_SCHEMA_OPTOFF = (
    # opt columns                                                   no-opt columns            compare
    ["Benchmark",  "LOC",
     "Warnings",   "Caused by",  "Left OOB",  "Right OOB",  "Time (s)",
     "Warnings*",  "Left OOB*",  "Right OOB*", "Time* (s)",  "Speedup"],
    ["name",       "loc",
     "bugs",       "handlers",   "left_oob",  "right_oob",  "time",
     "bugs2",      "left_oob2",  "right_oob2", "time2",      "speedup"],
    ["l",          "r",
     "r",          "l",          "l",         "l",          "r",
     "r",          "l",          "l",          "r",          "r"],
)
# * columns come from the -optoff (no-opt) run


# ── running + parsing ──────────────────────────────────────────────────────────

def run_analyzer(binary: str, bench_path: Path, optoff: bool = False) -> tuple[str, str]:
    """Return (stdout, stderr) from `binary -prov [-optoff] bench_path`."""
    cmd = [binary, "-prov"]
    if optoff:
        cmd.append("-optoff")
    cmd.append(str(bench_path))
    result = subprocess.run(cmd, capture_output=True, text=True)
    return result.stdout, result.stderr


def parse_output(stdout: str, stderr: str) -> dict:
    """
    Returns:
      bugs       int        — interrupt-caused bugs found
      handlers   list[int]  — handler ids that caused a bug (deduplicated)
      left_oobs  list[str]  — left-OOB range per bug  (e.g. "⟂", "[-∞,-1]")
      right_oobs list[str]  — right-OOB range per bug (e.g. "[8,255]", "⟂")
      abs_time   float      — abs_analyze wall seconds
      trace_time float      — trace_analyze wall seconds
    """
    m = re.search(r"Trace Warning Report: (\d+) warnings?", stdout)
    bugs = int(m.group(1)) if m else 0

    handlers, left_oobs, right_oobs = [], [], []
    for block in re.split(r"--- Warning #\d+ ---", stdout)[1:]:
        # "access: Write, kind: right OOB, interrupt influence: handler 0, handler 1"
        im = re.search(r"interrupt influence:\s*(.+)", block)
        if im:
            handlers.extend(int(h) for h in re.findall(r"handler (\d+)", im.group(1)))
        # "safe=[0,7] left=⟂ right=[8,255]"
        lm = re.search(r"left=(\S+)", block)
        rm = re.search(r"right=(\S+)", block)
        left_oobs.append(lm.group(1) if lm else "?")
        right_oobs.append(rm.group(1) if rm else "?")

    abs_time = trace_time = 0.0
    for line in stderr.splitlines():
        if m2 := re.search(r"\[time\] abs_analyze\s*:\s*([\d.]+)s", line):
            abs_time = float(m2.group(1))
        if m2 := re.search(r"\[time\] trace_analyze\s*:\s*([\d.]+)s", line):
            trace_time = float(m2.group(1))

    return {
        "bugs":      bugs,
        "handlers":  sorted(set(handlers)),
        "left_oobs": left_oobs,
        "right_oobs": right_oobs,
        "abs_time":  abs_time,
        "trace_time": trace_time,
    }


def _fmt_handlers(handlers: list[int]) -> str:
    return ", ".join(f"H{h}" for h in handlers) if handlers else "—"


def _fmt_oob(oobs: list[str]) -> str:
    """Deduplicated OOB ranges joined with ' / ', or '—' if empty."""
    if not oobs:
        return "—"
    seen: dict[str, None] = {}
    for r in oobs:
        seen[r] = None
    return " / ".join(seen)


def _fmt_time(t: float) -> str:
    return f"{t:.3f}"


def _fmt_speedup(t_opt: float, t_noopt: float) -> str:
    if t_opt <= 0:
        return "—"
    return f"{t_noopt / t_opt:.2f}x"


# ── data collection ────────────────────────────────────────────────────────────

def collect_rows(binary: str, bench_dir: str, with_optoff: bool = False) -> list[dict]:
    paths = sorted(Path(bench_dir).glob("*.si"), key=_rank)
    rows = []
    total = len(paths)

    for i, path in enumerate(paths, 1):
        label = f"[{i:2d}/{total}] {path.name}"
        tag   = " (opt)   " if with_optoff else " "

        # optimized pass
        print(f"  {label}{tag}...", end=" ", flush=True, file=sys.stderr)
        t0 = time.perf_counter()
        stdout, stderr = run_analyzer(binary, path, optoff=False)
        wall = time.perf_counter() - t0
        p = parse_output(stdout, stderr)
        t_opt = p["abs_time"] + p["trace_time"]
        print(f"{wall:.1f}s", file=sys.stderr)

        loc = sum(1 for _ in open(path))
        row: dict = {
            "name":      path.stem,
            "loc":       loc,
            "bugs":      p["bugs"],
            "handlers":  _fmt_handlers(p["handlers"]),
            "left_oob":  _fmt_oob(p["left_oobs"]),
            "right_oob": _fmt_oob(p["right_oobs"]),
            "time":      _fmt_time(t_opt),
        }

        # no-opt pass
        if with_optoff:
            print(f"  {label} (no-opt) ...", end=" ", flush=True, file=sys.stderr)
            t0 = time.perf_counter()
            stdout2, stderr2 = run_analyzer(binary, path, optoff=True)
            wall2 = time.perf_counter() - t0
            p2 = parse_output(stdout2, stderr2)
            t_noopt = p2["abs_time"] + p2["trace_time"]
            print(f"{wall2:.1f}s", file=sys.stderr)

            row["bugs2"]      = p2["bugs"]
            row["left_oob2"]  = _fmt_oob(p2["left_oobs"])
            row["right_oob2"] = _fmt_oob(p2["right_oobs"])
            row["time2"]      = _fmt_time(t_noopt)
            row["speedup"]    = _fmt_speedup(t_opt, t_noopt)

        rows.append(row)

    return rows


# ── table rendering ────────────────────────────────────────────────────────────

def _build_cells(rows: list[dict], headers: list[str],
                 keys: list[str]) -> list[list[str]]:
    out = [list(headers)]
    for row in rows:
        out.append([str(row[k]) for k in keys])
    return out


def _col_widths(cells: list[list[str]]) -> list[int]:
    return [max(len(cells[r][c]) for r in range(len(cells)))
            for c in range(len(cells[0]))]


def _pad(s: str, w: int, align: str) -> str:
    return s.rjust(w) if align == "r" else s.ljust(w)


def _is_composite(row: dict) -> bool:
    return row["name"].startswith("composite")


def render_ascii(rows: list[dict], schema: tuple) -> str:
    headers, keys, aligns = schema
    cells  = _build_cells(rows, headers, keys)
    widths = _col_widths(cells)
    sep    = "+-" + "-+-".join("-" * w for w in widths) + "-+"
    lines  = [sep]
    for i, row_cells in enumerate(cells):
        padded = [_pad(c, w, a) for c, w, a in zip(row_cells, widths, aligns)]
        lines.append("| " + " | ".join(padded) + " |")
        if i == 0:
            lines.append(sep)
    lines.append(sep)
    return "\n".join(lines)


def render_md(rows: list[dict], schema: tuple) -> str:
    headers, keys, aligns = schema
    cells  = _build_cells(rows, headers, keys)
    widths = _col_widths(cells)
    lines  = []
    lines.append("| " + " | ".join(
        _pad(c, w, a) for c, w, a in zip(cells[0], widths, aligns)) + " |")
    seps = [("-" * w + ":") if a == "r" else ("-" * (w + 1))
            for w, a in zip(widths, aligns)]
    lines.append("|" + "|".join(seps) + "|")
    prev_kind = None
    for row, row_cells in zip(rows, cells[1:]):
        kind = "composite" if _is_composite(row) else "micro"
        if prev_kind is not None and kind != prev_kind:
            lines.append("|" + "|".join(" " + "-" * w + " " for w in widths) + "|")
        prev_kind = kind
        padded = [_pad(c, w, a) for c, w, a in zip(row_cells, widths, aligns)]
        lines.append("| " + " | ".join(padded) + " |")
    return "\n".join(lines)


def render_latex(rows: list[dict], schema: tuple) -> str:
    headers, keys, aligns = schema
    col_spec   = "".join(aligns)
    header_row = " & ".join(f"\\textbf{{{h}}}" for h in headers) + r" \\"
    lines = [
        r"\begin{tabular}{" + col_spec + "}",
        r"\toprule",
        header_row,
        r"\midrule",
    ]
    prev_kind = None
    for row in rows:
        kind = "composite" if _is_composite(row) else "micro"
        if prev_kind is not None and kind != prev_kind:
            lines.append(r"\midrule")
        prev_kind = kind
        name_tex = row["name"].replace("_", r"\_")
        cells = [name_tex] + [str(row[k]) for k in keys[1:]]
        lines.append(" & ".join(cells) + r" \\")
    lines += [r"\bottomrule", r"\end{tabular}"]
    return "\n".join(lines)


def render_csv(rows: list[dict], schema: tuple) -> str:
    headers, keys, _ = schema

    def q(s: str) -> str:
        return f'"{s}"' if "," in s or '"' in s else s

    lines = [",".join(headers)]
    for row in rows:
        lines.append(",".join(q(str(row[k])) for k in keys))
    return "\n".join(lines)


# ── main ───────────────────────────────────────────────────────────────────────

def main() -> None:
    parser = argparse.ArgumentParser(
        description="Run benchmarks and print a result table.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    fmt = parser.add_mutually_exclusive_group()
    fmt.add_argument("--md",    action="store_true", help="GitHub-Flavored Markdown table")
    fmt.add_argument("--latex", action="store_true", help="LaTeX tabular block (booktabs)")
    fmt.add_argument("--csv",   action="store_true", help="CSV with header row")
    parser.add_argument("--all",    action="store_true",
                        help="print ASCII + Markdown + LaTeX together")
    parser.add_argument("--optoff", action="store_true",
                        help="also run with -optoff; appends Warnings*, Left OOB*, "
                             "Right OOB*, Time* and Speedup columns (* = no-opt run)")
    parser.add_argument("--dir", default=BENCH_DIR_DEFAULT, metavar="DIR",
                        help=f"benchmark directory (default: {BENCH_DIR_DEFAULT})")
    parser.add_argument("--bin", default=BINARY_DEFAULT, metavar="PATH",
                        help=f"analyzer binary (default: {BINARY_DEFAULT})")
    args = parser.parse_args()

    if not Path(args.bin).exists():
        print(f"error: analyzer binary not found: {args.bin}\n"
              f"  build with: dune build", file=sys.stderr)
        sys.exit(1)
    if not Path(args.dir).is_dir():
        print(f"error: benchmark directory not found: {args.dir}", file=sys.stderr)
        sys.exit(1)

    schema = _SCHEMA_OPTOFF if args.optoff else _SCHEMA_BASE

    print(f"Analyzer : {args.bin}", file=sys.stderr)
    print(f"Directory: {args.dir}/", file=sys.stderr)
    if args.optoff:
        print("Mode     : opt vs no-opt comparison  (* columns = -optoff run)",
              file=sys.stderr)
    print(file=sys.stderr)

    rows = collect_rows(args.bin, args.dir, with_optoff=args.optoff)
    print(file=sys.stderr)

    if args.all:
        print("=== ASCII ===")
        print(render_ascii(rows, schema))
        print("\n=== Markdown ===")
        print(render_md(rows, schema))
        print("\n=== LaTeX ===")
        print(render_latex(rows, schema))
    elif args.latex:
        print(render_latex(rows, schema))
    elif args.csv:
        print(render_csv(rows, schema))
    elif args.md:
        print(render_md(rows, schema))
    else:
        print(render_ascii(rows, schema))


if __name__ == "__main__":
    main()
