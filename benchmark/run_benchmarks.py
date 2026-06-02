#!/usr/bin/env python3
"""
run_benchmarks.py — run the interrupt-aware analyzer on every *.si benchmark
and print a result table suitable for inclusion in a paper.

Usage (from project root):
    python3 benchmark/run_benchmarks.py            # ASCII table to stdout
    python3 benchmark/run_benchmarks.py --md       # GitHub-Flavored Markdown
    python3 benchmark/run_benchmarks.py --latex    # LaTeX tabular block
    python3 benchmark/run_benchmarks.py --csv      # CSV
    python3 benchmark/run_benchmarks.py --all      # include all output formats

Options:
    --md        GitHub-Flavored Markdown table
    --latex     LaTeX tabular (requires booktabs)
    --csv       CSV (header row included)
    --dir DIR   benchmark directory  [default: benchmark/]
    --bin PATH  analyzer binary      [default: ./_build/default/bin/main.exe]

Notes:
  * The analyzer is run with the -prov flag, which reports only
    interrupt-caused out-of-bounds accesses.
  * micro4_nonhandler_oob and the composite true-negative scans are
    intentionally non-handler-caused; they will show 0 interrupt warnings,
    which is the correct (passing) result.
  * Analysis of composite files can take ~30-40 seconds each.
"""

import argparse
import re
import subprocess
import sys
import time
from pathlib import Path

# ── configuration ────────────────────────────────────────────────────────────

BINARY_DEFAULT = "./_build/default/bin/main.exe"
BENCH_DIR_DEFAULT = "benchmark"

# Preferred display order — files are sorted by the first prefix they match.
_ORDER = [
    "micro1_direct_buggy",
    "micro1_direct_fixed",
    "micro2_alias1_buggy",
    "micro2_alias1_fixed",
    "micro3_aliasmulti_buggy",
    "micro3_aliasmulti_fixed",
    "micro4_nonhandler_oob",
    "composite1",
    "composite2",
]


def _rank(path: Path) -> int:
    name = path.stem
    for i, prefix in enumerate(_ORDER):
        if name.startswith(prefix):
            return i
    return len(_ORDER)


# ── running + parsing ─────────────────────────────────────────────────────────

def run_analyzer(binary: str, bench_path: Path) -> tuple[str, str]:
    """Return (stdout, stderr) from running `binary -prov bench_path`."""
    result = subprocess.run(
        [binary, "-prov", str(bench_path)],
        capture_output=True,
        text=True,
    )
    return result.stdout, result.stderr


def parse_output(stdout: str, stderr: str) -> dict:
    """
    Parse analyzer output into a structured dict:
      bugs       int        — interrupt-caused bugs found
      handlers   list[int]  — handler ids that caused a bug (deduplicated)
      oob_rights list[str]  — right-OOB range per bug (e.g. "[8,255]", "⟂")
      abs_time   float      — abs_analyze seconds
      prov_time  float      — prov_analyze seconds
    """
    # Total count
    m = re.search(r"Provenance Report: (\d+) bugs? found", stdout)
    bugs = int(m.group(1)) if m else 0

    # Per-bug details: split on "--- Bug #N ---" markers
    handlers = []
    oob_rights = []
    for block in re.split(r"--- Bug #\d+ ---", stdout)[1:]:
        hm = re.search(r"\[caused by interrupt: handler (\d+)\]", block)
        if hm:
            handlers.append(int(hm.group(1)))
        rm = re.search(r"right OOB: (\S+)", block)
        oob_rights.append(rm.group(1) if rm else "?")

    # Timings from stderr
    abs_time = prov_time = 0.0
    for line in stderr.splitlines():
        if m := re.search(r"\[time\] abs_analyze\s*:\s*([\d.]+)s", line):
            abs_time = float(m.group(1))
        if m := re.search(r"\[time\] prov_analyze\s*:\s*([\d.]+)s", line):
            prov_time = float(m.group(1))

    return {
        "bugs": bugs,
        "handlers": sorted(set(handlers)),
        "oob_rights": oob_rights,
        "abs_time": abs_time,
        "prov_time": prov_time,
    }


def _fmt_handlers(handlers: list[int]) -> str:
    return ", ".join(f"H{h}" for h in handlers) if handlers else "—"


def _fmt_oob(oob_rights: list[str]) -> str:
    if not oob_rights:
        return "—"
    seen: dict[str, None] = {}
    for r in oob_rights:
        seen[r] = None
    return " / ".join(seen)


def _fmt_time(abs_t: float, prov_t: float) -> str:
    return f"{abs_t + prov_t:.3f}"


# ── data collection ───────────────────────────────────────────────────────────

def collect_rows(binary: str, bench_dir: str) -> list[dict]:
    paths = sorted(Path(bench_dir).glob("*.si"), key=_rank)
    rows = []
    total = len(paths)
    for i, path in enumerate(paths, 1):
        print(f"  [{i:2d}/{total}] {path.name} ...", end=" ", flush=True, file=sys.stderr)
        loc = sum(1 for _ in open(path))
        t0 = time.perf_counter()
        stdout, stderr = run_analyzer(binary, path)
        wall = time.perf_counter() - t0
        parsed = parse_output(stdout, stderr)
        print(f"{wall:.1f}s", file=sys.stderr)
        rows.append({
            "name":     path.stem,
            "loc":      loc,
            "bugs":     parsed["bugs"],
            "handlers": _fmt_handlers(parsed["handlers"]),
            "oob":      _fmt_oob(parsed["oob_rights"]),
            "time":     _fmt_time(parsed["abs_time"], parsed["prov_time"]),
        })
    return rows


# ── table rendering ───────────────────────────────────────────────────────────

HEADERS = ["Benchmark",  "LOC", "Int. warnings", "Caused by", "Right OOB",  "Time (s)"]
KEYS    = ["name",       "loc", "bugs",           "handlers",  "oob",        "time"]
ALIGNS  = ["l",          "r",   "r",              "l",         "l",          "r"]


def _cells(rows: list[dict]) -> list[list[str]]:
    out = [list(HEADERS)]
    for row in rows:
        out.append([str(row[k]) for k in KEYS])
    return out


def _col_widths(cells: list[list[str]]) -> list[int]:
    return [max(len(cells[r][c]) for r in range(len(cells)))
            for c in range(len(HEADERS))]


def _pad(s: str, w: int, align: str) -> str:
    return s.rjust(w) if align == "r" else s.ljust(w)


def render_ascii(rows: list[dict]) -> str:
    cells = _cells(rows)
    widths = _col_widths(cells)
    sep = "+-" + "-+-".join("-" * w for w in widths) + "-+"
    lines = [sep]
    for i, row_cells in enumerate(cells):
        padded = [_pad(c, w, a) for c, w, a in zip(row_cells, widths, ALIGNS)]
        lines.append("| " + " | ".join(padded) + " |")
        if i == 0:
            lines.append(sep)
    lines.append(sep)
    return "\n".join(lines)


def render_md(rows: list[dict]) -> str:
    cells = _cells(rows)
    widths = _col_widths(cells)
    lines = []
    # header
    lines.append("| " + " | ".join(_pad(c, w, a)
                                   for c, w, a in zip(cells[0], widths, ALIGNS)) + " |")
    # separator
    seps = [("-" * w + ":") if a == "r" else ("-" * (w + 1))
            for w, a in zip(widths, ALIGNS)]
    lines.append("|" + "|".join(seps) + "|")
    # data rows — insert blank separator between micro and composite sections
    prev_kind = None
    for row, row_cells in zip(rows, cells[1:]):
        kind = "composite" if row["name"].startswith("composite") else "micro"
        if prev_kind is not None and kind != prev_kind:
            lines.append("|" + "|".join(" " + "-" * w + " " for w in widths) + "|")
        prev_kind = kind
        padded = [_pad(c, w, a) for c, w, a in zip(row_cells, widths, ALIGNS)]
        lines.append("| " + " | ".join(padded) + " |")
    return "\n".join(lines)


def render_latex(rows: list[dict]) -> str:
    col_spec = "".join(ALIGNS)
    header = " & ".join(f"\\textbf{{{h}}}" for h in HEADERS) + r" \\"
    lines = [
        r"\begin{tabular}{" + col_spec + "}",
        r"\toprule",
        header,
        r"\midrule",
    ]
    prev_kind = None
    for row in rows:
        kind = "composite" if row["name"].startswith("composite") else "micro"
        if prev_kind is not None and kind != prev_kind:
            lines.append(r"\midrule")
        prev_kind = kind
        name_tex = row["name"].replace("_", r"\_")
        cells = [name_tex] + [str(row[k]) for k in KEYS[1:]]
        lines.append(" & ".join(cells) + r" \\")
    lines += [r"\bottomrule", r"\end{tabular}"]
    return "\n".join(lines)


def render_csv(rows: list[dict]) -> str:
    def q(s: str) -> str:
        return f'"{s}"' if "," in s or '"' in s else s
    lines = [",".join(HEADERS)]
    for row in rows:
        lines.append(",".join(q(str(row[k])) for k in KEYS))
    return "\n".join(lines)


# ── main ──────────────────────────────────────────────────────────────────────

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
    parser.add_argument("--all", action="store_true",
                        help="print all formats (ASCII + Markdown + LaTeX)")
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

    print(f"Analyzer : {args.bin}", file=sys.stderr)
    print(f"Directory: {args.dir}/", file=sys.stderr)
    print(file=sys.stderr)

    rows = collect_rows(args.bin, args.dir)
    print(file=sys.stderr)

    if args.all:
        print("=== ASCII ===")
        print(render_ascii(rows))
        print("\n=== Markdown ===")
        print(render_md(rows))
        print("\n=== LaTeX ===")
        print(render_latex(rows))
    elif args.latex:
        print(render_latex(rows))
    elif args.csv:
        print(render_csv(rows))
    elif args.md:
        print(render_md(rows))
    else:
        print(render_ascii(rows))


if __name__ == "__main__":
    main()
