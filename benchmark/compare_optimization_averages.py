#!/usr/bin/env python3
"""
Run optimization comparison benchmarks repeatedly and write a Markdown report.

The report uses NoOpt as the baseline:
  NoOpt           = -optoff     (selective off, compilation off)
  Selective only  = -compileoff (selective on,  compilation off)
  Compilation only= -selectoff  (selective off, compilation on)
  Both            = default     (selective on,  compilation on)

Speedup columns are computed as: NoOpt average time / mode average time.
"""

from __future__ import annotations

import argparse
import statistics
import sys
from datetime import datetime
from pathlib import Path

import run_benchmarks


MODES = [
    ("noopt", "NoOpt", "-optoff", "selective off, compilation off"),
    ("selective", "Selective only", "-compileoff", "selective on, compilation off"),
    ("compilation", "Compilation only", "-selectoff", "selective off, compilation on"),
    ("both", "Both", None, "selective on, compilation on"),
]


def fmt_time(value: float) -> str:
    return f"{value:.3f}"


def fmt_speedup(noopt: float, value: float) -> str:
    if value <= 0:
        return "-"
    return f"{noopt / value:.2f}x"


def fmt_stdev(values: list[float]) -> str:
    if len(values) < 2:
        return "0.000"
    return f"{statistics.stdev(values):.3f}"


def run_one(binary: str, path: Path, flag: str | None) -> dict:
    stdout, stderr = run_benchmarks.run_analyzer(binary, path, extra_flag=flag)
    parsed = run_benchmarks.parse_output(stdout, stderr)
    parsed["time"] = parsed["abs_time"] + parsed["trace_time"]
    return parsed


def collect(binary: str, bench_dir: str, runs: int) -> list[dict]:
    paths = sorted(Path(bench_dir).glob("*.si"), key=run_benchmarks._rank)
    rows: list[dict] = []
    total_jobs = len(paths) * len(MODES) * runs
    done = 0

    for path in paths:
        row: dict = {
            "name": path.stem,
            "loc": sum(1 for _ in open(path)),
            "modes": {},
        }
        for key, label, flag, _description in MODES:
            times: list[float] = []
            candidates: list[int] = []
            warnings: list[int] = []
            for run_idx in range(1, runs + 1):
                done += 1
                print(
                    f"[{done:3d}/{total_jobs}] {path.name} | {label} | run {run_idx}/{runs}",
                    file=sys.stderr,
                    flush=True,
                )
                parsed = run_one(binary, path, flag)
                times.append(parsed["time"])
                candidates.append(parsed["candidates"])
                warnings.append(parsed["bugs"])

            row["modes"][key] = {
                "label": label,
                "times": times,
                "avg": statistics.mean(times),
                "stdev": statistics.stdev(times) if len(times) > 1 else 0.0,
                "candidates": candidates[-1],
                "warnings": warnings[-1],
                "stable_counts": len(set(candidates)) == 1 and len(set(warnings)) == 1,
            }
        rows.append(row)

    return rows


def render_markdown(rows: list[dict], runs: int, binary: str, bench_dir: str) -> str:
    lines: list[str] = []
    lines.append("# Optimization Performance Comparison")
    lines.append("")
    lines.append(f"- Runs per benchmark/mode: {runs}")
    lines.append(f"- Analyzer: `{binary}`")
    lines.append(f"- Benchmark directory: `{bench_dir}`")
    lines.append(f"- Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    lines.append("")
    lines.append("Speedups use `NoOpt avg / mode avg`; larger is faster relative to no optimization.")
    lines.append("")
    lines.append("| Mode | Analyzer flags | Meaning |")
    lines.append("|---|---|---|")
    for _key, label, flag, description in MODES:
        flag_text = "`default`" if flag is None else f"`{flag}`"
        lines.append(f"| {label} | {flag_text} | {description} |")
    lines.append("")
    lines.append(
        "| Benchmark | LOC | Cand | Warn | NoOpt avg | Selective avg | Selective speedup | "
        "Compilation avg | Compilation speedup | Both avg | Both speedup |"
    )
    lines.append("|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|")

    for row in rows:
        modes = row["modes"]
        noopt_avg = modes["noopt"]["avg"]
        cand = modes["both"]["candidates"]
        warn = modes["both"]["warnings"]
        lines.append(
            "| {name} | {loc} | {cand} | {warn} | {noopt} | {sel} | {sel_spd} | "
            "{comp} | {comp_spd} | {both} | {both_spd} |".format(
                name=row["name"],
                loc=row["loc"],
                cand=cand,
                warn=warn,
                noopt=fmt_time(noopt_avg),
                sel=fmt_time(modes["selective"]["avg"]),
                sel_spd=fmt_speedup(noopt_avg, modes["selective"]["avg"]),
                comp=fmt_time(modes["compilation"]["avg"]),
                comp_spd=fmt_speedup(noopt_avg, modes["compilation"]["avg"]),
                both=fmt_time(modes["both"]["avg"]),
                both_spd=fmt_speedup(noopt_avg, modes["both"]["avg"]),
            )
        )

    lines.append("")
    lines.append("## Timing Detail")
    lines.append("")
    lines.append("| Benchmark | Mode | Avg (s) | Stddev (s) | Runs (s) | Count stable |")
    lines.append("|---|---|---:|---:|---|---:|")
    for row in rows:
        for key, _label, _flag, _description in MODES:
            mode = row["modes"][key]
            runs_text = ", ".join(fmt_time(t) for t in mode["times"])
            stable = "yes" if mode["stable_counts"] else "no"
            lines.append(
                f"| {row['name']} | {mode['label']} | {fmt_time(mode['avg'])} | "
                f"{fmt_stdev(mode['times'])} | {runs_text} | {stable} |"
            )

    lines.append("")
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Run repeated optimization comparison benchmarks and write Markdown."
    )
    parser.add_argument("--runs", type=int, default=5, help="runs per benchmark/mode")
    parser.add_argument("--dir", default=run_benchmarks.BENCH_DIR_DEFAULT, metavar="DIR")
    parser.add_argument("--bin", default=run_benchmarks.BINARY_DEFAULT, metavar="PATH")
    parser.add_argument(
        "--out",
        default="benchmark/optimization_performance_5run.md",
        metavar="PATH",
        help="Markdown output path",
    )
    args = parser.parse_args()

    if args.runs <= 0:
        raise SystemExit("--runs must be positive")
    if not Path(args.bin).exists():
        raise SystemExit(f"analyzer binary not found: {args.bin}; build with dune build")
    if not Path(args.dir).is_dir():
        raise SystemExit(f"benchmark directory not found: {args.dir}")

    rows = collect(args.bin, args.dir, args.runs)
    markdown = render_markdown(rows, args.runs, args.bin, args.dir)
    out = Path(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(markdown, encoding="utf-8")
    print(f"wrote {out}", file=sys.stderr)


if __name__ == "__main__":
    main()
