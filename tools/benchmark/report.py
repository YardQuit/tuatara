#!/usr/bin/env python3
# tools/benchmark/report.py --- Write the editor benchmark's results as a Markdown report.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Turn the records tools/benchmark/harness.py wrote into a Markdown report.

Usage: python3 -I tools/benchmark/report.py RESULTS.jsonl > REPORT.md

The report names the machine, the editors' versions and how each was set up,
the files, how the measuring was done, then a table for each file with every
number, every run that failed and why, and a list of every case where
Tuatara is not the fastest or the smallest. tools/benchmark/run.sh runs it.
"""

import json
import statistics
import sys

MIB = 1024 * 1024
BUSY = float("inf")

RULES = """\
- Every editor runs in its terminal mode, in the same {rows} by {columns}
  pseudo-terminal (TERM=xterm-256color), driven by the same harness
  (tools/benchmark/harness.py, Python standard library only), on the same
  files, pinned to the same processors in a systemd user scope whose memory
  is capped as the reference machine's: `{wrap}`.
- Each editor has an empty home of its own, made afresh for every run
  (HOME, the XDG directories, TMPDIR and GNUPGHOME all inside it), so no one's
  own settings are read. Each has its stock setup for large files, as listed
  above. Emacs is started without the files a distribution adds to it
  (--no-site-file, inhibit-default-init): Fedora's bring warnings that take
  half its screen. Before each timed run the editor is started once on a
  small file, not timed, so that its caches are made, and the benchmark file
  is read once so that every editor opens it from the page cache.
- Where so-long makes Emacs's buffer read-only, Emacs cannot type into it as
  it stands; that is reported, and a second row, "made writable", turns
  read-only off (C-x C-q) first and times the typing.
- Open is the time from starting the editor to the first screen showing the
  file's first line, shown and idle as for a key below.
- Each key gets two times. Shown runs from writing the key to the terminal
  until the editor's output for it has settled, no output coming for
  {quiet:g} ms (not counted): what a person waits for. Idle runs on until, for
  {quiet:g} ms, there was also no output and the editor's processes used no
  processor time (one clock tick at most): the work the key cost, including
  work done in the background after the screen settled. Before each key the
  harness waits for {rest:g} ms with no output, so that redraws an editor puts
  off on purpose are not counted in the next key; an editor that collects
  garbage when typing pauses (Tuatara after 0.3 s) may do so in that wait,
  which a person typing steadily would not give it time for. The tables give
  the median, the 95th percentile and the worst of all keys of all
  repetitions. Typing is {typing} letters and spaces at the start of the
  file, a new line every 50; page down is {pages} screens.
- Search is a word found once, near the end of the file, typed into each
  editor's search one key every 150 ms, as a person types, not timed; the
  final RET is timed until the match is on the screen (shown) and until the
  editor is idle as above. Every editor here searches as you type by default.
- Memory is the largest resident set of the editor's processes together,
  read from /proc while it runs.
- A freeze is any key whose shown time is over a second. A run that takes over {limit:g} s
  for one step is stopped and reported as timed out; an editor that crashes
  or refuses is reported so, never left out.
- The editors take turns in a different order in each of the {repetitions}
  repetitions."""


def milliseconds(value):
    """Return VALUE, a time in milliseconds, written for the table; n/a when there is none."""
    if value is None:
        return "n/a"
    if value == BUSY:
        return "over 10 s"
    if value < 10:
        return f"{value:.1f}"
    if value < 10000:
        return f"{value:.0f}"
    return f"{value / 1000:.1f} s"


def percentile(values, fraction):
    """Return the value of VALUES at FRACTION (0.95 for the 95th percentile), by the nearest rank."""
    ordered = sorted(values)
    return ordered[max(0, min(len(ordered) - 1, round(fraction * len(ordered) + 0.5) - 1))]


def summary(runs):
    """Return the numbers of one editor on one file from its RUNS, one record each repetition."""
    def middle(key):
        values = [r[key] for r in runs if r.get(key) is not None]
        return statistics.median(values) if values else None

    def idle(key):
        values = [BUSY if r.get(key) is None else r[key] for r in runs if r.get(key[:-8] + "_ms") is not None]
        return statistics.median(values) if values else None

    out = {"open": middle("open_ms"), "end": middle("end_ms"), "search": middle("search_ms"),
           "open_idle": idle("open_idle_ms"), "end_idle": idle("end_idle_ms"), "search_idle": idle("search_idle_ms"),
           "memory": max((r.get("peak_rss") or 0 for r in runs), default=0) or None,
           "freezes": sum(r.get("freezes", 0) for r in runs),
           "failures": [f"repetition {r['repetition']}: {r['status']}" for r in runs if r["status"] != "ok"],
           "no_change": {}}
    for case in ("typing", "page", "typing_idle", "page_idle"):
        samples = [BUSY if t is None else t for r in runs for t in r.get(case, [])]
        out[case] = (statistics.median(samples), percentile(samples, 0.95), max(samples)) if samples else None
        out[case + "_count"] = len(samples)
    for r in runs:
        for case, count in r.get("no_change", {}).items():
            out["no_change"][case] = out["no_change"].get(case, 0) + count
    return out


def triple(values):
    """Return a median, 95th percentile and worst written as one table cell."""
    return "n/a" if values is None else " / ".join(milliseconds(v) for v in values)


def losses(tables, editors):
    """Return a line for each case where Tuatara is slower or larger than another editor, or has no result."""
    def key(case, index):
        return lambda s: s[case] and s[case][index]

    measures = [("open", lambda s: s["open"]), ("typing, median", key("typing", 0)),
                ("typing, 95th percentile", key("typing", 1)), ("typing, worst", key("typing", 2)),
                ("page down, median", key("page", 0)), ("page down, 95th percentile", key("page", 1)),
                ("page down, worst", key("page", 2)), ("to the end", lambda s: s["end"]),
                ("search", lambda s: s["search"]), ("memory", lambda s: s["memory"]),
                ("freezes", lambda s: s["freezes"])]
    lines = []
    for kind, table in tables.items():
        ours = table.get("tuatara")
        if ours is None:
            continue
        for label, get in measures:
            mine = get(ours)
            others = [(get(s), e) for e, s in table.items() if e != "tuatara" and get(s) is not None]
            if not others:
                continue
            best, who = min(others)
            name = editors[who]["name"]
            if mine is None:
                lines.append(f"- {kind}, {label}: Tuatara has no result; {name} has {show(label, best)}.")
            elif best < mine:
                lines.append(f"- {kind}, {label}: Tuatara {show(label, mine)}, {name} {show(label, best)}.")
    return lines


def show(label, value):
    """Return VALUE written as the measure LABEL is: memory in MiB, freezes counted, else milliseconds."""
    if label == "memory":
        return f"{value / MIB:.0f} MiB"
    if label == "freezes":
        return str(value)
    return milliseconds(value) + ("" if milliseconds(value).endswith(" s") else " ms")


def main():
    """Read the records named on the command line and print the report."""
    meta, records = None, []
    with open(sys.argv[1]) as f:
        for line in f:
            item = json.loads(line)
            if "meta" in item:
                meta = item["meta"]
            else:
                records.append(item)
    m = meta["machine"]
    editors = meta["editors"]
    out = ["# Large files in four editors", "",
           f"Measured {m['date']} by tools/benchmark/run.sh in the Tuatara repository.", "",
           "## Machine", "",
           "| | |", "|---|---|",
           f"| Processor | {m['cpu']}, {m['threads']} threads, at most {m['max_mhz']} MHz |",
           f"| Memory | {m['memory'] / MIB / 1024:.0f} GiB |",
           f"| Pinning and cap | `{m['wrap']}` |",
           f"| Kernel | Linux {m['kernel']} |",
           f"| Harness | Python {m['python']} |", "",
           "## Editors", "",
           "| Editor | Version | Setup for large files |", "|---|---|---|"]
    for e in editors.values():
        out.append(f"| {e['name']} | {e['version']} | {e['large_file_setup']} |")
    out += ["", "Visual Studio Code is not in the comparison: it is not installed on the machine, and it has no "
            "terminal mode to drive the same way.", "",
            "## Files", "",
            "Made by tools/benchmark/make-data.py from a fixed seed; the same sizes and seed give the same bytes.", "",
            "| File | Size | SHA-256 |", "|---|---|---|"]
    for entry in meta["files"]:
        out.append(f"| {entry['path']} | {entry['size'] / MIB:.1f} MiB | `{entry['sha256'][:16]}` |")
    out += ["", "## How it was measured", "",
            RULES.format(rows=m["rows"], columns=m["columns"], wrap=m["wrap"], quiet=m["quiet_ms"],
                         rest=m["rest_ms"], typing=m["typing_keys"], pages=m["page_keys"], limit=m["limit_s"],
                         repetitions=m["repetitions"]), ""]
    tables = {}
    for entry in meta["files"]:
        kind = entry["kind"]
        tables[kind] = {}
        out += [f"## {entry['path']} ({entry['size'] / MIB:.1f} MiB)", "",
                "Shown: until the screen settled, what a person waits for. Times in milliseconds; for keys "
                "the median / 95th percentile / worst.", "",
                "| Editor | Open | Typing | Page down | To the end | Search | Memory | Freezes |",
                "|---|---|---|---|---|---|---|---|"]
        work = ["", "Idle: until the editor's processes had also stopped using the processor, the work each "
                "step cost, background work included.", "",
                "| Editor | Open | Typing | Page down | To the end | Search |", "|---|---|---|---|---|---|"]
        notes = []
        for key, e in editors.items():
            runs = [r for r in records if r["editor"] == key and r["file"] == kind and r["status"] != "skipped"]
            if not runs:
                continue
            s = summary(runs)
            tables[kind][key] = s
            memory = f"{s['memory'] / MIB:.0f} MiB" if s["memory"] else "n/a"
            out.append(f"| {e['name']} | {milliseconds(s['open'])} | {triple(s['typing'])} | {triple(s['page'])} | "
                       f"{milliseconds(s['end'])} | {milliseconds(s['search'])} | {memory} | {s['freezes']} |")
            work.append(f"| {e['name']} | {milliseconds(s['open_idle'])} | {triple(s['typing_idle'])} | "
                        f"{triple(s['page_idle'])} | {milliseconds(s['end_idle'])} | "
                        f"{milliseconds(s['search_idle'])} |")
            notes += [f"- {e['name']}, {failure}" for failure in s["failures"]]
            for case, count in s["no_change"].items():
                notes.append(f"- {e['name']}: {count} {case} key(s) changed nothing on the screen, not timed.")
        out += work
        if notes:
            out += ["", "Runs that did not go as planned:", ""] + notes
        out.append("")
    out += ["## Where Tuatara loses", "",
            "Every case where another editor is faster or smaller, however slightly; differences of a "
            "millisecond or less are within what the measuring can tell apart.", ""]
    out += losses(tables, editors) or ["- None."]
    print("\n".join(out))


if __name__ == "__main__":
    main()
