#!/usr/bin/env python3
# tools/benchmark/harness.py --- Time editors on large files through a pseudo-terminal.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Run each editor on each benchmark file in a pseudo-terminal and time what it does.

Usually run by tools/benchmark/run.sh, which makes the files and pins the
editors to the reference environment's cores and memory. By hand:

  python3 -I tools/benchmark/harness.py --data DIR --scratch DIR --out FILE.jsonl
      [--wrap COMMAND] [--editors emacs,emacs-writable,neovim,helix,tuatara]
      [--files csv,log,org,json] [--repetitions 3] [--typing-keys 100] [--page-keys 40]

DIR for --data is what make-data.py wrote. --scratch is a directory the
harness may empty and fill: each editor's home is made there afresh for each
run. One line of JSON is written to --out for the machine and the versions,
then one for each run of an editor on a file.

How it times: a key is written to the editor's terminal and the time noted;
the editor's output is read as it comes into a screen of --rows by --columns.
Each key gets two times. Shown: until output has come (and, for a search,
the match is on the screen) and then none for --quiet-ms, that quiet spell
not counted; this is what a person waits for, and freezes are counted on it.
Idle: until, for --quiet-ms, there was also no output and the editor's
processes used no processor time (one clock tick at most); this is the work
the key cost, background work included. Before the next key the harness
waits until the editor has written nothing for --rest-ms, so that a redraw
an editor puts off on purpose (a matching parenthesis shown after a delay,
say) does not land in the next key. A search is typed one key every 150 ms,
as a person types, and only its last key is timed. A key the editor shows no
change for is counted as "no change" rather than timed. A shown time over a
second is a freeze; one over --limit seconds ends the run, which is reported
as timed out. Memory is the largest resident set of the editor's processes
together, read from /proc while it runs.
"""

import argparse
import fcntl
import glob
import importlib.util
import json
import os
import platform
import pty
import random
import re
import select
import shlex
import shutil
import signal
import struct
import subprocess
import sys
import termios
import time

HERE = os.path.dirname(os.path.abspath(__file__))


def load(name):
    """Load the module NAME.py that sits beside this file."""
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, name + ".py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


Screen = load("screen").Screen
EDITORS = load("keys").EDITORS
PAGE = os.sysconf("SC_PAGE_SIZE")
FREEZE_MS = 1000


class Exited(Exception):
    """The editor ended while the harness waited for it."""


class NotShown(Exception):
    """The editor sat idle without showing the file it was asked to open."""


class TimedOut(Exception):
    """The editor took longer than the time limit for one step."""


def now():
    """Return the time in seconds from a clock that only goes forward."""
    return time.perf_counter()


def process_tree(pid):
    """Return PID and the process IDs of all its descendants that are still running."""
    found, todo = [], [pid]
    while todo:
        p = todo.pop()
        found.append(p)
        for children in glob.glob(f"/proc/{p}/task/*/children"):
            try:
                with open(children) as f:
                    todo.extend(int(c) for c in f.read().split())
            except OSError:
                pass
    return found


def tree_rss(pid):
    """Return the resident memory of PID and its descendants together, in bytes."""
    total = 0
    for p in process_tree(pid):
        try:
            with open(f"/proc/{p}/statm") as f:
                total += int(f.read().split()[1]) * PAGE
        except (OSError, IndexError, ValueError):
            pass
    return total


def tree_peak(pid):
    """Return the sum of the largest resident sets (VmHWM) PID and its running descendants have had, in bytes."""
    total = 0
    for p in process_tree(pid):
        try:
            with open(f"/proc/{p}/status") as f:
                for line in f:
                    if line.startswith("VmHWM:"):
                        total += int(line.split()[1]) * 1024
        except (OSError, ValueError):
            pass
    return total


def tree_cpu(pid):
    """Return the processor time PID and its descendants have used, in clock ticks."""
    total = 0
    for p in process_tree(pid):
        try:
            with open(f"/proc/{p}/stat") as f:
                fields = f.read().rsplit(")", 1)[1].split()
            total += int(fields[11]) + int(fields[12])
        except (OSError, IndexError, ValueError):
            pass
    return total


class Session:
    """An editor running in a pseudo-terminal of its own, its output read into a Screen."""

    def __init__(self, wrap, argv, env, rows, columns, quiet, rest):
        """Start ARGV with environment ENV in a ROWS by COLUMNS terminal, run by the command WRAP.

        WRAP (a list, maybe empty) runs with the harness's own environment, as
        systemd-run needs to reach the user's service manager; ARGV gets ENV
        alone, by env -i.

        QUIET and REST are in seconds: how long no output means a key is done,
        and how long to wait for quiet before the next key."""
        self.screen = Screen(rows, columns)
        self.quiet = quiet
        self.rest_time = rest
        self.peak_rss = 0
        self.last_output = None
        self.outputs = 0
        self._next_sample = 0
        self.started = now()
        pid, fd = pty.fork()
        if pid == 0:
            try:
                fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", rows, columns, 0, 0))
                command = wrap + ["env", "-i"] + [f"{k}={v}" for k, v in env.items()] + argv
                os.execvp(command[0], command)
            finally:
                os._exit(127)
        self.pid, self.fd = pid, fd
        self.status = None

    def _sample(self):
        """Note the editor's memory, at most every 50 ms."""
        t = now()
        if t >= self._next_sample:
            self.peak_rss = max(self.peak_rss, tree_rss(self.pid))
            self._next_sample = t + 0.05

    def _read(self, timeout):
        """Read what the editor wrote within TIMEOUT seconds into the screen; answer its questions."""
        ready, _, _ = select.select([self.fd], [], [], max(0.0, timeout))
        if not ready:
            return False
        try:
            data = os.read(self.fd, 1 << 16)
        except OSError:
            data = b""
        if not data:
            self._reap()
            raise Exited(self.status)
        self.last_output = now()
        self.outputs += 1
        reply = self.screen.feed(data)
        if reply:
            os.write(self.fd, reply)
        return True

    def _reap(self):
        """Note how the editor ended."""
        if self.status is None:
            try:
                _, status = os.waitpid(self.pid, 0)
                self.status = os.waitstatus_to_exitcode(status)
            except ChildProcessError:
                self.status = "unknown"

    def send(self, keys):
        """Write KEYS to the editor's terminal; return the time just before."""
        start = now()
        os.write(self.fd, keys.encode("utf-8"))
        return start

    def wait(self, since, limit, shows=None, need_output=True, idle_limit=10.0):
        """Wait for what the editor was sent at SINCE; return (first, shown, idle, changed), times or None.

        SHOWN is when the screen settled: output came after SINCE (when
        NEED_OUTPUT), the screen holds SHOWS (when given), and then no output
        came for the quiet time; SHOWN is the start of that quiet spell, which
        is not counted. IDLE is the start of the first later spell of the quiet
        time in which no output came and the editor's processes used no more
        than one clock tick of processor time: when the work for it was done.
        IDLE is None when that takes over IDLE_LIMIT seconds after SHOWN.
        FIRST is the time of the first output after SINCE. CHANGED is False
        when the editor sat idle for over a second without the screen being
        settled as asked: nothing changed on it, or it does not hold SHOWS.
        Raises TimedOut when the screen has not settled after LIMIT seconds
        and Exited when the editor ends."""
        first = shown_at = None
        outputs = self.outputs
        has_text = shows is None
        cpu = tree_cpu(self.pid)
        idle_from, idle_cpu, idle_outputs = since, cpu, outputs
        while True:
            t = now()
            if shown_at is None and t > since + limit:
                raise TimedOut()
            got = self.outputs > outputs
            if got and not has_text and shows in self.screen.text():
                has_text = True
            output_quiet = max(self.last_output or since, since)
            if shown_at is None and (got or not need_output) and has_text and t - output_quiet >= self.quiet:
                shown_at = output_quiet
            cpu = tree_cpu(self.pid)
            if self.outputs != idle_outputs:
                idle_from, idle_cpu, idle_outputs = self.last_output, cpu, self.outputs
            elif cpu - idle_cpu > 1:
                idle_from, idle_cpu = t, cpu
            if t - idle_from >= self.quiet:
                if shown_at is not None:
                    return first, shown_at, max(idle_from, shown_at), True
                if t - idle_from > 1.0:
                    return None, None, None, False
            if shown_at is not None and t - shown_at > idle_limit:
                return first, shown_at, None, True
            self._sample()
            if self._read(0.01) and first is None and self.last_output >= since:
                first = self.last_output

    def rest(self):
        """Wait until the editor has written nothing for the rest time, or two seconds have passed."""
        start = now()
        while now() - start < 2.0:
            left = self.rest_time - (now() - (self.last_output or 0))
            if left <= 0:
                return
            self._sample()
            self._read(min(left, 0.05))

    def key(self, keys, limit, shows=None, paced=""):
        """Send KEYS and wait for the editor; return (first, shown, idle) in ms, or None when nothing changed.

        PACED, when given, is sent first one key at a time, one every 150 ms as
        a person types, not timed; the times are those of KEYS alone. SHOWS is
        as for WAIT; IDLE is None when the editor was still busy ten seconds
        after the screen settled."""
        self.rest()
        for char in paced:
            self.send(char)
            pause = now() + 0.15
            while now() < pause:
                self._sample()
                self._read(pause - now())
        start = self.send(keys)
        first, shown, idle, changed = self.wait(start, limit, shows)
        if not changed:
            return None
        return (((first or shown) - start) * 1000, (shown - start) * 1000,
                None if idle is None else (idle - start) * 1000)

    def untimed(self, keys, limit):
        """Send KEYS, not timed, and wait until the editor is done with them."""
        if keys:
            self.rest()
            self.wait(self.send(keys), limit)

    def close(self):
        """End the editor and all its processes; return the sum of their largest resident sets."""
        peak = tree_peak(self.pid) if self.status is None else 0
        try:
            os.killpg(self.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        for p in process_tree(self.pid)[1:]:
            try:
                os.kill(p, signal.SIGKILL)
            except ProcessLookupError:
                pass
        self._reap()
        os.close(self.fd)
        return peak


def fence(scratch, editor):
    """Make an empty home for EDITOR under SCRATCH and return the environment that points the editor into it.

    Nothing in it leads to the person's own files, settings, agents or display."""
    home = os.path.join(scratch, "fence-" + editor)
    shutil.rmtree(home, ignore_errors=True)
    dirs = {}
    for name in ("home", "config", "state", "data", "cache", "runtime", "tmp", "keys"):
        dirs[name] = os.path.join(home, name)
        os.makedirs(dirs[name], mode=0o700)
    for target, source in EDITORS[editor]["setup"].items():
        path = os.path.join(dirs["home"], target)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        shutil.copyfile(source, path)
    env = {"PATH": "/usr/local/bin:/usr/bin:/bin", "TERM": "xterm-256color", "COLORTERM": "truecolor",
           "LANG": "C.UTF-8", "HOME": dirs["home"], "XDG_CONFIG_HOME": dirs["config"],
           "XDG_STATE_HOME": dirs["state"], "XDG_DATA_HOME": dirs["data"], "XDG_CACHE_HOME": dirs["cache"],
           "XDG_RUNTIME_DIR": dirs["runtime"], "TMPDIR": dirs["tmp"], "GNUPGHOME": dirs["keys"],
           "GIT_CONFIG_GLOBAL": "/dev/null", "GIT_CONFIG_NOSYSTEM": "1"}
    if editor == "emacs" and os.environ.get("BENCH_EMACS_VLF"):
        env["BENCH_EMACS_VLF"] = os.environ["BENCH_EMACS_VLF"]
    return env


def warm_cache(path):
    """Read PATH once, so that every editor opens it from the page cache."""
    with open(path, "rb") as f:
        while f.read(1 << 22):
            pass


def clean_data(directory):
    """Remove the auto-save, lock and backup files editors left beside the benchmark files in DIRECTORY."""
    for name in os.listdir(directory):
        if name.startswith(("#", ".#")) or name.endswith("~") or name.endswith(".swp"):
            os.remove(os.path.join(directory, name))


def typing_text(count, seed):
    """Return COUNT made-up lower-case letters and spaces to type, no eight of them in a row repeated."""
    rng = random.Random(seed)
    while True:
        text = "".join(rng.choice("abcdefghijklmnopqrstuvwxyz   ") for _ in range(count)).replace("  ", " a")
        text = text[:count]
        windows = [text[i:i + 8] for i in range(len(text) - 7)]
        if len(set(windows)) == len(windows) and not text.startswith(" "):
            return text


def run_one(editor, entry, data, args, wrap):
    """Run EDITOR on the benchmark file of manifest ENTRY in DATA; return the run's record."""
    keys = EDITORS[editor]
    path = os.path.join(data, entry["path"])
    env = fence(args.scratch, editor)
    record = {"editor": editor, "file": entry["kind"], "status": "ok", "typing": [], "typing_first": [],
              "page": [], "page_first": [], "typing_idle": [], "page_idle": [], "no_change": {},
              "open_ms": None, "end_ms": None, "search_ms": None, "open_idle_ms": None, "end_idle_ms": None,
              "search_idle_ms": None}
    limit = args.limit
    quiet, rest = args.quiet_ms / 1000, args.rest_ms / 1000
    # one start not timed, on a small file, so that each editor's caches are made
    warm = os.path.join(args.scratch, "warm-up.txt")
    with open(warm, "w") as f:
        f.write("warm up the editor\n")
    session = Session(wrap, keys["command"] + [warm], env, args.rows, args.columns, quiet, rest)
    try:
        if not session.wait(session.started, limit, shows="warm up the editor")[3]:
            raise NotShown()
    except (NotShown, TimedOut, Exited) as e:
        record["status"] = f"warm-up start failed ({type(e).__name__})"
        record["screen_at_end"] = session.screen.text()
        session.close()
        return record
    session.close()
    clean_data(args.scratch)
    warm_cache(path)
    session = Session(wrap, keys["command"] + [path], env, args.rows, args.columns, quiet, rest)
    case = "open"
    problems = []
    try:
        _, shown, idle, changed = session.wait(session.started, limit, shows=entry["open_marker"])
        if not changed:
            raise NotShown()
        record["open_ms"] = (shown - session.started) * 1000
        record["open_idle_ms"] = None if idle is None else (idle - session.started) * 1000
        if keys.get("only_if") and not re.search(keys["only_if"], session.screen.text(), re.MULTILINE):
            record["status"] = "skipped"
            session.close()
            return record
        case = "typing"
        session.untimed(keys["start_typing"], limit)
        text = typing_text(args.typing_keys, f"{entry['kind']}-{editor}")
        typed = ""
        for i, char in enumerate(text):
            if i and i % 50 == 0:
                timing = session.key(keys["newline"], limit)
                typed = ""
            else:
                typed += char
                timing = session.key(char, limit, shows=typed[-8:] if len(typed) >= 8 else None)
            if timing is None:
                record["no_change"]["typing"] = record["no_change"].get("typing", 0) + 1
            else:
                record["typing_first"].append(timing[0])
                record["typing"].append(timing[1])
                record["typing_idle"].append(timing[2])
        # what was typed last must be in the file on the screen, or none of it counts
        if not record["typing"] or (len(typed) >= 8 and typed[-8:] not in session.screen.text()):
            record["typing"], record["typing_first"], record["typing_idle"] = [], [], []
            problems.append("what was typed is not in the file (is it read-only?)")
        session.untimed(keys["stop_typing"], limit)
        case = "page"
        for _ in range(args.page_keys):
            timing = session.key(keys["page_down"], limit)
            if timing is None:
                record["no_change"]["page"] = record["no_change"].get("page", 0) + 1
            else:
                record["page_first"].append(timing[0])
                record["page"].append(timing[1])
                record["page_idle"].append(timing[2])
        case = "end"
        timing = session.key(keys["to_end"], limit)
        if timing is None:
            record["no_change"]["end"] = 1
        else:
            record["end_ms"], record["end_idle_ms"] = timing[1], timing[2]
        session.untimed(keys["to_start"], limit)
        case = "search"
        search = keys["search"](entry["needle"])
        timing = session.key(search[-1], limit, shows=entry["found"], paced=search[:-1])
        if timing is None:
            problems.append("search did not reach the match")
        else:
            record["search_ms"], record["search_idle_ms"] = timing[1], timing[2]
    except NotShown:
        problems.append("did not show the file's first line")
    except TimedOut:
        problems.append(f"timed out in {case} (over {limit:g} s)")
    except Exited as e:
        problems.append(f"ended in {case} (exit {e.args[0]})")
    if problems:
        record["status"] = "; ".join(problems)
    record["peak_rss"] = max(session.peak_rss, session.close())
    record["freezes"] = sum(1 for t in record["typing"] + record["page"] if t > FREEZE_MS) + sum(
        1 for t in (record["end_ms"], record["search_ms"]) if t is not None and t > FREEZE_MS)
    record["screen_at_end"] = session.screen.text()
    clean_data(data)
    return record


def version(editor, scratch):
    """Return the first line EDITOR prints for its version, run in its fenced home."""
    try:
        out = subprocess.run(EDITORS[editor]["version"], capture_output=True, text=True, timeout=30,
                             env=fence(scratch, editor)).stdout
        return out.strip().splitlines()[0]
    except (OSError, subprocess.SubprocessError, IndexError):
        return "unknown"


def machine(args, wrap):
    """Return what the report says about the machine and the settings of this run."""
    model = "unknown"
    with open("/proc/cpuinfo") as f:
        for line in f:
            if line.startswith("model name"):
                model = line.split(":", 1)[1].strip()
                break
    try:
        with open("/sys/devices/system/cpu/cpu0/cpufreq/scaling_max_freq") as f:
            mhz = int(f.read()) // 1000
    except (OSError, ValueError):
        mhz = None
    with open("/proc/meminfo") as f:
        memory = int(f.readline().split()[1]) * 1024
    return {"cpu": model, "max_mhz": mhz, "threads": os.cpu_count(), "memory": memory,
            "kernel": platform.release(), "python": platform.python_version(), "wrap": " ".join(wrap),
            "rows": args.rows, "columns": args.columns, "quiet_ms": args.quiet_ms, "rest_ms": args.rest_ms,
            "limit_s": args.limit, "repetitions": args.repetitions, "typing_keys": args.typing_keys,
            "page_keys": args.page_keys, "date": time.strftime("%Y-%m-%d %H:%M %Z")}


def main():
    """Read the arguments, run every editor on every file the asked number of times, and write the records."""
    parser = argparse.ArgumentParser(description="Time editors on large files through a pseudo-terminal.")
    parser.add_argument("--data", required=True)
    parser.add_argument("--scratch", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument("--wrap", default="")
    parser.add_argument("--editors", default="emacs,emacs-writable,neovim,helix,tuatara")
    parser.add_argument("--files", default="csv,huge,log,org,json")
    parser.add_argument("--repetitions", type=int, default=3)
    parser.add_argument("--typing-keys", type=int, default=100)
    parser.add_argument("--page-keys", type=int, default=40)
    parser.add_argument("--rows", type=int, default=40)
    parser.add_argument("--columns", type=int, default=120)
    parser.add_argument("--quiet-ms", type=float, default=100)
    parser.add_argument("--rest-ms", type=float, default=400)
    parser.add_argument("--limit", type=float, default=120)
    args = parser.parse_args()
    wrap = shlex.split(args.wrap)
    editors = args.editors.split(",")
    with open(os.path.join(args.data, "manifest.json")) as f:
        manifest = json.load(f)
    entries = []
    for kind in args.files.split(","):
        entry = dict(manifest[kind])
        entry["kind"] = kind
        entries.append(entry)
    os.makedirs(args.scratch, exist_ok=True)
    with open(args.out, "w") as out:
        meta = {"machine": machine(args, wrap), "files": entries,
                "editors": {e: {"name": EDITORS[e]["name"], "version": version(e, args.scratch),
                                "large_file_setup": EDITORS[e]["large_file_setup"]} for e in editors}}
        out.write(json.dumps({"meta": meta}) + "\n")
        for repetition in range(args.repetitions):
            order = editors[repetition % len(editors):] + editors[:repetition % len(editors)]
            for entry in entries:
                for editor in order:
                    print(f"repetition {repetition + 1}: {editor} on {entry['path']}", file=sys.stderr, flush=True)
                    record = run_one(editor, entry, args.data, args, wrap)
                    record["repetition"] = repetition + 1
                    out.write(json.dumps(record) + "\n")
                    out.flush()
                    print(f"  {record['status']}, open {record['open_ms'] or 0:.0f} ms", file=sys.stderr, flush=True)
    for editor in editors:
        shutil.rmtree(os.path.join(args.scratch, "fence-" + editor), ignore_errors=True)


if __name__ == "__main__":
    main()
