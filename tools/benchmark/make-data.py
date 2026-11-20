#!/usr/bin/env python3
# tools/benchmark/make-data.py --- Write the made-up large files the editor benchmark opens.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Write the benchmark's files: two CSV tables, a log, an Org note and a JSON file on one line.

Usage: python3 -I tools/benchmark/make-data.py DIRECTORY [--csv-mb N] [--huge-mb N]
       [--log-mb N] [--org-mb N] [--json-mb N] [--seed N]

The defaults are the full benchmark's sizes (a CSV of 2 GiB, a gigantic CSV of
11 GiB, not far below the 16 GiB of memory each editor is given, a log of 1
GiB, an Org note of 10 MiB, JSON of 20 MiB); tools/benchmark/run.sh --trial
asks for small ones. Everything is made up from a fixed seed, so the same
sizes and seed give the same bytes on any machine: manifest.json in DIRECTORY
lists each file with its size and SHA-256 sum to compare. No real names,
places or records are used.

Each file has one line near its end holding the search target, a word found
nowhere else in it (the needle), with a word on each side of it that the
harness looks for on the screen to know the search arrived (the found word):
on both sides, as an editor may scroll a long line only as far as the needle.
The manifest also names a piece of the first line, which the harness waits
for when it opens the file. A file already there with the size and seed asked for is kept.
"""

import argparse
import datetime
import hashlib
import json
import os
import random
import sys

MIB = 1024 * 1024
NEEDLE = "needle-kq7vz"
FOUND = "FOUNDIT"
FOUND_RUN = " ".join([FOUND] * 4)
FORMAT = 3
SYLLABLES = ["ka", "lo", "mi", "ra", "te", "vo", "su", "ne", "pa", "di", "ho", "wu", "ze", "bi", "fo", "gu"]
STATUSES = ["ok", "late", "missing", "checked", "repaired", "estimated"]
LEVELS = ["INFO", "INFO", "INFO", "INFO", "DEBUG", "DEBUG", "WARN", "ERROR"]


def made_up_words(rng, count):
    """Return COUNT made-up words of two to four syllables, none a real name and none holding the needle."""
    words = set()
    while len(words) < count:
        words.add("".join(rng.choice(SYLLABLES) for _ in range(rng.randint(2, 4))))
    return sorted(words)


class Writer:
    """A file being written in large pieces, its SHA-256 sum kept as it goes."""

    def __init__(self, path):
        """Open PATH for writing."""
        self.file = open(path, "wb")
        self.hash = hashlib.sha256()
        self.size = 0

    def write(self, text):
        """Add TEXT to the file."""
        data = text.encode("utf-8")
        self.file.write(data)
        self.hash.update(data)
        self.size += len(data)

    def close(self):
        """Close the file and return its size and SHA-256 sum."""
        self.file.close()
        return self.size, self.hash.hexdigest()


def write_lines(writer, target, make_block, target_line):
    """Write blocks of lines from MAKE_BLOCK(INDEX) until WRITER holds TARGET bytes.

    TARGET_LINE() is written once, when 99% of TARGET is reached."""
    index = 0
    placed = False
    while writer.size < target:
        if not placed and writer.size >= target * 0.99:
            writer.write(target_line())
            placed = True
        writer.write(make_block(index))
        index += 1
    if not placed:
        writer.write(target_line())


def make_csv(path, size, rng):
    """Write a CSV table of station readings of about SIZE bytes to PATH.

    Return its writer and a piece of its first line."""
    stations = made_up_words(rng, 400)
    remarks = made_up_words(rng, 2000)
    writer = Writer(path)
    writer.write("row_id,recorded_at,station,reading,status,remark\n")
    row = [0]

    def block(_):
        lines = []
        for _ in range(4096):
            row[0] += 1
            n = row[0]
            lines.append(f"{n},2026-{1 + n % 12:02d}-{1 + n % 28:02d}T{n % 24:02d}:{n % 60:02d}:{n * 7 % 60:02d},"
                         f"{rng.choice(stations)},{rng.random() * 1000:.3f},{rng.choice(STATUSES)},"
                         f"{rng.choice(remarks)} {rng.choice(remarks)}\n")
        return "".join(lines)

    def target():
        row[0] += 1
        return f"{row[0]},2026-06-15T12:00:00,{stations[0]},0.000,checked,{FOUND_RUN} {NEEDLE} {FOUND_RUN}\n"

    write_lines(writer, size, block, target)
    return writer, "row_id,recorded_at,station"


TOKEN_CHARACTERS = bytes(b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"[i % 62] for i in range(256))
SENTENCE = "The quick brown fox jumps over the lazy dog."
FIRST_SECOND = 946684800      # 2000-01-01T00:00:00Z
LAST_SECOND = 1767225600      # 2026-01-01T00:00:00Z


def make_gigantic(path, size, rng):
    """Write a CSV table of about SIZE bytes to PATH, five fields a row: a time, a later time
    (at most an hour later), the later time as seconds since 1970, a sentence and a token of 16
    letters and digits. Return its writer and a piece of its first line."""
    writer = Writer(path)
    writer.write("DATE-1,DATE-2,DATE-3,DATA-1,DATA-2\n")
    stamp = datetime.datetime.fromtimestamp
    utc = datetime.timezone.utc

    def block(_):
        tokens = rng.randbytes(16 * 4096).translate(TOKEN_CHARACTERS).decode("ascii")
        lines = []
        for k in range(4096):
            first = rng.randrange(FIRST_SECOND, LAST_SECOND)
            second = first + rng.randrange(1, 3601)
            lines.append(f"{stamp(first, utc):%Y-%m-%dT%H:%M:%SZ},{stamp(second, utc):%Y-%m-%dT%H:%M:%SZ},"
                         f"{second},{SENTENCE},{tokens[16 * k:16 * k + 16]}\n")
        return "".join(lines)

    def target():
        return f"2026-06-15T12:00:00Z,2026-06-15T12:30:00Z,1781526600,{FOUND_RUN} {NEEDLE} {FOUND_RUN},{FOUND}\n"

    write_lines(writer, size, block, target)
    return writer, "DATE-1,DATE-2,DATE-3"


def make_log(path, size, rng):
    """Write a service log of about SIZE bytes to PATH; return its writer and a piece of its first line."""
    services = made_up_words(rng, 40)
    paths = ["/" + "/".join(rng.sample(made_up_words(rng, 60), 3)) for _ in range(300)]
    writer = Writer(path)
    writer.write("2026-01-01T00:00:00.000Z INFO [start] log begins, generated for the editor benchmark\n")
    second = [0]

    def block(_):
        lines = []
        for _ in range(4096):
            second[0] += 1
            s = second[0]
            lines.append(f"2026-01-{1 + s // 86400 % 28:02d}T{s // 3600 % 24:02d}:{s // 60 % 60:02d}:{s % 60:02d}."
                         f"{rng.randrange(1000):03d}Z {rng.choice(LEVELS)} [{rng.choice(services)}] "
                         f"request id={rng.getrandbits(48):012x} path={rng.choice(paths)} "
                         f"took={rng.randrange(1, 900)}ms status={rng.choice((200, 200, 200, 204, 404, 500))}\n")
        return "".join(lines)

    def target():
        return f"2026-01-28T23:59:59.999Z WARN [check] {FOUND_RUN} {NEEDLE} {FOUND_RUN}\n"

    write_lines(writer, size, block, target)
    return writer, "log begins, generated"


def make_org(path, size, rng):
    """Write an Org note of about SIZE bytes to PATH: headings, paragraphs, lists, tables and code.

    Return its writer and a piece of its first line."""
    words = made_up_words(rng, 3000)
    writer = Writer(path)
    writer.write("#+title: Field notes, generated for the editor benchmark\n\n")

    def sentence():
        return " ".join(rng.choice(words) for _ in range(rng.randint(6, 14))).capitalize() + "."

    def paragraph():
        text = " ".join(sentence() for _ in range(rng.randint(2, 5)))
        out, line = [], ""
        for word in text.split():
            if len(line) + len(word) + 1 > 72:
                out.append(line)
                line = word
            else:
                line = f"{line} {word}" if line else word
        out.append(line)
        return "\n".join(out) + "\n\n"

    def block(index):
        parts = [f"* {rng.choice(['TODO ', 'DONE ', ''])}{sentence()[:50]}\n"]
        for sub in range(rng.randint(1, 4)):
            parts.append(f"** {sentence()[:40]} :{rng.choice(words)}:\n")
            parts.append(paragraph())
            kind = (index + sub) % 4
            if kind == 0:
                parts.extend(f"- {sentence()}\n" for _ in range(rng.randint(2, 5)))
                parts.append("\n")
            elif kind == 1:
                parts.append("| name | count | note |\n|------+-------+------|\n")
                parts.extend(f"| {rng.choice(words)} | {rng.randrange(1000)} | {rng.choice(words)} |\n"
                             for _ in range(rng.randint(2, 6)))
                parts.append("\n")
            elif kind == 2:
                parts.append("#+begin_src python\n")
                parts.extend(f"total += {rng.choice(words)}({rng.randrange(100)})\n" for _ in range(rng.randint(2, 5)))
                parts.append("#+end_src\n\n")
        return "".join(parts)

    def target():
        return f"* Last checks\nThe search target is here: {FOUND_RUN} {NEEDLE} {FOUND_RUN}\n\n"

    write_lines(writer, size, block, target)
    return writer, "#+title: Field notes"


def make_json(path, size, rng):
    """Write JSON of about SIZE bytes to PATH, all on one line with no newline in it.

    Return its writer and a piece of its first line."""
    words = made_up_words(rng, 1000)
    writer = Writer(path)
    writer.write('{"source":"generated for the editor benchmark","records":[')
    count = [0]

    def block(_):
        items = []
        for _ in range(512):
            count[0] += 1
            items.append(json.dumps({"id": count[0], "name": rng.choice(words), "tags": rng.sample(words, 3),
                                     "value": round(rng.random() * 100, 4), "active": rng.random() < 0.5},
                                    separators=(",", ":")) + ",")
        return "".join(items)

    def target():
        return json.dumps({"id": 0, "name": f"{FOUND_RUN} {NEEDLE} {FOUND_RUN}"}, separators=(",", ":")) + ","

    write_lines(writer, size, block, target)
    writer.write('{"id":-1,"name":"end"}]}')
    return writer, '"source":"generated'


KINDS = {"csv": ("table.csv", make_csv), "huge": ("gigantic.csv", make_gigantic), "log": ("service.log", make_log),
         "org": ("notes.org", make_org), "json": ("records.json", make_json)}


def main():
    """Read the arguments and write the files that are missing or of another size or seed."""
    parser = argparse.ArgumentParser(description="Write the editor benchmark's made-up files.")
    parser.add_argument("directory")
    parser.add_argument("--csv-mb", type=float, default=2048)
    parser.add_argument("--huge-mb", type=float, default=11264)
    parser.add_argument("--log-mb", type=float, default=1024)
    parser.add_argument("--org-mb", type=float, default=10)
    parser.add_argument("--json-mb", type=float, default=20)
    parser.add_argument("--seed", type=int, default=20261101)
    args = parser.parse_args()
    os.makedirs(args.directory, exist_ok=True)
    manifest_path = os.path.join(args.directory, "manifest.json")
    try:
        with open(manifest_path) as f:
            old = json.load(f)
    except (OSError, ValueError):
        old = {}
    manifest = {}
    for kind, (name, make) in KINDS.items():
        size = int(getattr(args, f"{kind}_mb") * MIB)
        path = os.path.join(args.directory, name)
        entry = old.get(kind, {})
        if (entry.get("asked") == size and entry.get("seed") == args.seed and entry.get("format") == FORMAT
                and os.path.exists(path) and os.path.getsize(path) == entry.get("size")):
            manifest[kind] = entry
            continue
        print(f"Writing {path} ({size / MIB:g} MiB)...", file=sys.stderr, flush=True)
        writer, first = make(path, size, random.Random(f"{args.seed}-{kind}"))
        written, digest = writer.close()
        manifest[kind] = {"path": name, "asked": size, "seed": args.seed, "format": FORMAT, "size": written,
                          "sha256": digest, "open_marker": first, "needle": NEEDLE, "found": FOUND}
    with open(manifest_path, "w") as f:
        json.dump(manifest, f, indent=1)
        f.write("\n")


if __name__ == "__main__":
    main()
