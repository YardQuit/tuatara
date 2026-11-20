#!/usr/bin/env python3
# tools/benchmark/keys.py --- How each editor in the benchmark is started and which keys do each edit.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""The editors the benchmark compares, and the same edits written in each one's keys.

It is not run on its own: tools/benchmark/harness.py loads it. To add an
editor, add an entry to EDITORS with the same fields. Every editor is run in
its terminal mode with its stock settings in an empty home of its own; the
only settings written are Emacs's, in tools/benchmark/emacs-init.el, which
turn on its own large-file help (so-long) and stop it asking whether to
open a large file at all. Emacs leaves out the files a distribution adds to
it (--no-site-file), so that their warnings do not take half its screen.

The edits, in the order the harness does them:
  start_typing  keys that put the editor in a state where letters are typed
                into the file (not timed);
  typing        letters and spaces typed one key at a time at the start of
                the file, a newline every 50 keys (each key timed);
  stop_typing   keys back to the editor's usual state (not timed);
  page_down     one screen down (each key timed);
  to_end        to the end of the file (timed);
  to_start      back to the start (not timed);
  search        a search for a word found once near the end of the file,
                typed one key every 150 ms as a person types (not timed);
                the last key (RET) is timed until the match is on the screen
                and the editor has stopped working.

An entry with only_if is run only on the files whose first screen matches
that regular expression; on the others its run is skipped and left out of
the report.
"""

import os

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
ESC = "\x1b"
TUATARA = os.environ.get("BENCH_TUATARA", os.path.join(ROOT, "build", "tuatara"))


def ctrl(letter):
    """Return the character a terminal sends for Control and LETTER."""
    return chr(ord(letter.lower()) - ord("a") + 1)


EDITORS = {
    "emacs": {
        "name": "GNU Emacs",
        "command": ["emacs", "-nw", "--no-site-file"],
        "version": ["emacs", "--version"],
        "setup": {".emacs.d/init.el": os.path.join(HERE, "emacs-init.el")},
        "large_file_setup": "so-long (global-so-long-mode), no question before a large file, no site files; "
                            "vlf when BENCH_EMACS_VLF names a directory holding it",
        "start_typing": "",
        "newline": "\r",
        "stop_typing": "",
        "page_down": ctrl("v"),
        "to_end": ESC + ">",
        "to_start": ESC + "<",
        "search": lambda word: ctrl("s") + word + "\r",
    },
    # the same Emacs, run only on the files so-long makes read-only, which it
    # makes writable (C-x C-q) before typing
    "emacs-writable": {
        "name": "GNU Emacs, made writable",
        "command": ["emacs", "-nw", "--no-site-file"],
        "version": ["emacs", "--version"],
        "setup": {".emacs.d/init.el": os.path.join(HERE, "emacs-init.el")},
        "large_file_setup": "as GNU Emacs, run only where so-long made the buffer read-only; C-x C-q before typing",
        "only_if": r"^-\S*:%%",
        "start_typing": ctrl("x") + ctrl("q"),
        "newline": "\r",
        "stop_typing": "",
        "page_down": ctrl("v"),
        "to_end": ESC + ">",
        "to_start": ESC + "<",
        "search": lambda word: ctrl("s") + word + "\r",
    },
    "neovim": {
        "name": "Neovim",
        "command": ["nvim"],
        "version": ["nvim", "--version"],
        "setup": {},
        "large_file_setup": "stock",
        "start_typing": "i",
        "newline": "\r",
        "stop_typing": ESC,
        "page_down": ctrl("f"),
        "to_end": "G",
        "to_start": "gg",
        "search": lambda word: "/" + word + "\r",
    },
    "helix": {
        "name": "Helix",
        "command": ["hx"],
        "version": ["hx", "--version"],
        "setup": {},
        "large_file_setup": "stock",
        "start_typing": "i",
        "newline": "\r",
        "stop_typing": ESC,
        "page_down": ctrl("f"),
        "to_end": "ge",
        "to_start": "gg",
        "search": lambda word: "/" + word + "\r",
    },
    "tuatara": {
        "name": "Tuatara",
        "command": [TUATARA],
        "version": [TUATARA, "--version"],
        "setup": {},
        "large_file_setup": "stock",
        "start_typing": "i",
        "newline": "\r",
        "stop_typing": ESC,
        "page_down": ctrl("v"),
        "to_end": ESC + ">",
        "to_start": ESC + "<",
        "search": lambda word: ctrl("s") + word + "\r",
    },
}
