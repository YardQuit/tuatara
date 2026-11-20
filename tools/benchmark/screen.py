#!/usr/bin/env python3
# tools/benchmark/screen.py --- A small terminal screen that reads an editor's output, for the benchmark.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""A small terminal screen for the benchmark's pseudo-terminal harness.

It is not run on its own: tools/benchmark/harness.py loads it. It keeps the
characters an editor has drawn in a grid of rows and columns, as an xterm would
show them, so that the harness can tell when the text it waits for is on the
screen. Colours and other attributes are read and dropped. It answers the
questions editors ask a terminal when they start (its kind, its version, where
the cursor is), the same answers for every editor, so that none of them waits
for a reply that never comes.
"""

import codecs
import unicodedata

DEVICE_ATTRIBUTES = b"\x1b[?62;22c"
SECONDARY_ATTRIBUTES = b"\x1b[>0;95;0c"


class Screen:
    """A grid of ROWS by COLUMNS characters that FEED fills from an editor's output.

    TEXT gives what is on the screen as one string, rows joined by newlines.
    Each call to FEED returns the bytes to send back to the editor: the answers
    to the questions it asked in that output, empty when it asked none."""

    def __init__(self, rows, columns):
        """Make an empty screen of ROWS by COLUMNS, the cursor at the top left."""
        self.rows = rows
        self.columns = columns
        self._decoder = codecs.getincrementaldecoder("utf-8")(errors="replace")
        self._state = "ground"
        self._sequence = ""
        self._main = None
        self.reset()

    def reset(self):
        """Clear the screen and put the cursor, margins and modes back as they are at the start."""
        self.grid = [[" "] * self.columns for _ in range(self.rows)]
        self.row = 0
        self.column = 0
        self.top = 0
        self.bottom = self.rows - 1
        self.wrap_pending = False
        self.saved = (0, 0)
        self.last_char = " "

    def text(self):
        """Return the screen's characters, each row with its trailing spaces removed, rows joined by newlines."""
        return "\n".join("".join(cells).rstrip() for cells in self.grid)

    def feed(self, data):
        """Read DATA, bytes the editor wrote, into the screen; return the bytes of any answers it asked for."""
        replies = []
        for char in self._decoder.decode(data):
            state = self._state
            if state == "ground":
                if char >= " " and char != "\x7f":
                    self._put(char)
                elif char == "\x1b":
                    self._state = "escape"
                    self._sequence = ""
                else:
                    self._control(char)
            elif state == "escape":
                self._escape(char)
            elif state == "csi":
                if "@" <= char <= "~":
                    self._state = "ground"
                    reply = self._csi(self._sequence, char)
                    if reply:
                        replies.append(reply)
                elif char == "\x1b":
                    self._state = "escape"
                    self._sequence = ""
                else:
                    self._sequence += char
            elif state == "string":
                # an operating system command, device control or other string, ended by BEL or ESC \
                if char == "\x07":
                    self._state = "ground"
                elif char == "\x1b":
                    self._state = "string-escape"
            elif state == "string-escape":
                self._state = "ground" if char == "\\" else "string"
            elif state == "charset":
                self._state = "ground"
        return b"".join(replies)

    def _escape(self, char):
        """Act on the character after an ESC."""
        self._state = "ground"
        if char == "[":
            self._state = "csi"
        elif char in "]P_^X":
            self._state = "string"
        elif char in "()*+-./#%":
            self._state = "charset"
        elif char == "7":
            self.saved = (self.row, self.column)
        elif char == "8":
            self.row, self.column = self.saved
            self.wrap_pending = False
        elif char == "D":
            self._index()
        elif char == "E":
            self.column = 0
            self._index()
        elif char == "M":
            self._reverse_index()
        elif char == "c":
            self.reset()

    def _control(self, char):
        """Act on a control character."""
        if char == "\r":
            self.column = 0
            self.wrap_pending = False
        elif char in "\n\x0b\x0c":
            self._index()
        elif char == "\b":
            if self.column > 0:
                self.column -= 1
            self.wrap_pending = False
        elif char == "\t":
            self.column = min(self.columns - 1, (self.column // 8 + 1) * 8)

    def _put(self, char):
        """Draw CHAR at the cursor and move on, wrapping at the right edge as xterm does."""
        width = 2 if unicodedata.east_asian_width(char) in "WF" else 1
        if unicodedata.combining(char):
            return
        if self.wrap_pending or self.column + width > self.columns:
            self.column = 0
            self._index()
            self.wrap_pending = False
        self.grid[self.row][self.column] = char
        if width == 2 and self.column + 1 < self.columns:
            self.grid[self.row][self.column + 1] = ""
        self.last_char = char
        if self.column + width >= self.columns:
            self.column = self.columns - 1
            self.wrap_pending = True
        else:
            self.column += width

    def _index(self):
        """Move the cursor down one row, scrolling the margins' rows up at the bottom margin."""
        self.wrap_pending = False
        if self.row == self.bottom:
            self._scroll_up(1)
        elif self.row < self.rows - 1:
            self.row += 1

    def _reverse_index(self):
        """Move the cursor up one row, scrolling the margins' rows down at the top margin."""
        self.wrap_pending = False
        if self.row == self.top:
            self._scroll_down(1)
        elif self.row > 0:
            self.row -= 1

    def _blank(self):
        """Return an empty row."""
        return [" "] * self.columns

    def _scroll_up(self, count, top=None):
        """Scroll the rows from TOP (the top margin when not given) to the bottom margin up by COUNT."""
        top = self.top if top is None else top
        for _ in range(min(count, self.bottom - top + 1)):
            del self.grid[top]
            self.grid.insert(self.bottom, self._blank())

    def _scroll_down(self, count, top=None):
        """Scroll the rows from TOP (the top margin when not given) to the bottom margin down by COUNT."""
        top = self.top if top is None else top
        for _ in range(min(count, self.bottom - top + 1)):
            del self.grid[self.bottom]
            self.grid.insert(top, self._blank())

    def _csi(self, sequence, final):
        """Act on a control sequence: its parameters and intermediates SEQUENCE, then FINAL; return any answer."""
        private = sequence[:1] if sequence[:1] in "?>=<" else ""
        body = sequence[len(private):]
        if any(c in body for c in " !\"#$%&'*+,-./"):
            return None
        numbers = []
        for part in body.split(";"):
            head = part.split(":")[0]
            numbers.append(int(head) if head.isdigit() else 0)

        def number(index=0, default=1):
            value = numbers[index] if index < len(numbers) else 0
            return value if value else default

        if final == "c":
            return SECONDARY_ATTRIBUTES if private == ">" else (DEVICE_ATTRIBUTES if not private else None)
        if private == "?":
            if final in "hl" and set(numbers) & {47, 1047, 1049}:
                self.grid = [self._blank() for _ in range(self.rows)]
                if 1049 in numbers and final == "h":
                    self._main = (self.row, self.column)
                elif 1049 in numbers and self._main:
                    self.row, self.column = self._main
            return None
        if private:
            return None
        self.wrap_pending = False
        if final in "Hf":
            self.row = min(self.rows - 1, number(0) - 1)
            self.column = min(self.columns - 1, number(1) - 1)
        elif final == "A":
            self.row = max(0, self.row - number())
        elif final in "Be":
            self.row = min(self.rows - 1, self.row + number())
        elif final in "Ca":
            self.column = min(self.columns - 1, self.column + number())
        elif final == "D":
            self.column = max(0, self.column - number())
        elif final == "E":
            self.row = min(self.rows - 1, self.row + number())
            self.column = 0
        elif final == "F":
            self.row = max(0, self.row - number())
            self.column = 0
        elif final in "G`":
            self.column = min(self.columns - 1, number() - 1)
        elif final == "d":
            self.row = min(self.rows - 1, number() - 1)
        elif final == "I":
            for _ in range(number()):
                self.column = min(self.columns - 1, (self.column // 8 + 1) * 8)
        elif final == "Z":
            for _ in range(number()):
                self.column = max(0, (self.column - 1) // 8 * 8)
        elif final == "J":
            self._erase_display(number(0, 0))
        elif final == "K":
            self._erase_line(number(0, 0))
        elif final == "X":
            line = self.grid[self.row]
            for c in range(self.column, min(self.columns, self.column + number())):
                line[c] = " "
        elif final == "@":
            line = self.grid[self.row]
            count = min(number(), self.columns - self.column)
            line[self.column:self.column] = [" "] * count
            del line[self.columns:]
        elif final == "P":
            line = self.grid[self.row]
            count = min(number(), self.columns - self.column)
            del line[self.column:self.column + count]
            line.extend([" "] * count)
        elif final == "L":
            if self.top <= self.row <= self.bottom:
                self._scroll_down(number(), self.row)
        elif final == "M":
            if self.top <= self.row <= self.bottom:
                self._scroll_up(number(), self.row)
        elif final == "S":
            self._scroll_up(number())
        elif final == "T":
            self._scroll_down(number())
        elif final == "b":
            for _ in range(number()):
                self._put(self.last_char)
        elif final == "r":
            top = number(0) - 1
            bottom = number(1, self.rows) - 1
            if 0 <= top < bottom < self.rows:
                self.top, self.bottom = top, bottom
                self.row, self.column = 0, 0
        elif final == "s":
            self.saved = (self.row, self.column)
        elif final == "u":
            self.row, self.column = self.saved
        elif final == "n":
            if number(0, 0) == 5:
                return b"\x1b[0n"
            if number(0, 0) == 6:
                return f"\x1b[{self.row + 1};{self.column + 1}R".encode()
        return None

    def _erase_display(self, mode):
        """Erase below the cursor (MODE 0), above it (1) or the whole screen (2 and 3)."""
        if mode == 0:
            self._erase_line(0)
            for r in range(self.row + 1, self.rows):
                self.grid[r] = self._blank()
        elif mode == 1:
            self._erase_line(1)
            for r in range(0, self.row):
                self.grid[r] = self._blank()
        else:
            self.grid = [self._blank() for _ in range(self.rows)]

    def _erase_line(self, mode):
        """Erase the cursor's row to its right (MODE 0), to its left (1) or whole (2)."""
        line = self.grid[self.row]
        if mode == 0:
            start, end = self.column, self.columns
        elif mode == 1:
            start, end = 0, self.column + 1
        else:
            start, end = 0, self.columns
        for c in range(start, end):
            line[c] = " "
