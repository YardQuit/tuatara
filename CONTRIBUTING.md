# Contributing to Tuatara

Changes come into Tuatara only through pull requests, and pull requests
only from approved contributors. Anyone may open an issue: to report a
fault, to suggest an idea, or to talk about a change before making it.

## Becoming an approved contributor

Open an issue that says what you would like to work on. A maintainer who
agrees adds you to the organisation's contributors; a pull request from
anyone else is closed by the checks, with a note pointing here.

## Making a change

1. Fork `Tuatara-Editor/tuatara` and clone your fork. Add the project as
   `upstream`:

   ```
   git remote add upstream https://github.com/Tuatara-Editor/tuatara.git
   ```

2. Start each change from the newest `main`, on a branch of its own:

   ```
   git fetch upstream
   git switch -c short-name-of-the-change upstream/main
   ```

3. Build, change and test as [HACKING.org](HACKING.org) says. Run the test
   groups the change touches; a change across the code runs `tools/test.sh`.
4. Commit with a sign-off, and sign the commit if you can:

   ```
   git commit -s -S
   ```

5. Push the branch to your fork and open a pull request against
   `Tuatara-Editor/tuatara:main`. Keep it to one change: two unrelated
   fixes are two pull requests.
6. A maintainer reviews it. Push further commits to the same branch to
   answer the review; the pull request is merged as one commit, its title
   becoming the commit's first line.
7. After the merge, delete the branch and bring your fork's `main` up to
   date (`git switch main && git pull --ff-only upstream main && git push`).

## What a change needs

- **Its tests.** A fix, or two ways of doing one thing merged into one, comes
  with a test that fails on the code before the change and passes after it.
  A fix for slowness measures what it fixes.
- **Its documentation.** Every definition has a docstring that a user reads in
  the help; README, HACKING or the starter init file is updated where the
  change makes them wrong.
- **The project's conventions**, in [HACKING.org](HACKING.org): one way for each
  kind of thing (look for what does the job before writing another),
  nothing slow on the editor's thread, no colours outside the theme, nothing in
  a file ever run.
- **English.** All text, from docstrings to commit messages, is in English;
  British spelling is preferred (colour, behaviour, organise). Names already
  in the code keep their spelling.
- **Nothing private.** No files, names, logos or text that belong to an
  employer, a customer or a case. Test files are small, made up for the test,
  and say nothing about real people.
- **No paths outside the project.** Code, tests, documentation and examples
  refer to no file or folder of anyone's own: no home folder layout, note or
  file name, tag, template, account or machine. They name only the project's
  own files, a test's own temporary folder, what every system has (`/tmp`,
  `/etc`), or plain made-up places (`~/notes/`, `~/Documents/report.org`).

## Keeping what others build on

From 1.0.0 on, packages and init files are written against Tuatara's stable
surface: the names the `tuatara` package exports (the package API in
`lisp/api.lisp`), with the arguments its functions and macros take; the
settings `tuatara-user` exports; and the names of the commands. Within a
major version nothing of it is removed, renamed or changed so that what used
it stops working: no argument added that must be given, none taken away, no
keyword dropped, no meaning changed. Adding is always fine: a new name, a
new optional or keyword argument, a new command.

A name that has to go is first marked obsolete in a minor release: it keeps
working, its docstring says since when and what to use instead, and the
release notes list it. It goes in the next major release.

`test/api/stable-surface.sexp` pins the surface as the last release had it,
and the `stable-surface` tests fail on a change that breaks it within a major
version. Before 1.0.0 the surface may still change; a pull request that
changes it says so.

## Dependencies

Everything Tuatara takes from others is a way in for code that should not be
there, so each dependency is reviewed before it is added or its pin moved,
and the whole list again at every security review: whether it is
maintained, how widely it is used, who keeps it and how security faults were
handled, and whether its owner or name has changed. It is taken pinned, to a
commit or a version, and what is downloaded is checked against a recorded
hash. Native code that runs inside the editor (tree-sitter grammars, the
terminal library) is read most closely, since the editor opens files that
are not trusted. A pull request that adds a dependency or moves a pin says
why, and what the review found.

A Lisp library is added with `tools/lisp-archives.sh add LIBRARY`, which
downloads it over HTTPS and adds its line, with the archive's SHA-256, to
`tools/lisp-archives.sha256`. That line is the hash the review accepts: the
pull request shows it, and says where the library comes from and what was read
of it. Quicklisp downloads nothing else (`tools/lisp-archives.sh lock`), so a
library not on the list stops the build.

## Sign-off

The `Signed-off-by:` line `git commit -s` adds is your statement, under the
[Developer Certificate of Origin](https://developercertificate.org/), that
you wrote the change or otherwise have the right to give it under the
project's licence, the GNU General Public License, version 3 or later. The
checks refuse a pull request with a commit not signed off by its author;
`git rebase --signoff upstream/main` adds the line to every commit of a
branch.

## Security faults

Report them privately, never in an issue: see
[SECURITY.md](https://github.com/Tuatara-Editor/.github/blob/main/SECURITY.md).
