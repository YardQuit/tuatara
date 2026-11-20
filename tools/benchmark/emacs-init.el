;;; emacs-init.el --- Emacs's settings in the editor benchmark  -*- lexical-binding: t -*-
;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

;;; Commentary:

;; Not loaded by hand: tools/benchmark/harness.py copies it to init.el in
;; the empty home it gives Emacs. It turns on what Emacs itself offers for
;; large files and nothing else.
;;
;; Emacs is started with --no-site-file and this file sets
;; inhibit-default-init, so the files a distribution adds (site-start.el,
;; default.el) are not loaded: on Fedora they bring warnings that split the
;; screen in two.
;;
;; - so-long, built in since Emacs 27, for files with very long lines. It
;;   makes such a buffer read-only, so Emacs types nothing into the long-line
;;   JSON file; the report says so.
;; - No question before opening a large file: the benchmark opens them all.
;; - vlf (View Large Files) only when BENCH_EMACS_VLF names a directory
;;   holding vlf.el: it is an ELPA package, not part of Emacs, and the
;;   benchmark downloads nothing. With it, files over the large-file limit
;;   open in pieces without asking.

;;; Code:

(setq inhibit-startup-screen t
      inhibit-default-init t)
(global-so-long-mode 1)
(let ((vlf (getenv "BENCH_EMACS_VLF")))
  (if (and vlf (file-exists-p (expand-file-name "vlf.el" vlf)))
      (progn
        (add-to-list 'load-path vlf)
        (require 'vlf-setup)
        (setq vlf-application 'dont-ask))
    (setq large-file-warning-threshold nil)))

;;; emacs-init.el ends here
