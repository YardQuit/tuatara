;;;; lisp/package.lisp --- The defaults package.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(defpackage #:tuatara.defaults
  ;; tuatara-user: every setting and function an init file may name, by the same names here
  (:use #:cl #:tuatara.command #:tuatara.modes #:tuatara.dates #:tuatara-user)
  ;; commands of these names here, kept apart from the functions of the init file's
  (:shadow #:count-words #:find-file #:point #:forward-char #:backward-char #:load-theme
           #:switch-to-buffer #:insert)
  (:shadowing-import-from #:tuatara.display #:define-theme)
  (:import-from #:tuatara.display #:define-face #:set-face-attributes)
  (:import-from #:tuatara.io #:octets-integer #:octets-signed #:octets-words #:directory-name)
  (:import-from #:tuatara.io #:fd-octets #:crc32 #:zip-octets #:zip-entries #:zip-entry-octets #:inflate #:zip-error
                #:*zip-largest-entry* #:*zip-largest-ratio*)
  (:import-from #:tuatara.buffer #:buffer-crlf-p)
  (:import-from #:tuatara.runtime #:foreign-library-p #:foreign-call #:jobs-of)
  (:import-from #:tuatara.display #:next-tab-stop)
  (:import-from #:tuatara.text #:word-char-p #:name-char-p #:number-formatter)
  (:import-from #:tuatara.text #:string-line-start #:string-line-end #:xml-entity #:xml-tag-end #:xml-text-value #:utf-16-text #:parse-decimal #:+fnv-offset+
                #:xml-name #:xml-attribute #:xml-children #:xml-child #:xml-root #:xml-local-name #:xml-local-children #:xml-find-all #:xml-all-text)
  (:import-from #:tuatara.text #:xml-first-text #:xml-parse #:*xml-deepest* #:*xml-most-elements* #:xml-escape
                #:xml-elements #:xml-element #:xml-top-elements #:xml-element-inner #:xml-element-inside #:xml-replace-elements #:xml-value)
  (:documentation "The defaults compiled into the image: the base editing commands, the
standard key bindings, the modal editing layer (TUATARA NORMAL and INSERT), and the setup function that installs
them into an editor.")
  (:export #:install-defaults #:*config-file* #:load-user-config
           #:modal-state #:tuatara-normal #:tuatara-insert
           #:*tuatara-normal-keymap* #:*tuatara-insert-keymap* #:*tuatara-support-keymap* #:*tuatara-list-keymap* #:*tuatara-leader-keymap*
           #:*global-keymap* #:*tuatara-normal-bindings* #:buffer-modal-class))

(in-package #:tuatara.defaults)

(defvar *global-keymap* nil
  "The global keymap of the editor the defaults were last installed into.
Defined here, before any file of the defaults refers to it.")

;; Settings and keymaps some files use before the file that defines them is
;; loaded: special from the start, so that a binding of one is never lexical.
(declaim (special *virtual-cursor-keymap* *tuatara-list-keymap* *tuatara-calendar-keymap* *split-chooser-keymap*
                  *rimu-quick-edit-normal-keymap* *rimu-quick-edit-keymap* *rewrite-threshold* *last-search-pattern*
                  *init-file-outcome* *image-formats* *enter-dwim-functions* *cursor-run-keymap* *cursor-keymap*
                  tuatara-user:*rimu-preview* tuatara-user:*gui-font*))
