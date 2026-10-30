;;;; test/themes.lisp --- Faces for the interface, themes, your own face settings.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test themes
  :description "Themes apply over the faces' definitions, the :default face reaches everything,
your own settings survive a theme change, #rrggbb colours map to a terminal palette, and every
part of the screen is painted with a named face.")

(defmacro with-theme-restored (&body body)
  "Run BODY from the faces as defined (no theme, no user settings), and return there afterwards;
every editor session applies the startup theme, so tests about faces start clean."
  `(unwind-protect (progn (setf tuatara.display:*user-face-settings* '())
                          (tuatara.display:apply-theme nil)
                          ,@body)
     (setf tuatara.display:*user-face-settings* '())
     (tuatara.display:apply-theme nil)))

(tuatara.display:define-theme test-theme
  "A theme for the tests."
  (:default :bg "#101010")
  (:comment :fg "#abcdef" :italic nil)
  (:region :reverse nil :bg "#333333"))

(define-test (themes apply-and-reset)
  (with-theme-restored
    (is equal '(:bright-black nil (:italic)) (multiple-value-list (tuatara.display:resolve-face-spec :comment)))
    (tuatara.display:apply-theme 'test-theme)
    ;; the theme's colours, and :default's background under every face
    (is equal '("#abcdef" "#101010" ()) (multiple-value-list (tuatara.display:resolve-face-spec :comment)))
    (is equal '(nil "#333333" ()) (multiple-value-list (tuatara.display:resolve-face-spec :region)))
    (is eq 'test-theme tuatara.display:*current-theme*)
    ;; back to the definitions
    (tuatara.display:apply-theme 'terminal)
    (is equal '(:bright-black nil (:italic)) (multiple-value-list (tuatara.display:resolve-face-spec :comment)))
    (is equal '(nil nil (:reverse)) (multiple-value-list (tuatara.display:resolve-face-spec :region)))
    ;; themes are found by name from any package; unknown names are errors
    (true (tuatara.display:find-theme "ao-dark"))
    (fail (tuatara.display:apply-theme 'no-such-theme))))

(define-test (themes user-settings-stay-on-top)
  (with-theme-restored
    (tuatara.display:set-user-face :keyword :fg :green)
    (tuatara.display:apply-theme 'ao-dark)
    (is eq :green (tuatara.display:face-attribute :keyword :fg))
    (tuatara.display:apply-theme 'ao-light)
    (is eq :green (tuatara.display:face-attribute :keyword :fg))
    ;; the theme still sets everything else
    (is equal "#ffffff" (tuatara.display:face-attribute :default :bg))))

(define-test (themes plain-calendar)
  ;; no colours: the calendar's days told apart by reverse video, bold, underline and italics
  (with-theme-restored
    (let ((editor (nth-value 1 (calendar-keys ""))))
      (tuatara.display:apply-theme 'plain)
      ;; the day under the cursor stays colourless too
      (tuatara.defaults::calendar-cursor-face editor (tuatara.buffer:find-buffer "*Calendar*")))
    (false (tuatara.display:face-attribute :calendar-selected :bg))
    (true (tuatara.display:face-attribute :calendar-selected :reverse))
    ;; the terminal's own colours, unknown to the editor: no colour made up in their place
    (let ((editor (nth-value 1 (calendar-keys ""))))
      (tuatara.display:apply-theme 'terminal)
      (tuatara.defaults::calendar-cursor-face editor (tuatara.buffer:find-buffer "*Calendar*")))
    (false (tuatara.display:face-attribute :calendar-selected :bg))
    (false (tuatara.display:face-attribute :calendar-selected :fg))
    (true (tuatara.display:face-attribute :calendar-selected :reverse))
    ;; nor dark pages: a PDF is shown as printed when the theme gives no colours
    (false (tuatara.defaults::dark-colours))
    (tuatara.display:apply-theme 'ao-dark)
    (true (tuatara.defaults::dark-colours))
    (tuatara.display:apply-theme 'plain)
    (flet ((flags (face) (nth-value 2 (tuatara.display:resolve-face-spec face))))
      (is equal '(:reverse) (flags :calendar-marked))
      (is equal '(:bold :underline) (flags :calendar-journal))
      (is equal '(:bold :italic) (flags :calendar-today))
      (is equal '(:bold :underline :reverse) (flags '(:calendar-journal :calendar-marked)))
      (dolist (face '(:calendar-marked :calendar-journal :calendar-today :calendar-selected))
        (false (tuatara.display:face-attribute face :fg))
        (false (tuatara.display:face-attribute face :bg))))))

(define-test (themes indexing-sign-follows-mode-line)
  ;; the sign at rest is a sixth of the way from the mode line's ground to its text, whatever the theme
  (with-theme-restored
    (flet ((idle () (tuatara.defaults::follow-mode-line-colours)
             (tuatara.display:face-attribute :mode-line-sign-idle :fg)))
      (tuatara.display:apply-theme 'ao-dark)
      (is string= (tuatara.display:mix-colours "#2c5484" "#f3f4f6" 1/6) (idle))
      (is string= "#4d6f97" (idle))
      ;; a mode line coloured by you: the sign follows it
      (tuatara.display:set-user-face :mode-line :fg "#ffffff" :bg "#000000")
      (is string= (tuatara.display:mix-colours "#000000" "#ffffff" 1/6) (idle))
      ;; a sign coloured by you stays as set
      (tuatara.display:set-user-face :mode-line-sign-idle :fg "#123456")
      (is string= "#123456" (idle))
      (setf tuatara.display:*user-face-settings* '())
      ;; no colours known, or none wanted: dim, as defined
      (tuatara.display:apply-theme 'terminal)
      (false (idle))
      (tuatara.display:apply-theme 'plain)
      (false (idle))
      (true (tuatara.display:face-attribute :mode-line-sign-idle :dim)))))

(define-test (themes own-theme-in-the-init-file)
  ;; define-theme as an init file writes it, read in TUATARA-USER, names and roles unqualified
  (with-theme-restored
    (let ((*package* (find-package :tuatara-user)))
      (eval (read-from-string "(define-theme init-file-ao \"Ao on black.\" :faces-from ao-dark :palette ((ground \"#000000\")))"))
      (eval (read-from-string "(load-theme 'init-file-ao)")))
    (is string= "#000000" (tuatara.display:face-attribute :default :bg))
    (is string= "#dadada" (tuatara.display:face-attribute :default :fg))))

(define-test (themes palette)
  (is = 9 (tuatara.display:nearest-palette-index "#ff0000" 256))
  (is = 1 (tuatara.display:nearest-palette-index "#ff0000" 8))
  (is = 244 (tuatara.display:nearest-palette-index "#808080" 256))
  (is = 0 (tuatara.display:nearest-palette-index "#000000" 256))
  (is = 52 (tuatara.display:nearest-palette-index "#5f0000" 256))
  (false (tuatara.display:parse-hex-colour "red"))
  (fail (tuatara.display:nearest-palette-index "#zz0000" 256)))

(define-test (themes screen-faces)
  ;; the mode line is painted with :mode-line across the row
  (multiple-value-bind (f editor) (run-keys "" :text "abc")
    (let ((mode-line-row (- (tuatara.command:editor-height editor) 2)))
      (is eq :mode-line (aref (tuatara.frontend.fake:fake-row-faces f) mode-line-row))))
  ;; the minibuffer: prompt, selected candidate, annotations
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 80 :height 16)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (setf tuatara.defaults::*command-history* '())
    (tuatara.frontend.fake:fake-queue-keys f "M-x e n d SPC o f SPC b u f")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*"))
    ;; the last frame is painted after the minibuffer closes: look at the one showing it
    (multiple-value-bind (screen spans row-faces) (tuatara.frontend.fake:fake-frame-with f "M-x end of buf")
      (let ((selected (position-if (lambda (row) (search "end-of-buffer" row)) screen))
            (prompt (position-if (lambda (row) (search "M-x end of buf" row)) screen)))
        (true selected)
        (is eq :completion-selected (aref row-faces selected))
        ;; what matches the words typed, the key bindings and the documentation have their faces
        (let ((faces (mapcar #'third (aref spans selected))))
          (is eq :completion-match-0 (first faces))
          (true (member :completion-key faces))
          (true (member :completion-annotation faces)))
        ;; after the candidate count, 1/1 padded to seven columns
        (is equal '((7 11 :minibuffer-prompt)) (aref spans prompt)))))
  ;; key hints: keys and prefixes have their faces
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width 100 :height 16)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (tuatara.frontend.fake:fake-queue-keys f "C-x")
    (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:find-buffer "*scratch*"))
    (multiple-value-bind (screen spans) (tuatara.frontend.fake:fake-frame-with f "C-f find-file")
      (declare (ignore screen))
      (let ((faces (loop for row-spans across spans append (mapcar #'third row-spans))))
        (true (member :key-hint-key faces))
        (true (member :key-hint-command faces))))))

(define-test (themes load-theme-command)
  (with-theme-restored
    (multiple-value-bind (f editor) (run-keys "M-x l o a d - t h e m e RET a o - l i g h t RET" :text "abc")
      (declare (ignore editor))
      (is eq 'tuatara.defaults::ao-light tuatara.display:*current-theme*)
      (true (tuatara.frontend.fake:fake-frame-showing f "Theme ao-light")))))

(define-test (themes startup-toggle-and-colourless)
  (with-theme-restored
    ;; ao-dark is in force when the editor starts
    (run-keys "" :text "abc")
    (is eq 'tuatara.defaults::ao-dark tuatara.display:*current-theme*)
    (is equal "#080d15" (tuatara.display:face-attribute :default :bg))
    ;; the toggle switches between the ao pair
    (run-keys "M-x t o g g l e - l i g h t - d a r k - t h e m e RET" :text "abc")
    (is eq 'tuatara.defaults::ao-light tuatara.display:*current-theme*)
    ;; ... and uses the pair you set
    (let ((tuatara-user:*light-theme* 'plain))
      ;; each session starts in ao-dark: one toggle gives your light theme, two come back
      (run-keys "M-x t o g g l e - l i g h t - d a r k - t h e m e RET" :text "abc")
      (is eq 'tuatara.defaults::plain tuatara.display:*current-theme*)
      (run-keys "M-x t o g g l e - l i g h t - d a r k - t h e m e RET M-x t o g g l e - l i g h t - d a r k - t h e m e RET" :text "abc")
      (is eq 'tuatara.defaults::ao-dark tuatara.display:*current-theme*))
    ;; a display without colours gets the plain theme: no colours, attributes only
    (let ((f (tuatara.frontend.fake:make-fake-frontend :colours 0)))
      (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer (tuatara.buffer:make-buffer "*mono*"))
      (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "*mono*"))
      (is eq 'tuatara.defaults::plain tuatara.display:*current-theme*)
      (is equal '(nil nil (:bold)) (multiple-value-list (tuatara.display:resolve-face-spec :keyword)))
      (is equal '(nil nil (:italic :dim)) (multiple-value-list (tuatara.display:resolve-face-spec :comment)))
      (is equal '(nil nil (:reverse)) (multiple-value-list (tuatara.display:resolve-face-spec :mode-line))))))

(define-test (themes list-and-help-headings)
  (dolist (b (copy-list tuatara.buffer:*buffers*))
    (unless (string= (tuatara.buffer:buffer-name b) "*scratch*") (tuatara.buffer:kill-buffer b)))
  (multiple-value-bind (f editor) (fresh-list-session "C-x C-b")
    (declare (ignore editor))
    (is eq :header-line (third (first (aref (tuatara.frontend.fake:fake-spans f) 0)))))
  (multiple-value-bind (f editor) (run-keys "C-h f u n d o RET" :text "")
    (declare (ignore editor))
    (is eq :heading (third (first (aref (tuatara.frontend.fake:fake-spans f) 0))))))

(define-test (themes ao-as-the-users-theme)
  ;; the faces as the Ao theme has them;
  ;; a face with no background of its own shows the theme's
  (with-theme-restored
    (flet ((face (name) (multiple-value-list (tuatara.display:resolve-face-spec name))))
      (tuatara.display:apply-theme 'ao-dark)
      (is equal '("#dadada" "#080d15" ()) (face :default))
      (is equal '("#838a97" "#080d15" ()) (face :search-outside-scope))      ; dimmed text
      (is equal '("#dadada" "#2a1740" ()) (face :match))
      (is equal '("#fa7970" "#080d15" (:bold)) (face :raw-byte))             ; characters shown escaped
      (is equal '("#254862" "#080d15" ()) (face :hex-unprintable))           ; whitespace marks
      (is equal '("#81be83" "#080d15" ()) (face :tag))                       ; markup tags
      (is equal '("#99c7ff" "#080d15" (:bold)) (face :heading))
      (tuatara.display:apply-theme 'ao-light)
      (is equal '("#222222" "#ffffff" ()) (face :default))
      (is equal '("#5d5d5d" "#ffffff" ()) (face :search-outside-scope))
      (is equal '("#222222" "#f3e8fd" ()) (face :match))
      (is equal '("#c5210f" "#ffffff" (:bold)) (face :raw-byte))
      (is equal '("#c1c1c1" "#ffffff" ()) (face :hex-unprintable))
      (is equal '("#3b7f3d" "#ffffff" ()) (face :tag)))))

(define-test (themes true-colour-in-the-terminal)
  ;; a terminal that says it shows 24-bit colour gets the theme's colours exactly, whatever
  ;; TERM names: its own (kitty's, tmux's) or one not installed here; not the Linux console's 8
  (flet ((direct (term colorterm term-colours &optional (wanted :auto))
           (tuatara.frontend.tui::direct-terminal-type :term term :colorterm colorterm :wanted wanted
                                                       :term-colours term-colours)))
    (is equal "xterm-direct256" (direct "xterm-256color" "truecolor" 256))
    (is equal "xterm-direct256" (direct "xterm" "24bit" 8))
    (is equal "xterm-direct256" (direct "xterm-kitty" "truecolor" 256))
    (is equal "xterm-direct256" (direct "tmux-256color" "truecolor" 256))
    (is equal "xterm-direct256" (direct "xterm-ghostty" "truecolor" nil))
    (false (direct "xterm-256color" nil 256))
    (false (direct "linux" "truecolor" 8))
    ;; said outright: 24-bit without COLORTERM (over ssh), or fewer whatever it says
    (is equal "xterm-direct256" (direct "xterm-256color" nil 256 :24-bit))
    (false (direct "xterm-kitty" "truecolor" 256 256))
    (false (direct "xterm-kitty" "truecolor" 256 nil)))
  ;; NO_COLOR asks for none, unless the setting says otherwise
  (is eq :auto (tuatara.frontend.tui::wanted-colours :setting :auto :no-color nil))
  (is eq :auto (tuatara.frontend.tui::wanted-colours :setting :auto :no-color ""))
  (is eq nil (tuatara.frontend.tui::wanted-colours :setting :auto :no-color "1"))
  (is eq :24-bit (tuatara.frontend.tui::wanted-colours :setting :24-bit :no-color "1"))
  ;; how many are used, and why, as check-health says it
  (let ((tuatara-user:*tui-colours* :auto))
    (is equal '(16777216 "COLORTERM says so") (multiple-value-list (tuatara.frontend.tui::colours-used :auto 16777216 t)))
    (is equal '(256 "from TERM") (multiple-value-list (tuatara.frontend.tui::colours-used :auto 256 nil)))
    (is equal '(0 "NO_COLOR is set") (multiple-value-list (tuatara.frontend.tui::colours-used nil 256 nil))))
  (is equal '(16 "*tui-colours* 16") (multiple-value-list (tuatara.frontend.tui::colours-used 16 256 nil)))
  (is = 8 (tuatara.frontend.tui::colours-used 16 8 nil))
  ;; the colours of an installed terminal description, none for one not installed
  (tuatara.frontend.tui::load-ncurses)
  (is = 256 (tuatara.frontend.tui::terminfo-colours "xterm-256color"))
  (false (tuatara.frontend.tui::terminfo-colours "no-such-terminal"))
  (let ((tuatara.frontend.tui::*direct-colour* t) (tuatara.frontend.tui::*terminal-colours* #x1000000))
    (is = #x080d15 (tuatara.frontend.tui::terminal-colour "#080d15"))
    (is = 256 (tuatara.frontend.tui::terminal-colour "#000000"))       ; below 256 is the palette's
    (is = 1 (tuatara.frontend.tui::terminal-colour :red))                ; names stay the terminal's own
    (is = -1 (tuatara.frontend.tui::terminal-colour nil)))
  ;; without it, the nearest of 256: the navy ground turns grey
  (let ((tuatara.frontend.tui::*direct-colour* nil) (tuatara.frontend.tui::*terminal-colours* 256))
    (is = 233 (tuatara.frontend.tui::terminal-colour "#080d15"))))

(defun themed-session (&key style keys init actions)
  "The theme in force after a session whose frontend reports the desktop's
STYLE, with KEYS typed, INIT (a string) as the init file, and ACTIONS, a list of
(KEYS FUNCTION): KEYS typed, then FUNCTION called with the frontend in its turn."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :desktop-style style))
        (file (format nil "~atheme-init-~d.lisp" (namestring (uiop:temporary-directory)) (random 1000000))))
    (with-open-file (o file :direction :output :if-exists :supersede) (write-string (or init "") o))
    (unwind-protect
         (progn
           (when keys (tuatara.frontend.fake:fake-queue-keys f keys))
           (loop for (more function) in actions
                 do (when more (tuatara.frontend.fake:fake-queue-keys f more))
                    (let ((function function))
                      (tuatara.frontend.fake:fake-queue-action f (lambda () (funcall function f)))))
           ;; what the init file sets stays in this session
           (let ((tuatara.defaults:*config-file* (pathname file))
                 (tuatara-user:*light-theme* tuatara-user:*light-theme*)
                 (tuatara-user:*dark-theme* tuatara-user:*dark-theme*)
                 (tuatara-user:*follow-desktop-style* t))
             (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults))
           tuatara.display:*current-theme*)
      (delete-file file))))

(define-test (themes follow-the-desktop)
  (with-theme-restored
    ;; the window's theme is the desktop's style: ao-light while it is light, ao-dark while dark
    (is eq 'tuatara.defaults::ao-light (themed-session :style :light))
    (is eq 'tuatara.defaults::ao-dark (themed-session :style :dark))
    ;; a terminal says nothing of the desktop: the startup theme stays
    (is eq 'tuatara.defaults::ao-dark (themed-session :style nil))
    ;; the desktop switching while the editor runs: the theme follows
    (is eq 'tuatara.defaults::ao-light
        (themed-session :style :dark
                        :actions (list (list "l" (lambda (f) (setf (tuatara.frontend.fake:fake-desktop-style f) :light))))))
    ;; the init file's own pair of themes
    (is eq 'tuatara.defaults::plain (themed-session :style :light :init "(setf *light-theme* 'plain)"))
    ;; a theme chosen in the init file stays, whatever the desktop
    (is eq 'tuatara.defaults::ao-dark (themed-session :style :light :init "(load-theme 'ao-dark)"))
    ;; not followed at all when asked
    (is eq 'tuatara.defaults::ao-dark (themed-session :style :light :init "(setf *follow-desktop-style* nil)"))
    ;; a theme chosen with a command stays when the desktop switches after
    (is eq 'tuatara.defaults::ao-dark
        (themed-session :style :light :keys "M-x t o g g l e - l i g h t - d a r k - t h e m e RET"
                        :actions (list (list nil (lambda (f) (setf (tuatara.frontend.fake:fake-desktop-style f) :dark)))
                                       (list nil (lambda (f) (setf (tuatara.frontend.fake:fake-desktop-style f) :light))))))))

(define-test (themes every-coloured-face-themed)
  ;; a face defined with colours has its own in the Ao themes, which choose them
  ;; to keep their contrast: none falls back to the terminal's sixteen
  (dolist (theme '(tuatara.defaults::ao-dark tuatara.defaults::ao-light))
    (let ((specs (tuatara.display::theme-specs (gethash theme tuatara.display:*themes*)))
          (missing '()))
      (maphash (lambda (name face)
                 (let ((base (tuatara.display::face-base face)))
                   (when (and (or (getf base :fg) (getf base :bg)) (not (assoc name specs)))
                     (push name missing))))
               tuatara.display:*faces*)
      (is equal '() missing "~(~a~) leaves these faces in the terminal's colours: ~s" theme missing))))

(defun ao-face-table ()
  "Every face the Ao themes set, as each theme leaves it, one row for each theme
and face in the order of test/themes/ao-faces.sexp: the theme's name, the face,
its own colours, flags, inheritance, font and height, and what it resolves to."
  (loop for theme in '(tuatara.defaults::ao-dark tuatara.defaults::ao-light)
        append (let ((names (sort (remove-duplicates (mapcar #'car (tuatara.display:theme-specs (tuatara.display:find-theme theme))))
                                  #'string< :key #'symbol-name)))
                 (tuatara.display:apply-theme theme)
                 (mapcar (lambda (name)
                           (let ((f (tuatara.display:find-face name)))
                             (list* (string-downcase (symbol-name theme)) name
                                    (append (mapcar (lambda (reader) (funcall reader f))
                                                    (list #'tuatara.display:face-fg #'tuatara.display:face-bg
                                                          #'tuatara.display:face-bold #'tuatara.display:face-italic
                                                          #'tuatara.display:face-underline #'tuatara.display:face-reverse
                                                          #'tuatara.display:face-dim #'tuatara.display:face-extend
                                                          #'tuatara.display:face-inherit #'tuatara.display:face-font
                                                          #'tuatara.display::face-height))
                                            (list (multiple-value-list (tuatara.display:resolve-face-spec name)))))))
                         names))))

(define-test (themes ao-faces-pinned)
  ;; every face of both Ao themes keeps its colours, flags and inheritance exactly
  (with-theme-restored
    (let ((expected (with-open-file (in (asdf:system-relative-pathname :tuatara "test/themes/ao-faces.sexp"))
                      (loop for row = (read in nil in) until (eq row in)
                            collect (cons (string-downcase (symbol-name (first row))) (rest row)))))
          (actual (ao-face-table)))
      (is = (length expected) (length actual))
      (is equal '() (set-difference actual expected :test #'equal) "Faces not as pinned: ~s" (set-difference actual expected :test #'equal))
      (is equal '() (set-difference expected actual :test #'equal)))))

(define-test (themes role-palettes)
  (with-theme-restored
    (unwind-protect
         (progn
           ;; roles in a palette, one role standing for another, plain colours beside them
           (tuatara.display:define-theme test-roles
             "A theme for the tests, coloured by roles."
             :palette ((paper "#101010") (ink "#eeeeee") (remark ink))
             (:default :fg ink :bg paper) (:comment :fg remark :italic nil) (:keyword :fg "#ff0000"))
           (tuatara.display:apply-theme 'test-roles)
           (is equal '("#eeeeee" "#101010" ()) (multiple-value-list (tuatara.display:resolve-face-spec :comment)))
           (is equal "#ff0000" (tuatara.display:face-attribute :keyword :fg))
           ;; a theme from another with one role changed: the rest as the other has it
           (tuatara.display:define-theme test-ao-black
             "Ao on a black ground, for the tests."
             :faces-from tuatara.defaults::ao-dark :palette ((ground "#000000")))
           (tuatara.display:apply-theme 'test-ao-black)
           (is equal "#000000" (tuatara.display:face-attribute :default :bg))
           (is equal "#000000" (tuatara.display:face-attribute :line-number :bg))
           (is equal "#838a97" (tuatara.display:face-attribute :comment :fg))
           (is equal "#fa7970" (tuatara.display:face-attribute :keyword :fg))
           ;; a role no palette names is refused when the theme is defined
           (fail (tuatara.display:register-theme 'test-no-role "No colour for a role." '((:default :fg no-such-role))))
           (false (tuatara.display:find-theme 'test-no-role))
           (fail (tuatara.display:register-theme 'test-no-source "From no theme." '() :faces-from 'no-such-theme)))
      (remhash 'test-roles tuatara.display:*themes*)
      (remhash 'test-ao-black tuatara.display:*themes*)))
  ;; ao-light names a colour for every role ao-dark's faces take, and every role its own
  ;; palette refers to, so none of the dark one's colours shows through
  (let* ((dark (tuatara.display:find-theme 'tuatara.defaults::ao-dark))
         (light (tuatara.display:find-theme 'tuatara.defaults::ao-light))
         (roles (remove-duplicates
                 (loop for (nil . attributes) in (tuatara.display:theme-settings dark)
                       append (loop for (key value) on attributes by #'cddr
                                    when (and (member key '(:fg :bg)) value (symbolp value) (not (keywordp value)))
                                      collect value))))
         (aliases (loop for (nil value) in (tuatara.display:theme-palette light)
                        when (and value (symbolp value)) collect value)))
    (flet ((unnamed (names)
             (remove-if (lambda (role) (find (symbol-name role) (tuatara.display:theme-palette light)
                                             :key (lambda (entry) (symbol-name (first entry))) :test #'string=))
                        names)))
      (is eq 'tuatara.defaults::ao-dark (tuatara.display:theme-faces-from light))
      (is equal '() (tuatara.display:theme-settings light))
      (true (< 40 (length roles)))
      (is equal '() (unnamed roles))
      (is equal '() (unnamed aliases)))))

(define-test (themes list-roles-not-code-faces)
  ;; what a list shows is coloured by the lists' own faces, never by code's:
  ;; changing the colours of code leaves every list as it was
  (let ((code '(:keyword :string :constant :comment :type :function :builtin :property :number :variable)))
    (maphash (lambda (name face)
               (when (uiop:string-prefix-p "LIST-" (symbol-name name))
                 (false (member (tuatara.display:face-inherit face) code) "~s inherits a face of code" name)))
             tuatara.display:*faces*))
  (let ((special (tuatara.buffer:make-buffer "*a list test*"))
        (read-only (tuatara.buffer:make-buffer "a read-only list test")))
    (unwind-protect
         (progn
           (setf (tuatara.buffer:buffer-local read-only :read-only) t)
           (is eq :list-special (tuatara.defaults::buffer-name-face special nil))
           (is eq :list-read-only (tuatara.defaults::buffer-name-face read-only nil))
           (is eq :list-marked (tuatara.defaults::buffer-name-face read-only :marked)))
      (tuatara.buffer:kill-buffer special)
      (tuatara.buffer:kill-buffer read-only)))
  (is eq :list-dim (tuatara.defaults::package-name-face nil "kept"))
  (is eq :list-directory (tuatara.defaults::bookmark-name-face nil "folder"))
  (is eq :list-dim (tuatara.defaults::bookmark-name-face nil "gone"))
  (is equal '("✓" :list-good) (multiple-value-list (tuatara.defaults::status-cell :good)))
  (is equal '("–" :list-dim) (multiple-value-list (tuatara.defaults::status-cell :none)))
  (is equal '("✘" :list-bad) (multiple-value-list (tuatara.defaults::status-cell :missing))))

(define-test (themes list-faces)
  (with-theme-restored
    ;; every face a theme can colour, in sections by the first word of its name
    (multiple-value-bind (pieces entries) (tuatara.defaults::face-list-pieces)
      (let ((text (tuatara.defaults::pieces-text pieces :keys t))
            (faces (remove-if-not #'keywordp (coerce entries 'list))))
        (is equal (tuatara.defaults::listed-faces) (sort (copy-list faces) #'string< :key #'symbol-name))
        (true (member :list-special faces))
        (true (member :comment faces))
        (false (find-if #'tuatara.defaults::font-sample-face-p faces))
        (true (find '(:section "list") entries :test #'equal))
        (true (find (list :section tuatara.defaults::*other-faces*) entries :test #'equal))
        ;; a face's line: its name, a sample drawn in it, the first sentence of what it colours
        (let ((line (find-if (lambda (l) (search ":list-special " l)) (uiop:split-string text :separator '(#\Newline)))))
          (true (search tuatara.defaults::*face-sample* line))
          (true (search "Something Tuatara made rather than a file of yours, in a list:" line)))
        (true (find :list-special pieces :key (lambda (p) (and (consp p) (cdr p)))))))
    ;; M-x list-faces shows them in *Faces*, narrowed by search filters as every list
    (multiple-value-bind (f editor) (run-keys "M-x l i s t - f a c e s RET" :text "" :width 120 :height 20)
      (declare (ignore f))
      (let ((list (session-buffer editor)))
        (is string= "*Faces*" (tuatara.buffer:buffer-name list))
        (true (tuatara.defaults::search-filters-list-p list))
        (true (search ":list-dim" (session-text editor)))))))

(define-test (themes describe-face)
  (with-theme-restored
    (tuatara.display:apply-theme 'tuatara.defaults::ao-dark)
    (flet ((described (name) (tuatara.defaults::pieces-text (tuatara.defaults::describe-face-pieces name) :keys t)))
      (let ((text (described :list-dim)))
        (true (uiop:string-prefix-p ":list-dim is a face." text))
        (true (search "What is of less interest in a list" text))
        (true (search "Its definition: :fg :bright-black" text))
        ;; the theme's setting by its role, and the colour the role stands for
        (true (search "The theme ao-dark sets: :fg text-dim (#838a97)" text))
        (true (search "Colour             #838a97" text)))
      ;; an inherited attribute says where it comes from, the face it inherits a link
      (let ((text (described :list-marked)))
        (true (search "Inherits from :warning" text))
        (true (search "Bold               yes, from :warning" text)))
      (true (find-if (lambda (piece) (and (consp piece) (consp (cdr piece)) (eq (second piece) :button)))
                     (tuatara.defaults::describe-face-pieces :list-marked))))
    ;; your own setting is shown too
    (tuatara.display:set-user-face :list-note :italic t)
    (true (search "Your init file sets: :italic t"
                  (tuatara.defaults::pieces-text (tuatara.defaults::describe-face-pieces :list-note) :keys t)))
    ;; M-x describe-face asks for the face and shows it in *Help*
    (let ((f (run-keys "M-x d e s c r i b e - f a c e RET : l i s t - s i z e RET" :text "" :width 100 :height 20)))
      (true (tuatara.frontend.fake:fake-frame-showing f ":list-size is a face.")))))
