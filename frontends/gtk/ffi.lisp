;;;; frontends/gtk/ffi.lisp --- The few calls into GTK4, GLib, Pango and Cairo the window needs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The window is one drawing area that the editor's own layout paints, so
;;;; only a narrow part of GTK is used: a window, a drawing area, key, focus,
;;;; scroll and pointer controllers, click and drag gestures, an input method,
;;;; the clipboard, Pango for text and Cairo for the rest. The libraries are
;;;; loaded only when a window is opened.

(defpackage #:tuatara.gtk
  (:use #:cl)
  (:import-from #:tuatara.io #:define-c-function #:null-pointer #:null-pointer-p #:sap-string)
  (:documentation "The calls into GTK4, GDK, GLib, GObject, GIO, Pango and Cairo that Tuatara's window makes, loaded when a window is opened.")
  (:export #:load-gtk-libraries #:+pango-scale+ #:with-gtk-float-traps #:callback
           #:pango-cairo-layout-path
           ;; glib and gobject
           #:g-signal-connect #:g-idle-add #:g-timeout-add #:g-main-context-iteration #:g-object-unref #:g-free
           #:g-settings-string #:g-object-class-find-property
           ;; gtk and gdk
           #:gtk-init #:gtk-widget-get-frame-clock #:g-bytes-new #:g-bytes-unref #:gdk-memory-texture-new #:gdk-texture-download #:gtk-picture-new #:gtk-picture-set-paintable #:gtk-picture-set-content-fit #:gtk-widget-set-size-request #:gtk-widget-set-can-target #:gtk-widget-set-visible #:gtk-fixed-new #:gtk-fixed-put #:gtk-fixed-set-child-transform #:gsk-transform-unref #:gsk-transform-move #:gtk-fixed-remove #:gtk-overlay-new #:gtk-overlay-set-child #:gtk-overlay-add-overlay #:gtk-widget-realize #:gtk-native-get-renderer #:g-type-name-from-instance #:g-set-prgname #:g-set-application-name #:gtk-window-new #:gtk-window-set-title #:gtk-window-set-default-size #:gtk-window-set-child
           #:gtk-window-present #:gtk-window-destroy #:gtk-drawing-area-new #:gtk-drawing-area-set-draw-func
           #:gtk-widget-set-focusable #:gtk-widget-grab-focus #:gtk-widget-queue-draw #:gtk-widget-add-controller
           #:gtk-widget-get-width #:gtk-widget-get-height #:gtk-widget-set-cursor-from-name
           #:gtk-event-controller-key-new #:gtk-event-controller-key-set-im-context #:gtk-event-controller-set-propagation-phase #:gtk-event-controller-focus-new
           #:gtk-event-controller-scroll-new #:gtk-event-controller-motion-new
           #:gtk-event-controller-get-current-event-state #:gtk-gesture-click-new #:gtk-gesture-drag-new
           #:gtk-gesture-single-set-button #:gtk-im-multicontext-new #:gtk-im-context-set-client-widget
           #:gtk-im-context-focus-in #:gtk-im-context-focus-out #:gtk-im-context-set-cursor-location
           #:gtk-im-context-reset #:gtk-im-context-get-preedit-string #:pango-attr-list-unref
           #:gtk-window-fullscreen #:gtk-window-unfullscreen #:gtk-window-is-fullscreen
           #:gtk-window-maximize #:gtk-window-unmaximize #:gtk-window-is-maximized
           #:gtk-drop-target-new #:gdk-file-list-get-type #:gdk-file-list-get-files #:g-value-get-boxed
           #:g-file-get-path #:g-slist-free
           #:gdk-display-get-default #:gdk-display-get-clipboard #:gdk-display-get-primary-clipboard #:gdk-clipboard-set-text
           #:gtk-icon-theme-get-for-display #:gtk-icon-theme-add-search-path #:gtk-window-set-default-icon-name
           #:gdk-clipboard-read-text-async #:gdk-clipboard-read-text-finish #:gdk-keyval-to-unicode
           ;; pango
           #:pango-cairo-create-layout #:pango-layout-set-font-description #:pango-layout-set-text
           #:pango-layout-get-pixel-size #:pango-layout-get-baseline #:pango-cairo-show-layout
           #:pango-font-description-from-string #:pango-font-description-free #:pango-font-description-set-weight
           #:pango-font-description-set-style #:pango-font-description-get-size #:pango-font-description-set-size
           #:pango-font-description-get-family #:pango-font-description-to-string
           #:pango-cairo-font-map-get-default #:pango-font-map-list-families #:pango-font-family-get-name
           #:pango-font-family-is-monospace #:pango-font-family-list-faces #:pango-font-face-get-face-name
           #:pango-font-face-describe #:pango-font-face-is-synthesized
           ;; cairo
           #:cairo-set-source-rgb #:cairo-rectangle #:cairo-fill #:cairo-move-to #:cairo-line-to #:cairo-stroke
           #:cairo-translate #:cairo-save #:cairo-restore #:cairo-clip #:cairo-set-antialias #:gtk-settings-get-default #:g-object-get-int #:widget-theme-colour
           #:cairo-set-line-width #:cairo-paint #:cairo-image-surface-create #:cairo-create #:cairo-destroy
           #:cairo-surface-destroy #:cairo-surface-set-device-scale #:cairo-surface-flush #:cairo-surface-write-to-png
           #:cairo-image-surface-get-data #:cairo-image-surface-get-stride
           #:cairo-scale #:cairo-set-source-surface #:cairo-surface-mark-dirty #:gtk-widget-get-scale-factor))

(in-package #:tuatara.gtk)

(defmacro with-gtk-float-traps (&body body)
  "Run BODY with the floating-point traps GTK, Pango and Cairo do not expect
turned off: they divide by zero and overflow on purpose, and would otherwise
stop with a Lisp error."
  `(sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
     ,@body))

(defparameter *gtk-libraries*
  '(("libglib-2.0.so.0" "libglib-2.0.so") ("libgobject-2.0.so.0" "libgobject-2.0.so")
    ("libgio-2.0.so.0" "libgio-2.0.so") ("libcairo.so.2" "libcairo.so")
    ("libpango-1.0.so.0" "libpango-1.0.so") ("libpangocairo-1.0.so.0" "libpangocairo-1.0.so")
    ("libgtk-4.so.1" "libgtk-4.so"))
  "The libraries a window needs, in the order they are loaded, each by the
names it is looked for by, the first found taken.")

(defun load-gtk-libraries ()
  "Load GTK4 and the libraries under it, once; signals when one is missing."
  (dolist (names *gtk-libraries*)
    (unless (some #'tuatara.runtime:foreign-library-p names)
      (error "The library ~a is not installed" (first names)))))

(defvar *callback-cells* '()
  "A cell (POINTER NAME) for each place a callback's C function pointer is
asked for (CALLBACK): POINTER, found the first time, NIL again once the
program is saved, as a callback may be elsewhere when it starts again.")

(defun callback-cell (name)
  "A new cell of *CALLBACK-CELLS* for the callback NAME."
  (let ((cell (list nil name)))
    (push cell *callback-cells*)
    cell))

(defun callback-pointer (cell)
  "The C function pointer of the callback CELL is for, found once and kept in it."
  (or (first cell)
      (setf (first cell) (sb-alien:alien-sap (sb-alien:alien-callable-function (second cell))))))

(defun forget-callback-pointers ()
  "Forget the callbacks' pointers kept, which a saved program finds again."
  (dolist (cell *callback-cells*) (setf (first cell) nil)))

(pushnew 'forget-callback-pointers sb-ext:*save-hooks*)

(defmacro callback (name)
  "The C function pointer of the callback NAME, one DEFINE-CALLBACK made:
found the first time this place asks, kept after, nothing made for it."
  `(callback-pointer (load-time-value (callback-cell ',name))))

(defconstant +pango-scale+ 1024
  "Pango's units a pixel.")

;;; GLib and GObject

(define-c-function ("g_signal_connect_data" %g-signal-connect-data) :ulong
  "Connect HANDLER to SIGNAL of INSTANCE, with DATA, DESTROY and FLAGS as GLib takes them; the handler's number."
  (instance :pointer) (signal :string) (handler :pointer) (data :pointer) (destroy :pointer) (flags :int))

(defun g-signal-connect (instance signal handler)
  "Call the C callback HANDLER when INSTANCE emits SIGNAL."
  (%g-signal-connect-data instance signal handler (null-pointer) (null-pointer) 0))

(define-c-function ("g_idle_add" g-idle-add) :uint
  "Call FUNCTION with DATA from the main loop when it is idle; any thread may ask."
  (function :pointer) (data :pointer))

(define-c-function ("g_timeout_add" g-timeout-add) :uint
  "Call FUNCTION with DATA from the main loop every MILLISECONDS while it answers true."
  (milliseconds :uint) (function :pointer) (data :pointer))

(define-c-function ("g_main_context_iteration" g-main-context-iteration) :boolean
  "Run one round of the main loop of CONTEXT (null for the default), waiting for something to do when MAY-BLOCK."
  (context :pointer) (may-block :boolean))

(define-c-function ("g_object_unref" g-object-unref) :void
  "Let go of a reference to OBJECT."
  (object :pointer))

(define-c-function ("g_free" g-free) :void
  "Free MEMORY that GLib allocated."
  (memory :pointer))

(define-c-function ("g_object_class_find_property" g-object-class-find-property) :pointer
  "The property NAME of objects of CLASS, a GObject class; a null pointer when they have none."
  (class :pointer) (name :string))

(define-c-function ("g_settings_schema_source_get_default" %g-settings-schema-source-get-default) :pointer
  "The source of the settings schemas installed on this system; a null pointer when there are none.")
(define-c-function ("g_settings_schema_source_lookup" %g-settings-schema-source-lookup) :pointer
  "The settings schema SCHEMA in SOURCE, or in those under it with RECURSIVE; a null pointer when none."
  (source :pointer) (schema :string) (recursive :boolean))
(define-c-function ("g_settings_schema_unref" %g-settings-schema-unref) :void
  "Let go of SCHEMA, a settings schema."
  (schema :pointer))
(define-c-function ("g_settings_new" %g-settings-new) :pointer
  "New settings of the schema named SCHEMA, an installed one."
  (schema :string))
(define-c-function ("g_settings_get_string" %g-settings-get-string) :pointer
  "The string setting KEY of SETTINGS, newly made: to be freed with g_free."
  (settings :pointer) (key :string))
(define-c-function ("g_settings_schema_has_key" %g-settings-schema-has-key) :boolean
  "True when SCHEMA, a settings schema, has a setting KEY."
  (schema :pointer) (key :string))

(defun g-settings-string (schema-id key)
  "The string setting KEY of the desktop settings SCHEMA-ID, or NIL when there
is no such schema or key (GSettings would end the program on an unknown one)."
  (let ((source (%g-settings-schema-source-get-default)))
    (unless (null-pointer-p source)
      (let ((schema (%g-settings-schema-source-lookup source schema-id t)))
        (unless (null-pointer-p schema)
          (unwind-protect
               (when (%g-settings-schema-has-key schema key)
                 (let ((settings (%g-settings-new schema-id)))
                   (unwind-protect
                        (let ((value (%g-settings-get-string settings key)))
                          (unwind-protect (sap-string value)
                            (g-free value)))
                     (g-object-unref settings))))
            (%g-settings-schema-unref schema)))))))

;;; GTK and GDK

(define-c-function ("gtk_init" gtk-init) :void
  "Start GTK: connect to the display.")

(define-c-function ("g_set_prgname" g-set-prgname) :void
  "Name the program NAME for the desktop, which matches its windows to the
launcher (NAME.desktop) by it."
  (name :string))

(define-c-function ("g_set_application_name" g-set-application-name) :void
  "Give the program the NAME people read, in the desktop's lists and dialogue boxes."
  (name :string))

(define-c-function ("gtk_window_new" gtk-window-new) :pointer
  "A new top-level window.")

(define-c-function ("gtk_window_set_title" gtk-window-set-title) :void
  "Give WINDOW the title TITLE."
  (window :pointer) (title :string))

(define-c-function ("gtk_window_set_default_size" gtk-window-set-default-size) :void
  "Open WINDOW WIDTH by HEIGHT pixels."
  (window :pointer) (width :int) (height :int))

(define-c-function ("gtk_window_set_child" gtk-window-set-child) :void
  "Make CHILD the widget WINDOW holds."
  (window :pointer) (child :pointer))

(define-c-function ("gtk_window_present" gtk-window-present) :void
  "Show WINDOW."
  (window :pointer))

(define-c-function ("g_bytes_new" g-bytes-new) :pointer
  "New bytes holding a copy of the SIZE octets at DATA."
  (data :pointer) (size :size))

(define-c-function ("g_bytes_unref" g-bytes-unref) :void
  "Let go of BYTES."
  (bytes :pointer))

(define-c-function ("gdk_memory_texture_new" gdk-memory-texture-new) :pointer
  "A texture WIDTH by HEIGHT pixels of BYTES, their rows STRIDE octets apart,
in FORMAT (29: 32 bits a pixel, blue first, the fourth unused, as cairo's
RGB24 on this kind of processor); the graphics card holds it once drawn."
  (width :int) (height :int) (format :int) (bytes :pointer) (stride :size))

(define-c-function ("gdk_texture_download" gdk-texture-download) :void
  "Copy TEXTURE's pixels to DATA, rows STRIDE octets apart, 32 bits a pixel, blue first."
  (texture :pointer) (data :pointer) (stride :size))

(define-c-function ("gtk_picture_new" gtk-picture-new) :pointer
  "A widget showing a picture, none yet.")

(define-c-function ("gtk_picture_set_paintable" gtk-picture-set-paintable) :void
  "Have PICTURE show PAINTABLE, a texture."
  (picture :pointer) (paintable :pointer))

(define-c-function ("gtk_picture_set_content_fit" gtk-picture-set-content-fit) :void
  "How PICTURE fits what it shows to its size: 0, filling it."
  (picture :pointer) (fit :int))

(define-c-function ("gtk_widget_set_size_request" gtk-widget-set-size-request) :void
  "Have WIDGET take WIDTH by HEIGHT at least."
  (widget :pointer) (width :int) (height :int))

(define-c-function ("gtk_widget_set_can_target" gtk-widget-set-can-target) :void
  "Whether the pointer's events reach WIDGET, or pass through it to what is below."
  (widget :pointer) (can :boolean))

(define-c-function ("gtk_widget_set_visible" gtk-widget-set-visible) :void
  "Show WIDGET, or hide it."
  (widget :pointer) (visible :boolean))

(define-c-function ("gtk_fixed_new" gtk-fixed-new) :pointer
  "A widget whose children stand where they are put.")

(define-c-function ("gtk_fixed_put" gtk-fixed-put) :void
  "Put WIDGET in FIXED at X Y."
  (fixed :pointer) (widget :pointer) (x :double) (y :double))

(define-c-function ("gtk_fixed_set_child_transform" gtk-fixed-set-child-transform) :void
  "Have WIDGET, in FIXED, drawn as TRANSFORM moves and scales it."
  (fixed :pointer) (widget :pointer) (transform :pointer))

(define-c-function ("gsk_transform_translate" %gsk-transform-translate) :pointer
  "The transform NEXT moved by POINT, a graphene point."
  (next :pointer) (point :pointer))

(define-c-function ("gsk_transform_unref" gsk-transform-unref) :void
  "Let go of TRANSFORM."
  (transform :pointer))

(defun gsk-transform-move (x y)
  "A new transform moving what it is given X across and Y down."
  (sb-alien:with-alien ((point (array single-float 2)))
    (setf (sb-alien:deref point 0) (float x 1f0)
          (sb-alien:deref point 1) (float y 1f0))
    (%gsk-transform-translate (null-pointer) (sb-alien:alien-sap point))))

(define-c-function ("gtk_fixed_remove" gtk-fixed-remove) :void
  "Take WIDGET out of FIXED, letting go of it."
  (fixed :pointer) (widget :pointer))

(define-c-function ("gtk_overlay_new" gtk-overlay-new) :pointer
  "A widget showing others over its child.")

(define-c-function ("gtk_overlay_set_child" gtk-overlay-set-child) :void
  "Make CHILD the widget OVERLAY shows others over."
  (overlay :pointer) (child :pointer))

(define-c-function ("gtk_overlay_add_overlay" gtk-overlay-add-overlay) :void
  "Show WIDGET over OVERLAY's child, over those added before."
  (overlay :pointer) (widget :pointer))

(define-c-function ("gtk_widget_get_frame_clock" gtk-widget-get-frame-clock) :pointer
  "The clock WIDGET's window paints its frames by, once it is realised."
  (widget :pointer))

(define-c-function ("gtk_widget_realize" gtk-widget-realize) :void
  "Make WIDGET's resources on the display now: for a window, its surface and
the renderer that draws it."
  (widget :pointer))

(define-c-function ("gtk_native_get_renderer" gtk-native-get-renderer) :pointer
  "The renderer that draws NATIVE, a window, once it is realised; a null pointer before."
  (native :pointer))

(define-c-function ("g_type_name_from_instance" g-type-name-from-instance) :string
  "The name of the type of INSTANCE, a GObject: GskGLRenderer for one."
  (instance :pointer))

(define-c-function ("gtk_window_destroy" gtk-window-destroy) :void
  "Close WINDOW for good."
  (window :pointer))

(define-c-function ("gtk_drawing_area_new" gtk-drawing-area-new) :pointer
  "A new drawing area.")

(define-c-function ("gtk_drawing_area_set_draw_func" gtk-drawing-area-set-draw-func) :void
  "Paint AREA with the C callback FUNCTION (area, cairo, width, height, data)."
  (area :pointer) (function :pointer) (data :pointer) (destroy :pointer))

(define-c-function ("gtk_widget_set_focusable" gtk-widget-set-focusable) :void
  "Let WIDGET take the keyboard focus, or not."
  (widget :pointer) (focusable :boolean))

(define-c-function ("gtk_widget_grab_focus" gtk-widget-grab-focus) :boolean
  "Give WIDGET the keyboard focus."
  (widget :pointer))

(define-c-function ("gtk_widget_queue_draw" gtk-widget-queue-draw) :void
  "Have WIDGET painted again soon."
  (widget :pointer))

(define-c-function ("gtk_widget_add_controller" gtk-widget-add-controller) :void
  "Give WIDGET the event CONTROLLER, which it then owns."
  (widget :pointer) (controller :pointer))

(define-c-function ("gtk_widget_get_width" gtk-widget-get-width) :int
  "WIDGET's width in pixels."
  (widget :pointer))

(define-c-function ("gtk_widget_get_height" gtk-widget-get-height) :int
  "WIDGET's height in pixels."
  (widget :pointer))

(define-c-function ("gtk_widget_set_cursor_from_name" gtk-widget-set-cursor-from-name) :void
  "Show the mouse pointer called NAME over WIDGET."
  (widget :pointer) (name :string))

(define-c-function ("gtk_event_controller_key_new" gtk-event-controller-key-new) :pointer
  "A new controller for keys.")

(define-c-function ("gtk_event_controller_set_propagation_phase" gtk-event-controller-set-propagation-phase) :void
  "Have CONTROLLER see events in PHASE: 1 on their way down to the widget they
are for, before that widget's own controllers and its input method (capture);
3 on their way up (bubble, the usual)."
  (controller :pointer) (phase :int))

(define-c-function ("gtk_event_controller_key_set_im_context" gtk-event-controller-key-set-im-context) :void
  "Pass the keys CONTROLLER sees through the input method CONTEXT first."
  (controller :pointer) (context :pointer))

(define-c-function ("gtk_event_controller_focus_new" gtk-event-controller-focus-new) :pointer
  "A new controller for the keyboard focus.")

(define-c-function ("gtk_event_controller_scroll_new" gtk-event-controller-scroll-new) :pointer
  "A new controller for scrolling, FLAGS saying which ways (1 vertical)."
  (flags :int))

(define-c-function ("gtk_event_controller_motion_new" gtk-event-controller-motion-new) :pointer
  "A new controller for the pointer moving over a widget.")

(define-c-function ("gtk_event_controller_get_current_event_state" gtk-event-controller-get-current-event-state) :uint
  "The modifiers held for the event CONTROLLER is handling now."
  (controller :pointer))

(define-c-function ("gtk_gesture_click_new" gtk-gesture-click-new) :pointer
  "A new gesture for presses of a mouse button, counting those in quick succession.")

(define-c-function ("gtk_gesture_drag_new" gtk-gesture-drag-new) :pointer
  "A new gesture for moving the mouse with a button held.")

(define-c-function ("gtk_gesture_single_set_button" gtk-gesture-single-set-button) :void
  "Have GESTURE follow mouse BUTTON alone: 1 the first, 0 any."
  (gesture :pointer) (button :uint))

(define-c-function ("gtk_im_multicontext_new" gtk-im-multicontext-new) :pointer
  "A new input method context, for the input method the desktop chooses.")

(define-c-function ("gtk_im_context_set_client_widget" gtk-im-context-set-client-widget) :void
  "Tie the input method CONTEXT to WIDGET."
  (context :pointer) (widget :pointer))

(define-c-function ("gtk_window_fullscreen" gtk-window-fullscreen) :void
  "Ask for WINDOW to fill the screen, without its title bar."
  (window :pointer))

(define-c-function ("gtk_window_unfullscreen" gtk-window-unfullscreen) :void
  "Ask for WINDOW to leave filling the screen."
  (window :pointer))

(define-c-function ("gtk_window_is_fullscreen" gtk-window-is-fullscreen) :boolean
  "True while WINDOW fills the screen."
  (window :pointer))

(define-c-function ("gtk_window_maximize" gtk-window-maximize) :void
  "Ask for WINDOW to be as large as the screen allows."
  (window :pointer))

(define-c-function ("gtk_window_unmaximize" gtk-window-unmaximize) :void
  "Ask for WINDOW to go back to its size before it was maximised."
  (window :pointer))

(define-c-function ("gtk_window_is_maximized" gtk-window-is-maximized) :boolean
  "True while WINDOW is maximised."
  (window :pointer))

(define-c-function ("gtk_drop_target_new" gtk-drop-target-new) :pointer
  "A new controller taking what is dropped on a widget when it is of TYPE (a
GType), for the ACTIONS given (1: copying)."
  (type :ulong) (actions :int))

(define-c-function ("gdk_file_list_get_type" gdk-file-list-get-type) :ulong
  "The GType of a list of files, as dropped from a file manager.")

(define-c-function ("gdk_file_list_get_files" gdk-file-list-get-files) :pointer
  "The files of LIST, a GSList of GFile to free with G-SLIST-FREE (the files stay LIST's)."
  (list :pointer))

(define-c-function ("g_value_get_boxed" g-value-get-boxed) :pointer
  "The boxed value VALUE holds, still VALUE's."
  (value :pointer))

(define-c-function ("g_file_get_path" g-file-get-path) :pointer
  "The local path of FILE, to free with G-FREE; null for a file with none (on another machine)."
  (file :pointer))

(define-c-function ("g_slist_free" g-slist-free) :void
  "Free the cells of the singly linked LIST, not what they hold."
  (list :pointer))

(define-c-function ("gtk_im_context_reset" gtk-im-context-reset) :void
  "Have the input method CONTEXT drop what it is composing."
  (context :pointer))

(define-c-function ("gtk_im_context_get_preedit_string" gtk-im-context-get-preedit-string) :void
  "Store at TEXT what the input method CONTEXT is composing, as UTF-8 to free
with G-FREE; at ATTRIBUTES its Pango attributes, to let go of with
PANGO-ATTR-LIST-UNREF; at CURSOR where its cursor is in it."
  (context :pointer) (text :pointer) (attributes :pointer) (cursor :pointer))

(define-c-function ("gtk_im_context_focus_in" gtk-im-context-focus-in) :void
  "Tell the input method CONTEXT its widget has the focus."
  (context :pointer))

(define-c-function ("gtk_im_context_focus_out" gtk-im-context-focus-out) :void
  "Tell the input method CONTEXT its widget lost the focus."
  (context :pointer))

(define-c-function ("gtk_im_context_set_cursor_location" %gtk-im-context-set-cursor-location) :void
  "Tell CONTEXT, an input method, where the cursor is: AREA, a GdkRectangle."
  (context :pointer) (area :pointer))

(defun gtk-im-context-set-cursor-location (context x y width height)
  "Tell the input method CONTEXT where the cursor is, so that its window shows beside it."
  ;; a GdkRectangle: four ints
  (sb-alien:with-alien ((area (array sb-alien:int 4)))
    (setf (sb-alien:deref area 0) x (sb-alien:deref area 1) y
          (sb-alien:deref area 2) width (sb-alien:deref area 3) height)
    (%gtk-im-context-set-cursor-location context (sb-alien:alien-sap area))))

(define-c-function ("gdk_display_get_default" gdk-display-get-default) :pointer
  "The display GTK is connected to.")

(define-c-function ("gtk_icon_theme_get_for_display" gtk-icon-theme-get-for-display) :pointer
  "The icon theme of DISPLAY, where windows' icons are looked up by name."
  (display :pointer))

(define-c-function ("gtk_icon_theme_add_search_path" gtk-icon-theme-add-search-path) :void
  "Have THEME also look for icons under the directory PATH, laid out as the
icon themes' directories are (hicolor/48x48/apps/NAME.png)."
  (theme :pointer) (path :string))

(define-c-function ("gtk_window_set_default_icon_name" gtk-window-set-default-icon-name) :void
  "Give every window without an icon of its own the icon called NAME in the icon theme."
  (name :string))

(define-c-function ("gdk_display_get_clipboard" gdk-display-get-clipboard) :pointer
  "The clipboard of DISPLAY."
  (display :pointer))

(define-c-function ("gdk_display_get_primary_clipboard" gdk-display-get-primary-clipboard) :pointer
  "The primary selection of DISPLAY: the text selected last, which the middle button pastes."
  (display :pointer))

(define-c-function ("gdk_clipboard_set_text" gdk-clipboard-set-text) :void
  "Put TEXT on CLIPBOARD."
  (clipboard :pointer) (text :string))

(define-c-function ("gdk_clipboard_read_text_async" gdk-clipboard-read-text-async) :void
  "Ask for the text on CLIPBOARD; the C callback CALLBACK (clipboard, result, data) is called with it."
  (clipboard :pointer) (cancellable :pointer) (callback :pointer) (data :pointer))

(define-c-function ("gdk_clipboard_read_text_finish" gdk-clipboard-read-text-finish) :pointer
  "The text a read of CLIPBOARD got, from RESULT, to be freed with G-FREE; null for none."
  (clipboard :pointer) (result :pointer) (error :pointer))

(define-c-function ("gdk_keyval_to_unicode" gdk-keyval-to-unicode) :uint32
  "The character KEYVAL types, or 0."
  (keyval :uint))

;;; Pango

(define-c-function ("pango_cairo_create_layout" pango-cairo-create-layout) :pointer
  "A new layout for text drawn on CAIRO."
  (cairo :pointer))

(define-c-function ("pango_layout_set_font_description" pango-layout-set-font-description) :void
  "Lay LAYOUT out in the font DESCRIPTION."
  (layout :pointer) (description :pointer))

(define-c-function ("pango_layout_set_text" %pango-layout-set-text) :void
  "Make the LENGTH octets of UTF-8 at TEXT what LAYOUT holds."
  (layout :pointer) (text :pointer) (length :int))

(defun pango-layout-set-text (layout text)
  "Make TEXT what LAYOUT holds."
  (tuatara.io:with-c-string (octets text :length length)
    (%pango-layout-set-text layout octets length)))

(define-c-function ("pango_layout_get_pixel_size" %pango-layout-get-pixel-size) :void
  "Store LAYOUT's width and height in pixels at WIDTH and HEIGHT, two pointers to ints."
  (layout :pointer) (width :pointer) (height :pointer))

(defun pango-layout-get-pixel-size (layout)
  "Two values: LAYOUT's width and height in pixels."
  (sb-alien:with-alien ((width sb-alien:int 0) (height sb-alien:int 0))
    (%pango-layout-get-pixel-size layout (sb-alien:alien-sap (sb-alien:addr width)) (sb-alien:alien-sap (sb-alien:addr height)))
    (values width height)))

(define-c-function ("pango_layout_get_baseline" pango-layout-get-baseline) :int
  "How far LAYOUT's first baseline is below its top, in Pango units."
  (layout :pointer))

(define-c-function ("pango_cairo_show_layout" pango-cairo-show-layout) :void
  "Draw LAYOUT on CAIRO at the current point, its top left there."
  (cairo :pointer) (layout :pointer))

(define-c-function ("pango_cairo_layout_path" pango-cairo-layout-path) :void
  "Add the outlines of LAYOUT's glyphs to CAIRO's path, at the current point."
  (cairo :pointer) (layout :pointer))

(define-c-function ("pango_font_description_from_string" pango-font-description-from-string) :pointer
  "The font DESCRIPTION reads as, \"Adwaita Mono 11\" for one."
  (description :string))

(define-c-function ("pango_font_description_get_size" pango-font-description-get-size) :int
  "The size DESCRIPTION asks for, in points times +PANGO-SCALE+; 0 when it names none."
  (description :pointer))

(define-c-function ("pango_font_description_set_size" pango-font-description-set-size) :void
  "Make DESCRIPTION ask for SIZE, in points times +PANGO-SCALE+."
  (description :pointer) (size :int))

(define-c-function ("pango_font_description_get_family" pango-font-description-get-family) :string
  "The family DESCRIPTION names, or NIL when it names none."
  (description :pointer))

(define-c-function ("pango_font_description_to_string" %pango-font-description-to-string) :pointer
  "DESCRIPTION written as a string Pango reads back, newly made: to be freed with g_free."
  (description :pointer))

(defun pango-font-description-to-string (description)
  "DESCRIPTION written out as Pango reads it back (\"Adwaita Mono Bold 12\")."
  (let ((pointer (%pango-font-description-to-string description)))
    (prog1 (sap-string pointer)
      (g-free pointer))))

(define-c-function ("pango_cairo_font_map_get_default" pango-cairo-font-map-get-default) :pointer
  "The font map of this thread for drawing with Cairo: every font installed.")

(define-c-function ("pango_font_map_list_families" pango-font-map-list-families) :void
  "Store at FAMILIES an array of MAP's font families, to free with G-FREE, and their number at COUNT."
  (map :pointer) (families :pointer) (count :pointer))

(define-c-function ("pango_font_family_get_name" pango-font-family-get-name) :string
  "The name of the font FAMILY."
  (family :pointer))

(define-c-function ("pango_font_family_is_monospace" pango-font-family-is-monospace) :boolean
  "True when every character of the font FAMILY has the same width."
  (family :pointer))

(define-c-function ("pango_font_family_list_faces" pango-font-family-list-faces) :void
  "Store at FACES an array of FAMILY's variants, to free with G-FREE, and their number at COUNT."
  (family :pointer) (faces :pointer) (count :pointer))

(define-c-function ("pango_font_face_get_face_name" pango-font-face-get-face-name) :string
  "The name of the font FACE within its family: Regular, Bold Italic, Light and the like."
  (face :pointer))

(define-c-function ("pango_font_face_describe" pango-font-face-describe) :pointer
  "A description of the font FACE, without a size, to free with PANGO-FONT-DESCRIPTION-FREE."
  (face :pointer))

(define-c-function ("pango_font_face_is_synthesized" pango-font-face-is-synthesized) :boolean
  "True when FACE is made by slanting or thickening another rather than drawn as itself."
  (face :pointer))

(define-c-function ("pango_font_description_free" pango-font-description-free) :void
  "Free the font DESCRIPTION."
  (description :pointer))

(define-c-function ("pango_attr_list_unref" pango-attr-list-unref) :void
  "Let go of the attribute LIST."
  (list :pointer))

(define-c-function ("pango_font_description_set_weight" pango-font-description-set-weight) :void
  "Give the font DESCRIPTION the WEIGHT, 400 normal and 700 bold."
  (description :pointer) (weight :int))

(define-c-function ("pango_font_description_set_style" pango-font-description-set-style) :void
  "Give the font DESCRIPTION the STYLE, 0 upright and 2 italic."
  (description :pointer) (style :int))

;;; Cairo

(define-c-function ("cairo_set_source_rgb" cairo-set-source-rgb) :void
  "Draw on CAIRO in the colour RED GREEN BLUE, each from 0 to 1."
  (cairo :pointer) (red :double) (green :double) (blue :double))

(define-c-function ("cairo_rectangle" cairo-rectangle) :void
  "Add the rectangle at X Y, WIDTH by HEIGHT, to CAIRO's path."
  (cairo :pointer) (x :double) (y :double) (width :double) (height :double))

(define-c-function ("cairo_translate" cairo-translate) :void
  "Move CAIRO's origin by X and Y: what is drawn after lands that far over."
  (cairo :pointer) (x :double) (y :double))

(define-c-function ("cairo_clip" cairo-clip) :void
  "Draw on CAIRO only within its path from now on (until CAIRO-RESTORE), and clear the path."
  (cairo :pointer))

(define-c-function ("cairo_set_antialias" cairo-set-antialias) :void
  "Set how the edges of shapes CR draws are smoothed: ANTIALIAS 0 the default,
1 none (whole pixels). Text is smoothed as its font options say."
  (cr :pointer) (antialias :int))

(define-c-function ("cairo_save" cairo-save) :void
  "Keep CAIRO's state (its origin, colour, clip) to come back to with CAIRO-RESTORE."
  (cairo :pointer))

(define-c-function ("cairo_restore" cairo-restore) :void
  "Go back to the state of CAIRO last kept with CAIRO-SAVE."
  (cairo :pointer))

(define-c-function ("gtk_settings_get_default" gtk-settings-get-default) :pointer
  "GTK's settings for the display: fonts, theme, the desktop's light or dark style.")

(define-c-function ("gtk_widget_get_style_context" gtk-widget-get-style-context) :pointer
  "The style of WIDGET as the desktop's theme gives it."
  (widget :pointer))

(define-c-function ("gtk_style_context_lookup_color" gtk-style-context-lookup-color) :boolean
  "Fill RGBA (four floats) with the colour the desktop's theme names NAME for CONTEXT; true when it names one."
  (context :pointer) (name :string) (rgba :pointer))

(defun widget-theme-colour (widget &rest names)
  "The first of NAMES the desktop's theme gives a colour for WIDGET, as a list
of three numbers from 0 to 1; NIL when it names none of them. On GTK's thread."
  (let ((context (gtk-widget-get-style-context widget)))
    (unless (null-pointer-p context)
      (sb-alien:with-alien ((rgba (array single-float 4)))
        (dolist (name names)
          (when (gtk-style-context-lookup-color context name (sb-alien:alien-sap rgba))
            (return (loop for i below 3 collect (float (sb-alien:deref rgba i) 1d0)))))))))

(define-c-function ("g_object_get" %g-object-get) :void
  "Store OBJECT's property NAME at VALUE; END, a null pointer, ends the list
of properties, of which there may be more."
  (object :pointer) (name :string) (value :pointer) (end :pointer))

(defun g-object-get-int (object property)
  "The value of the integer (or enumeration) PROPERTY of the GObject OBJECT."
  (sb-alien:with-alien ((value sb-alien:int 0))
    (%g-object-get object property (sb-alien:alien-sap (sb-alien:addr value)) (null-pointer))
    value))

(define-c-function ("cairo_fill" cairo-fill) :void
  "Fill CAIRO's path."
  (cairo :pointer))

(define-c-function ("cairo_move_to" cairo-move-to) :void
  "Put CAIRO's current point at X Y."
  (cairo :pointer) (x :double) (y :double))

(define-c-function ("cairo_line_to" cairo-line-to) :void
  "Add a line to X Y to CAIRO's path."
  (cairo :pointer) (x :double) (y :double))

(define-c-function ("cairo_stroke" cairo-stroke) :void
  "Draw CAIRO's path as lines."
  (cairo :pointer))

(define-c-function ("cairo_set_line_width" cairo-set-line-width) :void
  "Draw lines on CAIRO WIDTH wide."
  (cairo :pointer) (width :double))

(define-c-function ("cairo_paint" cairo-paint) :void
  "Paint all of CAIRO in its colour."
  (cairo :pointer))

(define-c-function ("cairo_image_surface_create" cairo-image-surface-create) :pointer
  "A picture in memory, WIDTH by HEIGHT, its pixels in FORMAT (0: 32 bits a pixel)."
  (format :int) (width :int) (height :int))

(define-c-function ("cairo_create" cairo-create) :pointer
  "A new context drawing on SURFACE."
  (surface :pointer))

(define-c-function ("cairo_destroy" cairo-destroy) :void
  "Let go of the drawing context CAIRO."
  (cairo :pointer))

(define-c-function ("cairo_surface_destroy" cairo-surface-destroy) :void
  "Let go of SURFACE."
  (surface :pointer))

(define-c-function ("cairo_surface_set_device_scale" cairo-surface-set-device-scale) :void
  "Have one unit drawn on SURFACE take X-SCALE by Y-SCALE of its pixels: 2 on a
screen scaled to 200 per cent."
  (surface :pointer) (x-scale :double) (y-scale :double))

(define-c-function ("cairo_surface_flush" cairo-surface-flush) :void
  "Finish the drawing on SURFACE, so that its pixels can be read."
  (surface :pointer))

(define-c-function ("cairo_surface_write_to_png" cairo-surface-write-to-png) :int
  "Save SURFACE as a PNG file at PATH; 0 when it was saved."
  (surface :pointer) (path :string))

(define-c-function ("cairo_image_surface_get_data" cairo-image-surface-get-data) :pointer
  "The pixels of the image SURFACE."
  (surface :pointer))

(define-c-function ("cairo_image_surface_get_stride" cairo-image-surface-get-stride) :int
  "The octets a row of the image SURFACE takes."
  (surface :pointer))

(define-c-function ("cairo_scale" cairo-scale) :void
  "Scale what is drawn on CAIRO from now on by SX across and SY down."
  (cairo :pointer) (sx :double) (sy :double))

(define-c-function ("cairo_set_source_surface" cairo-set-source-surface) :void
  "Draw on CAIRO with the pixels of SURFACE, its top left at X Y."
  (cairo :pointer) (surface :pointer) (x :double) (y :double))

(define-c-function ("cairo_surface_mark_dirty" cairo-surface-mark-dirty) :void
  "Tell cairo that the pixels of SURFACE were changed directly."
  (surface :pointer))

(define-c-function ("gtk_widget_get_scale_factor" gtk-widget-get-scale-factor) :int
  "How many of the display's own pixels WIDGET's each pixel takes across: 2 on a screen scaled to 200 per cent."
  (widget :pointer))
