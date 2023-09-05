;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/renderer/gtk
    (:documentation "GTK renderer using direct CFFI bindings."))
(in-package :nyxt/renderer/gtk)

(push :nyxt-gtk *features*)

(define-class gtk-renderer (renderer)
  ((name "GTK"))
  (:export-class-name-p t)
  (:export-accessor-names-p t))

(define-class gtk-browser ()
  (#+darwin
   (modifiers '()
              :documentation "On macOS some modifiers like Super and Meta are
seen like regular keys.
To work around this issue, we store them in this list while they are pressed.
See `push-modifiers', `pop-modifiers' and `key-event-modifiers'.")
   (modifier-translator #'translate-modifiers
                        :documentation "Function that returns a list of
modifiers understood by `keymaps:make-key'.  You can customize this slot if you
want to change the behaviour of modifiers, for instance swap 'control' and
'meta':

\(defun my-translate-modifiers (modifier-state &optional event)
  \"Swap control and meta.\"
  (declare (ignore event))
  (let ((plist '(:control-mask \"meta\"
                 :mod1-mask \"control\" ;; Usually it is Alt.
                 :mod5-mask nil         ;; See your config for what mod1-5 mean.
                 :shift-mask \"shift\"
                 :super-mask \"super\"
                 :hyper-mask \"hyper\"
                 :meta-mask nil         ;; Meta.
                 :lock-mask nil)))
    (delete nil (mapcar (lambda (mod) (getf plist mod)) modifier-state))))

\(define-configuration browser
  ((modifier-translator #'my-translate-modifiers)))")
   (web-contexts (make-hash-table :test 'equal)
                 :export nil
                 :documentation "Persistent `nyxt::webkit-web-context's
Keyed by strings as they must be backed to unique folders
See also the `ephemeral-web-contexts' slot.")
   (ephemeral-web-contexts (make-hash-table :test 'equal)
                           :export nil
                           :documentation "Ephemeral `nyxt::webkit-web-context's.
See also the `web-contexts' slot."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class))

(alex:define-constant +internal+ "internal" :test 'equal)
(alex:define-constant +default+ "default" :test 'equal)

(defmethod get-context ((browser gtk-browser) name &key ephemeral-p)
  (alexandria:ensure-gethash name
                             (if ephemeral-p
                                 (ephemeral-web-contexts browser)
                                 (web-contexts browser))
                             (make-context name :ephemeral-p ephemeral-p)))

(define-class gtk-window ()
  ((gtk-object
    :export t)                          ; TODO: Unexport?
   (handler-ids
    :documentation "See `gtk-buffer' slot of the same name.")
   (root-box-layout)
   (horizontal-box-layout)
   (panel-buffer-container-left)
   (panel-buffer-container-right)
   (panel-buffers-left)
   (panel-buffers-right)
   (main-buffer-container)
   (prompt-buffer-container)
   (prompt-buffer-view
    :documentation "Shared web view between all prompt buffers of this window.")
   (status-container)
   (message-container)
   (key-string-buffer))
  (:export-class-name-p t)
  (:export-accessor-names-p nil))

(define-class gtk-buffer ()
  ((gtk-object)
   (context-name
    +default+
    :type string
    :documentation "Name of the WebKit context.
When given the same context, multiple buffers share their internal browsing data.
`+default+' is the default context.
`+internal+' is a context that's not persisted to disk.")
   (handler-ids
    :export nil
    :documentation "Store all GObject signal handler IDs so that we can disconnect the signal handler when the object is finalised.
See https://developer.gnome.org/gobject/stable/gobject-Signals.html#signal-memory-management.")
   (gtk-proxy-url (quri:uri ""))
   (proxy-ignored-hosts '())
   (loading-webkit-history-p nil
                             :type boolean
                             :export nil
                             :documentation "Internal hack, do not use me!
WebKitGTK may trigger 'load-failed' when loading a page from the WebKit-history
cache.  Upstream bug?  We use this slot to know when to ignore these load
failures."))
  (:export-class-name-p t)
  (:export-accessor-names-p t))

(defmethod prompter:object-attributes :around ((buffer gtk-buffer) (source nyxt:buffer-source))
  (declare (ignore source))
  (append (call-next-method)
          `(("Context" ,(context-name buffer)))))

(defclass webkit-web-context (webkit:webkit-web-context) ()
  (:metaclass gobject:gobject-class))

(defmethod initialize-instance :after ((web-context webkit-web-context) &key)
  #+webkit2-sandboxing
  (webkit:webkit-web-context-set-sandbox-enabled web-context t)
  web-context)

(defmethod data-directory ((web-context webkit-web-context))
  "Directly returns the CFFI object's `base-data-directory'"
  (webkit:webkit-website-data-manager-base-data-directory (webkit:webkit-web-context-website-data-manager web-context)))

(defmethod cache-directory ((web-context webkit-web-context))
  "Directly returns the CFFI object's `base-cache-directory'"
  (webkit:webkit-website-data-manager-base-cache-directory (webkit:webkit-web-context-website-data-manager web-context)))

(defclass webkit-web-context-ephemeral (webkit-web-context) ()
  (:metaclass gobject:gobject-class))

(defmethod initialize-instance :after ((web-context webkit-web-context-ephemeral) &key)
  (unless (webkit:webkit-web-context-is-ephemeral web-context)
    (error 'web-context-error :context web-context
                                       :message "Web Contexts of class webkit-web-context-ephemeral must be ephemeral")))

(defclass webkit-website-data-manager (webkit:webkit-website-data-manager) ()
  (:metaclass gobject:gobject-class))

(defclass webkit-website-data-manager-ephemeral (webkit-website-data-manager) ()
  (:metaclass gobject:gobject-class))

(defmethod initialize-instance :after ((data-manager webkit-website-data-manager-ephemeral) &key)
  (unless (webkit:webkit-website-data-manager-is-ephemeral data-manager)
    (error 'web-context-error :context data-manager
                                       :message "Data Managers of class webkit-website-data-manager-ephemeral must be ephemeral")))

(defvar gtk-running-p nil
  "Non-nil if the GTK main loop is running.
See `ffi-initialize' and `ffi-kill-browser'.

Restarting GTK within the same Lisp image breaks WebKitGTK.
As a workaround, we never leave the GTK main loop when running from a REPL.

See https://github.com/atlas-engineer/nyxt/issues/740")

(defmethod renderer-thread-p ((renderer gtk-renderer) &optional (thread (bt:current-thread)))
  #+darwin
  (string= "main thread" (bt:thread-name thread))
  #-darwin
  (string= "cl-cffi-gtk main thread" (bt:thread-name thread)))

(defmacro within-gtk-thread (&body body)
  "Protected `gtk:within-gtk-thread'."
  `(gtk:within-gtk-thread
     (with-protect ("Error on GTK thread: ~a" :condition)
       ,@body)))

(defmethod ffi-within-renderer-thread ((browser gtk-browser) thunk)
  (declare (ignore browser))
  (within-gtk-thread
    (funcall thunk)))

(defun %within-renderer-thread (thunk)
  "If the current thread is the renderer thread, execute THUNK with `funcall'.
Otherwise run the THUNK on the renderer thread by passing it a channel and wait on the channel's result."
  (if (renderer-thread-p nyxt::*renderer*)
      (funcall thunk)
      (let ((channel (nyxt::make-channel 1)))
        (within-gtk-thread
          (funcall thunk channel))
        (calispel:? channel))))

(defun %within-renderer-thread-async (thunk)
  "Same as `%within-renderer-thread' but THUNK is not blocking and does
not return."
  (if (renderer-thread-p nyxt::*renderer*)
      (funcall thunk)
      (within-gtk-thread
        (funcall thunk))))

(export-always 'define-ffi-method)
(defmacro define-ffi-method (name args &body body)
  "Define an FFI method to run in the renderer thread.

Return the value or forward the condition retrieved from the renderer thread,
using a channel if the current thread is not the renderer one.

It's a `defmethod' wrapper. If you don't need the body of the method to execute in
the renderer thread, use `defmethod' instead."
  (multiple-value-bind (forms declares docstring)
      (alex:parse-body body :documentation t)
    `(defmethod ,name ,args
       ,@(sera:unsplice docstring)
       ,@declares
       (if (renderer-thread-p nyxt::*renderer*)
           (progn ,@forms)
           (let ((channel (nyxt::make-channel 1))
                 (error-channel (nyxt::make-channel 1)))
             (within-gtk-thread
               ;; We do not include `*debug-on-error*' for now, since we need to
               ;; first improve the debugger to properly handle the GTK thread.
               ;; TODO: Abstract this into `with-protect-from-thread'?
               (if (or nyxt::*run-from-repl-p* nyxt::*restart-on-error*)
                   (let ((current-condition nil))
                     (restart-case
                         (handler-bind ((condition (lambda (c) (setf current-condition c))))
                           (calispel:! channel (progn ,@forms)))
                       (abort-ffi-method ()
                         :report "Pass condition to calling thread."
                         (calispel:! error-channel current-condition))))
                   (handler-case (calispel:! channel (progn ,@forms))
                     (condition (c)
                       (calispel:! error-channel c)))))
             (calispel:fair-alt
               ((calispel:? channel result)
                result)
               ((calispel:? error-channel condition)
                (with-protect ("Error in FFI method: ~a" :condition)
                  (error condition)))))))))

(defmethod ffi-initialize ((browser gtk-browser) urls startup-timestamp)
  "gtk:within-main-loop handles all the GTK initialization. On
   GNU/Linux, Nyxt could hang after 10 minutes if it's not
   used. Conversely, on Darwin, if gtk:within-main-loop is used, no
   drawing happens. Drawing operations on Darwin MUST originate from
   the main thread, which the GTK main loop is not guaranteed to be
   on."
  (declare (ignore urls startup-timestamp))
  (log:debug "Initializing GTK Interface")
  (if gtk-running-p
      (within-gtk-thread
        (call-next-method))
      #-darwin
      (progn
        (setf gtk-running-p t)
        (glib:g-set-prgname "nyxt")
        (gdk:gdk-set-program-class "Nyxt")
        (gtk:within-main-loop
          (with-protect ("Error on GTK thread: ~a" :condition)
            (call-next-method)))
        (unless nyxt::*run-from-repl-p*
          (gtk:join-gtk-main)
          ;; KLUDGE: On FreeBSD nyxt is stuck inside some blocking
          ;; syscall if FINISH-OUTPUT is T. In this case the
          ;; "force-quitter" thread created by QUIT command will not
          ;; help and nyxt can only be killed with SIGKILL,
          ;; e.g. executing "killall -9 nyxt". A workaround is to call
          ;; UIOP:QUIT with FINISH-OUTPUT = NIL in the first place.
          ;;
          ;; FIXME: This also can be true for other BSD systems.
          (uiop:quit (slot-value browser 'nyxt::exit-code) #+freebsd nil)))
      #+darwin
      (progn
        (setf gtk-running-p t)
        (glib:g-set-prgname "nyxt")
        (gdk:gdk-set-program-class "Nyxt")
        (call-next-method)
        (gtk:gtk-main))))

(define-ffi-method ffi-kill-browser ((browser gtk-browser))
  ;; TODO: Terminating the GTK thread from the REPL seems to prevent Nyxt from
  ;; starting again.
  (unless nyxt::*run-from-repl-p*
    (gtk:leave-gtk-main)))

(define-class data-manager-file (nyxt-file)
  ((context-name (error "Context name required."))
   (files:name "web-context"))
  (:export-class-name-p t))

(defmethod files:resolve :around ((profile nosave-profile) (file data-manager-file))
  "We shouldn't store any `data-manager' data for `nosave-profile'."
  #p"")

(define-class data-manager-data-directory (files:data-file data-manager-file)
  ()
  (:export-class-name-p t))

(defmethod files:resolve ((profile nyxt-profile) (file data-manager-data-directory))
  (sera:path-join
   (call-next-method)
   (pathname (str:concat (context-name file) "-web-context/"))))

(define-class data-manager-cache-directory (files:cache-file data-manager-file)
  ()
  (:export-class-name-p t))

(defmethod files:resolve ((profile nyxt-profile) (file data-manager-cache-directory))
  (sera:path-join
   (call-next-method)
   (pathname (str:concat (context-name file) "-web-context/"))))

(define-class gtk-extensions-directory (nyxt-file)
  ((files:name "gtk-extensions")
   (files:base-path nyxt-asdf:*nyxt-libdir*))
  (:export-class-name-p t)
  (:documentation "Directory where to load the 'libnyxt' library.
By default it is found in the source directory."))

(defmethod files:resolve ((profile nyxt-profile) (file gtk-extensions-directory))
  (let ((system-directory (call-next-method)))
    (if (uiop:directory-exists-p system-directory)
        system-directory
        (files:join (files:expand *source-directory*) "/libraries/web-extensions/"))))

(define-class cookies-file (files:data-file data-manager-file)
  ((files:name "cookies"))
  (:export-class-name-p t))

(defmethod files:resolve ((profile nyxt-profile) (file cookies-file))
  (sera:path-join
   (call-next-method)
   (pathname (str:concat (context-name file) "-cookies"))))

(define-class gtk-download ()
  ((gtk-object)
   (handler-ids
    :export nil
    :documentation "See `gtk-buffer' slot of the same name.")))

(defclass webkit-web-view-ephemeral (webkit:webkit-web-view) ()
  (:metaclass gobject:gobject-class))

(defmethod initialize-instance :after ((web-view webkit-web-view-ephemeral) &key web-context)
  (unless (webkit:webkit-web-view-is-ephemeral web-view)
    (error 'web-context-error :context web-context
                                       :message "Tried to make an ephemeral web-view in a non-ephemeral context")))

(defmethod make-web-view ((profile nyxt-profile) (buffer t))
  "Return an ephemeral web view instance for basic buffers."
  (declare (ignorable profile buffer))
  (make-instance 'webkit-web-view-ephemeral
                 :web-context (get-context *browser* +internal+
                                           :ephemeral-p t)))

(defmethod make-web-view ((profile nyxt-profile) (buffer context-buffer))
  "Return a regular web view instance for buffers with context."
  (declare (ignorable profile))
  (make-instance 'webkit:webkit-web-view
                 :web-context (get-context *browser* (context-name buffer)
                                           :ephemeral-p nil)))

(defmethod make-web-view ((profile nyxt-profile) (buffer nosave-buffer))
  "Return an ephemeral web view instance for nosave buffers."
  (declare (ignorable profile))
  (make-instance 'webkit-web-view-ephemeral
                 :web-context (get-context *browser* (context-name buffer)
                                           :ephemeral-p t)))

(defmethod make-web-view ((profile nosave-profile) (buffer buffer))
  "Return an ephemeral web view instance for nosave profiles."
  (declare (ignorable profile))
  (make-instance 'webkit-web-view-ephemeral
                 :web-context (get-context *browser* (context-name buffer)
                                           :ephemeral-p t)))

(define-class gtk-request-data ()
  ((gtk-request
    nil
    :type (maybe webkit:webkit-uri-request))
   (gtk-response
    nil
    :type (maybe webkit:webkit-uri-response))
   (gtk-resource
    nil
    :type (maybe webkit:webkit-web-resource)))
  (:export-class-name-p t)
  ;; We export these accessors because it can be useful to inspect the guts of a
  ;; request, plus the upstream WebKit API is stable enough.
  (:export-accessor-names-p t)
  (:metaclass user-class))

(defun make-decide-policy-handler (buffer)
  (lambda (web-view response-policy-decision policy-decision-type-response)
    (declare (ignore web-view))
    ;; Even if errors are caught with `with-protect', we must ignore the policy
    ;; decision on error, lest we load a web page in an internal buffer for
    ;; instance.
    (g:g-object-ref (g:pointer response-policy-decision))
    (run-thread "asynchronous decide-policy processing"
      (handler-bind ((error (lambda (c)
                              (echo-warning "decide policy error: ~a" c)
                              ;; TODO: Don't automatically call the restart when from the REPL?
                              ;; (unless nyxt::*run-from-repl-p*
                              ;;   (invoke-restart 'ignore-policy-decision))
                              (invoke-restart 'ignore-policy-decision))))
        (restart-case (on-signal-decide-policy buffer response-policy-decision policy-decision-type-response)
          (ignore-policy-decision ()
            (webkit:webkit-policy-decision-ignore response-policy-decision)))))
    t))


(defmacro connect-signal-function (object signal fn)
  "Connect SIGNAL to OBJECT with a function FN.
OBJECT must have the `gtk-object' and `handler-ids' slots.
See also `connect-signal'."
  `(let ((handler-id (gobject:g-signal-connect
                      (gtk-object ,object) ,signal ,fn)))
     (push handler-id (handler-ids ,object))))

(defmacro connect-signal (object signal new-thread-p (&rest args) &body body)
  "Connect SIGNAL to OBJECT with a lambda that takes ARGS.
OBJECT must have the `gtk-object' and `handler-ids' slots. If
`new-thread-p' is non-nil, then a new thread will be launched for the
response.  The BODY is wrapped with `with-protect'."
  (multiple-value-bind (forms declares documentation)
      (alex:parse-body body :documentation t)
    `(let ((handler-id (gobject:g-signal-connect
                        (gtk-object ,object) ,signal
                        (lambda (,@args)
                          ,@(sera:unsplice documentation)
                          ,@declares
                          ,(if new-thread-p
                               `(run-thread "renderer signal handler"
                                    ,@forms)
                               `(with-protect ("Error in signal on renderer thread: ~a" :condition)
                                  ,@forms))))))
       (push handler-id (handler-ids ,object)))))

(defmethod customize-instance :after ((window gtk-window) &key)
  (%within-renderer-thread-async
   (lambda ()
     (with-slots (gtk-object root-box-layout horizontal-box-layout
                  panel-buffer-container-left
                  panel-buffer-container-right
                  main-buffer-container
                  nyxt::active-buffer prompt-buffer-container
                  prompt-buffer-view
                  status-buffer status-container
                  message-buffer message-container
                  key-string-buffer)
         window
       (unless gtk-object
         (setf gtk-object (make-instance 'gtk:gtk-window
                                         :type :toplevel
                                         :default-width 1024
                                         :default-height 768))
         (setf root-box-layout (make-instance 'gtk:gtk-box
                                              :orientation :vertical
                                              :spacing 0))
         (setf horizontal-box-layout (make-instance 'gtk:gtk-box
                                                    :orientation :horizontal
                                                    :spacing 0))
         (setf panel-buffer-container-left (make-instance 'gtk:gtk-box
                                                          :orientation :horizontal
                                                          :spacing 0))
         (setf panel-buffer-container-right (make-instance 'gtk:gtk-box
                                                           :orientation :horizontal
                                                           :spacing 0))
         (setf main-buffer-container (make-instance 'gtk:gtk-box
                                                    :orientation :vertical
                                                    :spacing 0))
         (setf prompt-buffer-container (make-instance 'gtk:gtk-box
                                                      :orientation :vertical
                                                      :spacing 0))
         (setf message-container (make-instance 'gtk:gtk-box
                                                :orientation :vertical
                                                :spacing 0))
         (setf status-container (make-instance 'gtk:gtk-box
                                               :orientation :vertical
                                               :spacing 0))
         (setf key-string-buffer (make-instance 'gtk:gtk-entry))
         ;; Dummy buffer is an `input-buffer' so that an empty window can still
         ;; receive input, for instance to create a new buffer.
         (setf nyxt::active-buffer (make-instance 'input-buffer))

         (when (eq (status-buffer-position window) :top)
           (gtk:gtk-box-pack-start root-box-layout status-container :expand nil))

         ;; Add the views to the box layout and to the window
         (gtk:gtk-box-pack-start main-buffer-container (gtk-object nyxt::active-buffer) :expand t :fill t)
         (gtk:gtk-box-pack-start horizontal-box-layout panel-buffer-container-left :expand nil)
         (gtk:gtk-box-pack-start horizontal-box-layout main-buffer-container :expand t :fill t)
         (gtk:gtk-box-pack-start horizontal-box-layout panel-buffer-container-right :expand nil)
         (gtk:gtk-box-pack-start root-box-layout horizontal-box-layout :expand t :fill t)

         (gtk:gtk-box-pack-end root-box-layout message-container :expand nil)
         (gtk:gtk-box-pack-start root-box-layout message-container :expand nil)
         (gtk:gtk-box-pack-start message-container (gtk-object message-buffer) :expand t)
         (setf (gtk:gtk-widget-size-request message-container)
               (list -1 (height message-buffer)))

         (when (eq (status-buffer-position window) :bottom)
           (gtk:gtk-box-pack-end root-box-layout status-container :expand nil))

         (gtk:gtk-box-pack-start status-container (gtk-object status-buffer) :expand t)
         (setf (gtk:gtk-widget-size-request status-container)
               (list -1 (height status-buffer)))

         (setf prompt-buffer-view (make-web-view (global-profile) nil))
         (gtk:gtk-box-pack-end root-box-layout prompt-buffer-container :expand nil)
         (gtk:gtk-box-pack-start prompt-buffer-container prompt-buffer-view :expand t)
         (setf (gtk:gtk-widget-size-request prompt-buffer-container)
               (list -1 0))

         (gtk:gtk-container-add gtk-object root-box-layout)

         (connect-signal window "key_press_event" nil (widget event)
           (declare (ignore widget))
           #+darwin
           (push-modifier *browser* event)
           (on-signal-key-press-event window event))
         (connect-signal window "key_release_event" nil (widget event)
           (declare (ignore widget))
           #+darwin
           (pop-modifier *browser* event)
           (on-signal-key-release-event window event))
         (connect-signal window "destroy" nil (widget)
           (declare (ignore widget))
           (on-signal-destroy window))
         (connect-signal window "window-state-event" nil (widget event)
           (declare (ignore widget))
           (setf (nyxt::fullscreen-p window)
                 (find :fullscreen
                       (gdk:gdk-event-window-state-new-window-state event)))
           (setf (nyxt::maximized-p window)
                 (find :maximized
                       (gdk:gdk-event-window-state-new-window-state event)))
           nil))

       (unless nyxt::*headless-p*
         (gtk:gtk-widget-show-all gtk-object))))))

(defmethod update-instance-for-redefined-class :after ((window window) added deleted plist &key)
  (declare (ignore added deleted plist))
  (customize-instance window))

(define-ffi-method on-signal-destroy ((window gtk-window))
  ;; Then remove buffer from window container to avoid corruption of buffer.
  (gtk:gtk-container-remove (main-buffer-container window) (gtk-object (nyxt::active-buffer window)))
  (window-delete window))

(define-ffi-method ffi-window-delete ((window gtk-window))
  (gtk:gtk-widget-destroy (gtk-object window)))

(define-ffi-method ffi-window-fullscreen ((window gtk-window))
  (gtk:gtk-window-fullscreen (gtk-object window)))

(define-ffi-method ffi-window-unfullscreen ((window gtk-window))
  (gtk:gtk-window-unfullscreen (gtk-object window)))

(define-ffi-method ffi-window-maximize ((window gtk-window))
  (gtk:gtk-window-maximize (gtk-object window)))

(define-ffi-method ffi-window-unmaximize ((window gtk-window))
  (gtk:gtk-window-unmaximize (gtk-object window)))

(defun derive-key-string (keyval character)
  "Return string representation of a keyval.
Return nil when key must be discarded, e.g. for modifiers."
  (let ((result
          (match keyval
            ((or "Alt_L" "Super_L" "Control_L" "Shift_L"
                 "Alt_R" "Super_R" "Control_R" "Shift_R"
                 "ISO_Level3_Shift" "Arabic_switch")
             ;; Discard modifiers (they usually have a null character).
             nil)
            ((guard s (str:contains? "KP_" s))
             (str:replace-all "KP_" "keypad" s))
            ;; With a modifier, "-" does not print, so we me must translate it
            ;; to "hyphen" just like in `printable-p'.
            ("minus" "hyphen")
            ;; shift-tab:
            ("ISO_Left_Tab" "tab")
            ;; In most cases, return character and not keyval for punctuation.
            ;; For instance, C-[ is not printable but the keyval is "bracketleft".
            ;; ASCII control characters like Escape, Delete or BackSpace have a
            ;; non-printable character (usually beneath #\space), so we use the
            ;; keyval in this case.
            ;; Even if space in printable, C-space is not so we return the
            ;; keyval in this case.
            (_ (if (or (char<= character #\space)
                       (char= character #\Del))
                   keyval
                   (string character))))))
    (if (< 1 (length result))
        (str:replace-all "_" "" (string-downcase result))
        result)))

#+darwin
(defmethod push-modifier ((browser gtk-browser) event)
  (let* ((modifier-state (gdk:gdk-event-key-state event))
         (key-value (gdk:gdk-event-key-keyval event))
         (key-value-name (gdk:gdk-keyval-name key-value)))
    (when (member :control-mask modifier-state)
      (push :control-mask (modifiers browser)))
    (when (member :shift-mask modifier-state)
      (push :shift-mask (modifiers browser)))
    (when (or (string= key-value-name "Arabic_switch")
              (string= key-value-name "Alt_L")
              (string= key-value-name "Alt_R"))
      (push :mod1-mask (modifiers browser)))
    (when (and (member :mod2-mask modifier-state)
               (member :meta-mask modifier-state))
      (push :super-mask (modifiers browser))))
  (setf (modifiers browser) (delete-duplicates (modifiers browser))))

#+darwin
(defmethod pop-modifier ((browser gtk-browser) event)
  (let* ((modifier-state (gdk:gdk-event-key-state event))
         (key-value (gdk:gdk-event-key-keyval event))
         (key-value-name (gdk:gdk-keyval-name key-value)))
    (when (member :control-mask modifier-state)
      (alex:deletef (modifiers browser) :control-mask))
    (when (member :shift-mask modifier-state)
      (alex:deletef (modifiers browser) :shift-mask))
    (when (or (string= key-value-name "Arabic_switch")
              (string= key-value-name "Alt_L")
              (string= key-value-name "Alt_R"))
      (alex:deletef (modifiers browser) :mod1-mask))
    (when (and (member :mod2-mask modifier-state)
               (member :meta-mask modifier-state))
      (alex:deletef (modifiers browser) :super-mask))))

(-> translate-modifiers (list &optional gdk:gdk-event) list)
(defun translate-modifiers (modifier-state &optional event)
  "Return list of modifiers fit for `keymaps:make-key'.
See `gtk-browser's `modifier-translator' slot."
  (declare (ignore event))
  (let ((plist '(:control-mask "control"
                 :mod1-mask "meta"
                 :mod5-mask nil
                 :shift-mask "shift"
                 :super-mask "super"
                 :hyper-mask "hyper"
                 :meta-mask nil
                 :lock-mask nil)))
    (delete nil (mapcar (lambda (mod) (getf plist mod)) modifier-state))))

#+darwin
(defun key-event-modifiers (key-event)
  (declare (ignore key-event))
  (modifiers *browser*))

#-darwin
(defun key-event-modifiers (key-event)
  (gdk:gdk-event-key-state key-event))

;; REVIEW: Remove after upstream fix is merged in Quicklisp, see https://github.com/crategus/cl-cffi-gtk/issues/74.
;; Wait for https://github.com/Ferada/cl-cffi-gtk/issues/new.
(defun gdk-event-button-state (button-event)
  "Return BUTTON-EVENT modifiers as a `gdk-modifier-type', i.e. a list of keywords."
  (let ((state (gdk:gdk-event-button-state button-event)))
    (if (listp state)
        state
        (cffi:with-foreign-objects ((modifiers 'gdk:gdk-modifier-type))
          (setf (cffi:mem-ref modifiers 'gdk:gdk-modifier-type) state)
          (cffi:mem-ref modifiers 'gdk:gdk-modifier-type)))))

#+darwin
(defun button-event-modifiers (button-event)
  (declare (ignore button-event))
  (modifiers *browser*))

#-darwin
(defun button-event-modifiers (button-event)
  (gdk-event-button-state button-event))

#+darwin
(defun scroll-event-modifiers (scroll-event)
  (declare (ignore scroll-event))
  (modifiers *browser*))

#-darwin
(defun scroll-event-modifiers (scroll-event)
  (gdk:gdk-event-scroll-state scroll-event))

(defmethod printable-p ((window gtk-window) event)
  "Return the printable value of EVENT."
  ;; Generate the result of the current keypress into the dummy
  ;; key-string-buffer (a GtkEntry that's never shown on screen) so that we
  ;; can collect the printed representation of composed keypress, such as dead
  ;; keys.
  (gtk:gtk-entry-im-context-filter-keypress (key-string-buffer window) event)
  (when (<= 1 (gtk:gtk-entry-text-length (key-string-buffer window)))
    (prog1
        (match (gtk:gtk-entry-text (key-string-buffer window))
          ;; Special cases: these characters are not supported as is for keyspecs.
          ;; See `self-insert' for the reverse translation.
          (" " "space")
          ("-" "hyphen")
          (character character))
      (setf (gtk:gtk-entry-text (key-string-buffer window)) ""))))

(defun update-prompt (buffer)
  (run-thread "Prompt updater"
    ;; Rebind prompt-buffer to ensure the watcher does not mix up the
    ;; different prompt-buffers.
    (let ((prompt-buffer buffer))
      (nyxt::update-prompt-input prompt-buffer))))

(define-ffi-method on-signal-key-press-event ((sender gtk-window) event)
  (let* ((keycode (gdk:gdk-event-key-hardware-keycode event))
         (keyval (gdk:gdk-event-key-keyval event))
         (keyval-name (gdk:gdk-keyval-name keyval))
         (character (gdk:gdk-keyval-to-unicode keyval))
         (printable-value (printable-p sender event))
         (key-string (or printable-value
                         (derive-key-string keyval-name character)))
         (modifiers (funcall (modifier-translator *browser*)
                             (key-event-modifiers event)
                             event))
         (buffer (or (current-prompt-buffer)
                     (nyxt::active-buffer sender))))
    (if modifiers
        (log:debug key-string keycode character keyval-name modifiers)
        (log:debug key-string keycode character keyval-name))
    (when (nyxt::prompt-buffer-p buffer)
      (update-prompt buffer))
    (if key-string
        (flet ((key ()
                 (keymaps:make-key :code keycode
                                   :value key-string
                                   :modifiers modifiers
                                   :status :pressed)))
          (alex:appendf (key-stack sender)
                        (list (key)))
          (run-thread "on-signal-key-press"
            (on-signal-key-press buffer (key)))
          (funcall (input-dispatcher sender) event
                   buffer
                   sender printable-value))
        ;; Do not forward modifier-only to renderer.
        t)))

(defmethod gtk-object ((prompt-buffer prompt-buffer))
  (prompt-buffer-view (window prompt-buffer)))

(define-ffi-method on-signal-key-release-event ((sender gtk-window) event)
  (declare (ignore sender event))
  ;; REVIEW: Is there any use for handling release events?
  nil)

(defun sender-window (sender)
  (or (find sender (window-list) :key #'nyxt::active-prompt-buffers)
      (find sender (window-list) :key #'nyxt::active-buffer)
      (current-window)))

(define-ffi-method on-signal-button-press-event ((sender gtk-buffer) event)
  (let* ((button (gdk:gdk-event-button-button event))
         ;; REVIEW: No need to store X and Y?
         ;; (x (gdk:gdk-event-button-x event))
         ;; (y (gdk:gdk-event-button-y event))
         (window (sender-window sender))
         (key-string (format nil "button~s" button))
         (modifiers (funcall (modifier-translator *browser*)
                             (button-event-modifiers event)
                             event))
         (buffer (or (current-prompt-buffer)
                     sender)))
    ;; Handle mode-specific logic here (e.g. VI switch to insertion) to not
    ;; interfere with regular keybinding logic.
    (flet ((key ()
             ;; It's a function so we instantiate multiple objects and avoid sharing.
             (keymaps:make-key :value key-string
                               :modifiers modifiers
                               :status :pressed)))
      (run-thread "on-signal-button-press"
        (on-signal-button-press buffer (key)))
      (when (prompt-buffer-p buffer)
        (update-prompt buffer))
      (when key-string
        (alex:appendf (key-stack window)
                      (list (key)))
        (funcall (input-dispatcher window) event sender window nil)))))

(define-ffi-method on-signal-scroll-event ((sender gtk-buffer) event)
  (let* ((button (match (gdk:gdk-event-scroll-direction event)
                   (:up 4)
                   (:down 5)
                   (:left 6)
                   (:right 7)
                   (:smooth
                    (cond
                      ((>= 0 (gdk:gdk-event-scroll-delta-y event))
                       4)
                      ((< 0 (gdk:gdk-event-scroll-delta-y event))
                       5)
                      ((>= 0 (gdk:gdk-event-scroll-delta-x event))
                       6)
                      ((< 0 (gdk:gdk-event-scroll-delta-x event))
                       7)))))
         (window (sender-window sender))
         (key-string (format nil "button~s" button))
         (modifiers (funcall (modifier-translator *browser*)
                             (scroll-event-modifiers event)
                             event)))
    (when key-string
      (alex:appendf (key-stack window)
                    (list (keymaps:make-key :value key-string
                                            :modifiers modifiers
                                            :status :pressed)))
      (funcall (input-dispatcher window) event sender window nil))))

(define-class gtk-scheme ()
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t))

;; From https://github.com/umpirsky/language-list/tree/master/data directory
;; listing $(ls /data) and processed with:
;; (defun iso-languages (languages-file-string)
;;   (remove-if (lambda (lang)
;;                (or (uiop:emptyp lang)
;;                    (/= 1 (count #\_ lang))
;;                    (notevery #'upper-case-p (subseq lang 3))))
;;              (uiop:split-string languages-file-string :separator '(#\Space #\Tab #\Newline))))
(defvar *spell-check-languages*
  (list "be_BY" "en_LC" "en_VG" "ff_MR" "fr_SC" "ln_CF" "nl_BE" "rm_CH" "uk_UA"
        "af_NA" "el_CY" "en_LR" "en_VI" "ff_SN" "fr_SN" "ii_CN" "ln_CG" "nl_BQ"
        "af_ZA" "bg_BG" "el_GR" "en_LS" "en_VU" "fr_SY" "nl_CW" "rn_BI" "ur_IN"
        "en_MG" "en_WS" "fi_FI" "fr_TD" "is_IS" "lo_LA" "nl_NL" "ur_PK" "ak_GH"
        "en_AG" "en_MH" "en_ZA" "fr_TG" "nl_SR" "ro_MD" "en_AI" "en_MO" "en_ZM"
        "fo_FO" "fr_TN" "it_CH" "lt_LT" "nl_SX" "ro_RO" "uz_AF" "am_ET" "en_AS"
        "en_MP" "en_ZW" "fr_VU" "it_IT" "bn_BD" "en_AU" "en_MS" "fr_BE" "fr_WF"
        "it_SM" "lu_CD" "nn_NO" "ru_BY" "ar_AE" "bn_IN" "en_BB" "en_MT" "fr_BF"
        "fr_YT" "ru_KG" "sr_ME" "ar_BH" "en_BE" "en_MU" "es_AR" "fr_BI" "ja_JP"
        "lv_LV" "no_NO" "ru_KZ" "sr_RS" "ar_DJ" "bo_CN" "en_BM" "en_MW" "es_BO"
        "fr_BJ" "fy_NL" "ru_MD" "sr_XK" "ar_DZ" "bo_IN" "en_BS" "en_MY" "es_CL"
        "fr_BL" "ka_GE" "mg_MG" "om_ET" "ru_RU" "ar_EG" "en_BW" "en_NA" "es_CO"
        "fr_CA" "ga_IE" "om_KE" "ru_UA" "sv_AX" "uz_UZ" "ar_EH" "br_FR" "en_BZ"
        "en_NF" "es_CR" "fr_CD" "ki_KE" "mk_MK" "sv_FI" "ar_ER" "en_CA" "en_NG"
        "es_CU" "fr_CF" "gd_GB" "or_IN" "rw_RW" "sv_SE" "vi_VN" "ar_IL" "bs_BA"
        "en_CC" "en_NR" "es_DO" "fr_CG" "ml_IN" "ar_IQ" "en_CK" "en_NU" "es_EA"
        "fr_CH" "gl_ES" "os_GE" "se_FI" "sw_CD" "ar_JO" "en_CM" "en_NZ" "es_EC"
        "fr_CI" "kk_KZ" "os_RU" "se_NO" "sw_KE" "yo_BJ" "ar_KM" "en_CX" "en_PG"
        "es_ES" "fr_CM" "gu_IN" "se_SE" "sw_TZ" "yo_NG" "ar_KW" "en_DG" "en_PH"
        "es_GQ" "fr_DJ" "kl_GL" "mn_MN" "sw_UG" "ar_LB" "en_DM" "en_PK" "es_GT"
        "fr_DZ" "gv_IM" "sg_CF" "zh_CN" "ar_LY" "ca_AD" "en_ER" "en_PN" "es_HN"
        "fr_FR" "km_KH" "mr_IN" "ta_IN" "zh_HK" "ar_MA" "ca_ES" "en_FJ" "en_PR"
        "es_IC" "fr_GA" "ha_GH" "sh_BA" "ta_LK" "ar_MR" "ca_FR" "en_FK" "en_PW"
        "es_MX" "fr_GF" "kn_IN" "ms_BN" "pa_IN" "ta_MY" "ar_OM" "ca_IT" "en_FM"
        "en_RW" "es_NI" "fr_GN" "pa_PK" "si_LK" "ta_SG" "ar_PS" "en_GB" "en_SB"
        "es_PA" "fr_GP" "ko_KP" "ar_QA" "cs_CZ" "en_GD" "en_SC" "es_PE" "fr_GQ"
        "ko_KR" "pl_PL" "sk_SK" "te_IN" "ar_SA" "en_GG" "en_SD" "es_PH" "fr_HT"
        "ha_NE" "ar_SD" "cy_GB" "en_GH" "en_SG" "es_PR" "fr_KM" "ha_NG" "ms_MY"
        "ps_AF" "sl_SI" "th_TH" "ar_SO" "en_GI" "en_SH" "es_PY" "fr_LU" "ms_SG"
        "ar_SS" "da_DK" "en_GM" "en_SL" "es_SV" "fr_MA" "he_IL" "ks_IN" "pt_AO"
        "sn_ZW" "ti_ER" "ar_SY" "da_GL" "en_GU" "en_SS" "es_US" "fr_MC" "mt_MT"
        "pt_BR" "ti_ET" "zh_MO" "ar_TD" "en_GY" "en_SX" "es_UY" "fr_MF" "hi_IN"
        "kw_GB" "pt_CV" "so_DJ" "zh_SG" "ar_TN" "de_AT" "en_HK" "en_SZ" "es_VE"
        "fr_MG" "my_MM" "pt_GW" "so_ET" "tl_PH" "zh_TW" "ar_YE" "de_BE" "en_IE"
        "en_TC" "fr_ML" "hr_BA" "pt_MO" "so_KE" "de_CH" "en_IM" "en_TK" "et_EE"
        "fr_MQ" "hr_HR" "nb_NO" "pt_MZ" "so_SO" "to_TO" "zu_ZA" "as_IN" "de_DE"
        "en_IN" "en_TO" "fr_MR" "ky_KG" "nb_SJ" "pt_PT" "de_LI" "en_IO" "en_TT"
        "eu_ES" "fr_MU" "hu_HU" "pt_ST" "sq_AL" "tr_CY" "az_AZ" "de_LU" "en_JE"
        "en_TV" "fr_NC" "lb_LU" "nd_ZW" "pt_TL" "sq_MK" "tr_TR" "en_JM" "en_TZ"
        "fa_AF" "fr_NE" "hy_AM" "sq_XK" "dz_BT" "en_KE" "en_UG" "fa_IR" "fr_PF"
        "lg_UG" "ne_IN" "qu_BO" "en_KI" "en_UM" "fr_PM" "id_ID" "ne_NP" "qu_EC"
        "sr_BA" "ee_GH" "en_KN" "en_US" "ff_CM" "fr_RE" "ln_AO" "qu_PE" "ug_CN"
        "ee_TG" "en_KY" "en_VC" "ff_GN" "fr_RW" "ig_NG" "ln_CD" "nl_AW")
  "The list of languages available for spell checking in `set-spell-check-languages'.")

(define-command-global set-spell-check-languages
    (&key (buffer (current-buffer))
     (languages (prompt :prompt "Languages to spell check"
                        :sources (make-instance 'prompter:source
                                                :name "Language codes"
                                                :enable-marks-p t
                                                :constructor *spell-check-languages*))))
  (let ((pointer (cffi:foreign-alloc :string
                                     :initial-contents languages
                                     :null-terminated-p t)))
    (webkit:webkit-web-context-set-spell-checking-languages
     (webkit:webkit-web-view-web-context (gtk-object buffer))
     pointer)
    (cffi:foreign-free pointer)))

(defun make-context (name &key ephemeral-p)
  (let* ((context
           (if ephemeral-p
               ;; An ephemeral data-manager cannot be given any directories, even if they are set to nil.
               (make-instance 'webkit-web-context-ephemeral
                              :website-data-manager
                              (make-instance 'webkit-website-data-manager-ephemeral
                                             :is-ephemeral t))
               (let ((data-manager-data-directory (make-instance 'data-manager-data-directory :context-name name))
                     (data-manager-cache-directory (make-instance 'data-manager-cache-directory :context-name name)))
                 (make-instance 'webkit-web-context
                                :website-data-manager
                                (make-instance
                                 'webkit-website-data-manager
                                 :base-data-directory (or (sera:and-let* ((path (files:expand data-manager-data-directory))
                                                                          (_ (not (nfiles:nil-pathname-p path))))
                                                            (uiop:native-namestring path))
                                                          (cffi:null-pointer))
                                 :base-cache-directory (or (sera:and-let* ((path (files:expand data-manager-cache-directory))
                                                                           (_ (not (nfiles:nil-pathname-p path))))
                                                             (uiop:native-namestring path))
                                                           (cffi:null-pointer)))))))
         (gtk-extensions-path (files:expand (make-instance 'gtk-extensions-directory)))
         (cookie-manager (webkit:webkit-web-context-get-cookie-manager context)))
    (webkit:webkit-web-context-set-spell-checking-enabled context t)
    ;; Need to set the initial language list.
    (let ((pointer (cffi:foreign-alloc :string
                                       :initial-contents (list (or (uiop:getenv "LANG")
                                                                   (uiop:getenv "LANGUAGE")
                                                                   (uiop:getenv "LC_CTYPE")
                                                                   "en_US"))
                                       :null-terminated-p t)))
      (webkit:webkit-web-context-set-spell-checking-languages context pointer)
      (cffi:foreign-free pointer))
    (when (and (not (nfiles:nil-pathname-p gtk-extensions-path))
               ;; Either the directory exists.
               (or (uiop:directory-exists-p gtk-extensions-path)
                   ;; Or try to create it.
                   (handler-case
                       (nth-value 1 (ensure-directories-exist gtk-extensions-path))
                     (file-error ()))))
      (log:info "GTK extensions directory: ~s" gtk-extensions-path)
      ;; TODO: Should we also use `connect-signal' here?  Does this yield a memory leak?
      (gobject:g-signal-connect
       context "initialize-web-extensions"
       (lambda (context)
         (with-protect ("Error in \"initialize-web-extensions\" signal thread: ~a" :condition)
           ;; The following calls
           ;; `webkit:webkit-web-context-add-path-to-sandbox' for us, so no need
           ;; to add `gtk-extensions-path' to the sandbox manually.
           (webkit:webkit-web-context-set-web-extensions-directory
            context
            (uiop:native-namestring gtk-extensions-path))))))
    (gobject:g-signal-connect
     context "download-started"
     (lambda (context download)
       (declare (ignore context))
       (with-protect ("Error in \"download-started\" signal thread: ~a" :condition)
         (wrap-download download))))
    (maphash
     (lambda (scheme scheme-object)
       (webkit:webkit-web-context-register-uri-scheme-callback
        context scheme
        (lambda (request)
          (let ((nyxt::*interactive-p* t)
                ;; Look up the scheme-object again so that we can live-update
                ;; the callback without having to create a new view with a new
                ;; context.
                (scheme-object (gethash scheme nyxt::*schemes*)))
            (funcall* (callback scheme-object)
                      (webkit:webkit-uri-scheme-request-get-uri request)
                      (find (webkit:webkit-uri-scheme-request-get-web-view request)
                            (delete nil
                                    (append (list (status-buffer (current-window))
                                                  (message-buffer (current-window)))
                                            (nyxt::active-prompt-buffers (current-window))
                                            (nyxt::panel-buffers (current-window))
                                            (buffer-list)))
                            :key #'gtk-object))))
        (or (error-callback scheme-object)
            (lambda (condition)
              (echo-warning "Error while routing ~s resource: ~a" scheme condition))))
       ;; We err on the side of caution, assigning the most restrictive policy
       ;; out of those provided. Should it be the other way around?
       (let ((manager (webkit:webkit-web-context-get-security-manager context)))
         (cond
           ((local-p scheme-object)
            (webkit:webkit-security-manager-register-uri-scheme-as-local
             manager scheme))
           ((no-access-p scheme-object)
            (webkit:webkit-security-manager-register-uri-scheme-as-no-access
             manager scheme))
           ((display-isolated-p scheme-object)
            (webkit:webkit-security-manager-register-uri-scheme-as-display-isolated
             manager scheme))
           ((secure-p scheme-object)
            (webkit:webkit-security-manager-register-uri-scheme-as-secure
             manager scheme))
           ((cors-enabled-p scheme-object)
            (webkit:webkit-security-manager-register-uri-scheme-as-cors-enabled
             manager scheme))
           ((empty-document-p scheme-object)
            (webkit:webkit-security-manager-register-uri-scheme-as-empty-document
             manager scheme)))))
     nyxt::*schemes*)
    (unless (or ephemeral-p
                (internal-context-p name))
      (let ((cookies-path (files:expand (make-instance 'cookies-file :context-name name))))
        (webkit:webkit-cookie-manager-set-persistent-storage
         cookie-manager
         (uiop:native-namestring cookies-path)
         :webkit-cookie-persistent-storage-text))
      (setf (ffi-buffer-cookie-policy cookie-manager) (default-cookie-policy *browser*)))
    context))

(defun internal-context-p (name)
  (equal name +internal+))

(defmethod customize-instance :after ((buffer gtk-buffer) &key extra-modes
                                                            no-hook-p
                                      &allow-other-keys)
  "Make BUFFER with EXTRA-MODES.
See `finalize-buffer'."
  (ffi-buffer-make buffer)
  (finalize-buffer buffer :extra-modes extra-modes :no-hook-p no-hook-p)
  (typecase buffer
    (status-buffer
     (%within-renderer-thread-async
      (lambda ()
        (with-slots (gtk-object) buffer
          (unless gtk-object
            (setf gtk-object (make-web-view (profile buffer) buffer))
            (connect-signal-function
             buffer "decide-policy"
             (make-decide-policy-handler buffer)))))))))

(define-ffi-method ffi-buffer-url ((buffer gtk-buffer))
  (quri:uri (webkit:webkit-web-view-uri (gtk-object buffer))))

(define-ffi-method ffi-buffer-title ((buffer gtk-buffer))
  (or (webkit:webkit-web-view-title (gtk-object buffer)) ""))

(define-ffi-method on-signal-load-failed-with-tls-errors ((buffer gtk-buffer) certificate url)
  "Return nil to propagate further (i.e. raise load-failed signal), T otherwise."
  (let* ((context (webkit:webkit-web-view-web-context (gtk-object buffer)))
         (host (quri:uri-host url)))
    (if (and (certificate-exceptions buffer)
             (member host (certificate-exceptions buffer) :test #'string=))
        (progn
          (webkit:webkit-web-context-allow-tls-certificate-for-host
           context
           (gobject:pointer certificate)
           host)
          (buffer-load url :buffer buffer)
          t)
        (progn
          (nyxt::tls-help buffer url)
          t))))

(define-ffi-method on-signal-decide-policy ((buffer gtk-buffer) response-policy-decision policy-decision-type-response)
  (let ((is-known-type t) (event-type :other) (modifiers ())
        is-new-window navigation-action navigation-type
        mouse-button url request mime-type method request-headers response-headers
        file-name toplevel-p response)
    (match policy-decision-type-response
      (:webkit-policy-decision-type-navigation-action
       (setf navigation-type (webkit:webkit-navigation-policy-decision-navigation-type response-policy-decision)))
      (:webkit-policy-decision-type-new-window-action
       (setf navigation-type (webkit:webkit-navigation-policy-decision-navigation-type response-policy-decision))
       (setf is-new-window t))
      (:webkit-policy-decision-type-response
       (setf request (webkit:webkit-response-policy-decision-request response-policy-decision))
       (setf is-known-type
             (webkit:webkit-response-policy-decision-is-mime-type-supported
              response-policy-decision))
       (setf response (webkit:webkit-response-policy-decision-response response-policy-decision))
       (setf mime-type (webkit:webkit-uri-response-mime-type response))
       (setf method (webkit:webkit-uri-request-get-http-method request))
       (setf file-name (webkit:webkit-uri-response-suggested-filename response))))
    ;; Set Event-Type
    (setf event-type
          (match navigation-type
            (:webkit-navigation-type-link-clicked :link-click)
            (:webkit-navigation-type-form-submitted :form-submission)
            (:webkit-navigation-type-back-forward :backward-or-forward)
            (:webkit-navigation-type-reload :reload)
            (:webkit-navigation-type-form-resubmitted :form-resubmission)
            (_ :other)))
    ;; Get Navigation Parameters from WebKitNavigationAction object
    (when navigation-type
      (setf navigation-action (webkit:webkit-navigation-policy-decision-get-navigation-action
                               response-policy-decision))
      (setf request (webkit:webkit-navigation-action-get-request navigation-action))
      (setf mouse-button (format nil "button~d"
                                 (webkit:webkit-navigation-action-get-mouse-button
                                  navigation-action)))
      (setf modifiers (funcall (modifier-translator *browser*)
                               (webkit:webkit-navigation-action-get-modifiers navigation-action))))
    (setf url (quri:uri (webkit:webkit-uri-request-uri request)))
    (setf request-headers (let ((headers (webkit:webkit-uri-request-get-http-headers request)))
                            (unless (cffi:null-pointer-p headers)
                              (webkit:soup-message-headers-get-headers headers))))
    (setf response-headers (when response
                             (let ((headers (webkit:webkit-uri-response-get-http-headers response)))
                               (unless (cffi:null-pointer-p headers)
                                 (webkit:soup-message-headers-get-headers headers)))))
    (setf toplevel-p (quri:uri=
                      url (quri:uri (webkit:webkit-web-view-uri
                                     (gtk-object buffer)))))
    (when toplevel-p
      (apply-auto-rules url buffer))
    (let* ((request-data
            (hooks:run-hook
             (request-resource-hook buffer)
             (sera:lret ((data (make-instance 'request-data
                                              :buffer buffer
                                              :url (quri:copy-uri url)
                                              :keys (unless (uiop:emptyp mouse-button)
                                                      (list (keymaps:make-key :value mouse-button
                                                                              :modifiers modifiers)))
                                              :event-type event-type
                                              :new-window-p is-new-window
                                              :http-method method
                                              :request-headers request-headers
                                              :response-headers response-headers
                                              :toplevel-p toplevel-p
                                              :mime-type mime-type
                                              :known-type-p is-known-type
                                              :file-name file-name)))
                        (setf (gtk-request data) request
                              (gtk-response data) response))))
           (keymap (when request-data
                     (nyxt::get-keymap (buffer request-data)
                                       (request-resource-keyscheme-map (buffer request-data)))))
           (bound-function (when request-data
                             (the (or symbol keymaps:keymap null)
                                  (keymaps:lookup-key (keys request-data) keymap)))))
      (cond
       ((not (typep request-data 'request-data))
        (log:debug "Don't forward to ~s's renderer (non request data)."
                   buffer)
        (webkit:webkit-policy-decision-ignore response-policy-decision))
       ;; FIXME: Do we ever use it? Do we actually need it?
       (bound-function
        (log:debug "Resource request key sequence ~a" (keyspecs-with-optional-keycode (keys request-data)))
        (funcall bound-function :url url :buffer buffer)
        (webkit:webkit-policy-decision-ignore response-policy-decision))
       ((new-window-p request-data)
        (log:debug "Load URL in new buffer: ~a" (render-url (url request-data)))
        (nyxt::open-urls (list (url request-data)))
        (webkit:webkit-policy-decision-ignore response-policy-decision))
       ((not (valid-scheme-p (quri:uri-scheme (url request-data))))
        (uiop:launch-program (list *open-program* (quri:render-uri (url request-data)))))
       ((and (quri:uri= url (url request-data))
             (str:starts-with-p "text/gemini" (mime-type request-data)))
        (log:debug "Processing gemtext from ~a." (render-url url))
        (enable-modes* 'nyxt/mode/small-web:small-web-mode (buffer request-data))
        (webkit:webkit-policy-decision-ignore response-policy-decision)
        (ffi-buffer-load-html
         buffer (nyxt/mode/small-web:gemtext-render (or (ignore-errors (dex:get (quri:render-uri url))) "") buffer)
         url))
       ((not (known-type-p request-data))
        (log:debug "Initiate download of ~s." (render-url (url request-data)))
        (webkit:webkit-policy-decision-download response-policy-decision))
       ((quri:uri= url (url request-data))
        (log:debug "Forward to ~s's renderer (unchanged URL)."
                   buffer)
        (webkit:webkit-policy-decision-use response-policy-decision))
       ((and (toplevel-p request-data)
             (not (quri:uri= (quri:uri (webkit:webkit-uri-request-uri request))
                             (url request-data))))
        ;; Low-level URL string, we must not render punycode so use
        ;; `quri:render-uri'.
        ;; See https://datatracker.ietf.org/doc/html/rfc3492.
        (setf (webkit:webkit-uri-request-uri request) (quri:render-uri (url request-data)))
        (log:debug "Don't forward to ~s's renderer (resource request replaced with ~s)."
                   buffer
                   (render-url (url request-data)))
        ;; Warning: We must ignore the policy decision _before_ we
        ;; start the new load request, or else WebKit will be
        ;; confused about which URL to load.
        (webkit:webkit-policy-decision-ignore response-policy-decision)
        (webkit:webkit-web-view-load-request (gtk-object buffer) request))
       (t
        (log:info "Cannot redirect to ~a in an iframe, forwarding to the original URL (~a)."
                  (render-url (url request-data))
                  (webkit:webkit-uri-request-uri request))
        (webkit:webkit-policy-decision-use response-policy-decision))))))

;; See https://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebView.html#WebKitLoadEvent
(defmethod on-signal-load-changed ((buffer gtk-buffer) load-event)
  ;; `url' can be nil if buffer didn't have any URL associated
  ;; to the web view, e.g. the start page, or if the load failed.
  (when (web-buffer-p buffer)
    (let* ((url (ignore-errors
                 (quri:uri (webkit:webkit-web-view-uri (gtk-object buffer)))))
           (url (if (url-empty-p url)
                    (url buffer)
                    url)))
      (cond ((eq load-event :webkit-load-started)
             (setf (nyxt::status buffer) :loading)
             (nyxt/web-extensions::tabs-on-updated buffer '(("status" . "loading")))
             (nyxt/web-extensions::tabs-on-updated buffer `(("url" . ,(render-url url))))
             (on-signal-load-started buffer url)
             (unless (internal-url-p url)
               (echo "Loading ~s." (render-url url))))
            ((eq load-event :webkit-load-redirected)
             (setf (url buffer) url)
             (nyxt/web-extensions::tabs-on-updated buffer '(("status" . "loading")))
             (nyxt/web-extensions::tabs-on-updated buffer `(("url" . ,(render-url url))))
             (on-signal-load-redirected buffer url))
            ((eq load-event :webkit-load-committed)
             (nyxt/web-extensions::tabs-on-updated buffer '(("status" . "loading")))
             (nyxt/web-extensions::tabs-on-updated buffer `(("url" . ,(render-url url))))
             (on-signal-load-committed buffer url))
            ((eq load-event :webkit-load-finished)
             (setf (loading-webkit-history-p buffer) nil)
             (unless (eq (slot-value buffer 'nyxt::status) :failed)
               (setf (nyxt::status buffer) :finished))
             (nyxt/web-extensions::tabs-on-updated buffer '(("status" . "complete")))
             (nyxt/web-extensions::tabs-on-updated buffer `(("url" . ,(render-url url))))
             (on-signal-load-finished buffer url)
             (unless (internal-url-p url)
               (echo "Finished loading ~s." (render-url url))))))))

(define-ffi-method on-signal-mouse-target-changed ((buffer gtk-buffer) hit-test-result modifiers)
  (declare (ignore modifiers))
  (alex:if-let ((url (or (webkit:webkit-hit-test-result-link-uri hit-test-result)
                         (webkit:webkit-hit-test-result-image-uri hit-test-result)
                         (webkit:webkit-hit-test-result-media-uri hit-test-result))))
    (progn
      (nyxt::print-message (str:concat "→ " (ffi-display-url *browser* url)))
      (setf (url-at-point buffer) (quri:uri url)))
    (progn
      (nyxt::print-message "")
      (setf (url-at-point buffer) (quri:uri "")))))

(define-ffi-method ffi-window-to-foreground ((window gtk-window))
  "Show window in foreground."
  (unless nyxt::*headless-p*
    (gtk:gtk-window-present (gtk-object window)))
  (call-next-method))

(define-ffi-method ffi-window-title ((window gtk-window))
  (gtk:gtk-window-title (gtk-object window)))
(define-ffi-method (setf ffi-window-title) (title (window gtk-window))
  (setf (gtk:gtk-window-title (gtk-object window)) title))

(define-ffi-method ffi-window-active ((browser gtk-browser))
  "Return the focused window."
  (or (find-if #'gtk:gtk-window-is-active (window-list) :key #'gtk-object)
      (call-next-method)))

(define-ffi-method ffi-window-set-buffer ((window gtk-window) (buffer gtk-buffer) &key (focus t))
  "Set BROWSER's WINDOW buffer to BUFFER."
  (let ((old-buffer (nyxt::active-buffer window)))
    ;; Just a precaution for the buffer to not be destroyed until we say so.
    ;; FIXME: Prompt buffer or something else seems to have no gtk-object.
    (when (gtk-object old-buffer)
      (g:g-object-ref (g:pointer (gtk-object old-buffer)))
      (gtk:gtk-container-remove (main-buffer-container window) (gtk-object old-buffer))
      (gtk:gtk-box-pack-start (main-buffer-container window) (gtk-object buffer) :expand t :fill t)
      (unless nyxt::*headless-p*
        (gtk:gtk-widget-show (gtk-object buffer)))
      (when focus
        (gtk:gtk-widget-grab-focus (gtk-object buffer))))
    (nyxt/web-extensions::tabs-on-activated old-buffer buffer)
    (nyxt/web-extensions::tabs-on-updated buffer `(("attention" . t)))
    (nyxt/web-extensions::tabs-on-updated
     old-buffer (alex:alist-hash-table `(("attention" . nil))))
    buffer))

(define-ffi-method ffi-window-add-panel-buffer ((window gtk-window) (buffer panel-buffer) side)
  "Add a panel buffer to a window."
  (match side
    (:left (gtk:gtk-box-pack-start (panel-buffer-container-left window) (gtk-object buffer))
           (push buffer (panel-buffers-left window)))
    (:right (gtk:gtk-box-pack-end (panel-buffer-container-right window) (gtk-object buffer))
            (push buffer (panel-buffers-right window))))
  (setf (gtk:gtk-widget-size-request (gtk-object buffer))
        (list (width buffer) -1))
  (unless nyxt::*headless-p*
    (gtk:gtk-widget-show (gtk-object buffer))))

(define-ffi-method ffi-width ((buffer panel-buffer))
  (nth-value 1 (gtk:gtk-widget-size-request (gtk-object buffer))))
(define-ffi-method (setf ffi-width) (width (buffer panel-buffer))
  (setf (gtk:gtk-widget-size-request (gtk-object buffer))
        (list width -1)))

(define-ffi-method ffi-window-delete-panel-buffer ((window gtk-window) (buffer panel-buffer))
  "Remove a panel buffer from a window."
  (cond ((find buffer (panel-buffers-left window))
         (setf (panel-buffers-left window) (remove buffer (panel-buffers-left window)))
         (gtk:gtk-container-remove (panel-buffer-container-left window) (gtk-object buffer)))
        ((find buffer (panel-buffers-right window))
         (setf (panel-buffers-right window) (remove buffer (panel-buffers-right window)))
         (gtk:gtk-container-remove (panel-buffer-container-right window) (gtk-object buffer)))))

(define-ffi-method ffi-height ((buffer prompt-buffer))
  (nth-value 1 (gtk:gtk-widget-size-request (prompt-buffer-container (window buffer)))))
(define-ffi-method (setf ffi-height) ((height integer) (buffer prompt-buffer))
  "Sets the prompt height.
As a special case, setting height to 0 means hiding prompt buffer and focusing
the `active-buffer'."
  (setf (gtk:gtk-widget-size-request (prompt-buffer-container (window buffer)))
        (list -1 height))
  (if (eql 0 height)
      (gtk:gtk-widget-grab-focus (gtk-object (nyxt::active-buffer (window buffer))))
      (gtk:gtk-widget-grab-focus (prompt-buffer-view (window buffer)))))

(define-ffi-method ffi-focus-prompt-buffer ((window gtk-window)
                                            (prompt-buffer prompt-buffer))
  "Focus PROMPT-BUFFER in WINDOW."
  (gtk:gtk-widget-grab-focus (prompt-buffer-view (window prompt-buffer)))
  prompt-buffer)

(define-ffi-method ffi-height ((buffer status-buffer))
  (gtk:gtk-widget-height-request (status-container (window buffer))))
(define-ffi-method (setf ffi-height) (height (buffer status-buffer))
  (setf (gtk:gtk-widget-height-request (status-container (window buffer)))
        height))

(define-ffi-method ffi-height ((buffer message-buffer))
  (gtk:gtk-widget-height-request (message-container (window buffer))))
(define-ffi-method (setf ffi-height) (height (buffer message-buffer))
  (setf (gtk:gtk-widget-height-request (message-container (window buffer)))
        height))

(defun get-bounds (object)
  (gtk:gtk-widget-get-allocation (nyxt/renderer/gtk::gtk-object object)))

(define-ffi-method ffi-height ((buffer gtk-buffer))
  (gdk:gdk-rectangle-height (get-bounds buffer)))
(define-ffi-method ffi-width ((buffer gtk-buffer))
  (gdk:gdk-rectangle-width (get-bounds buffer)))

(define-ffi-method ffi-height ((window gtk-window))
  (gdk:gdk-rectangle-height (get-bounds window)))
(define-ffi-method ffi-width ((window gtk-window))
  (gdk:gdk-rectangle-width (get-bounds window)))

(defun process-file-chooser-request (web-view file-chooser-request)
  (declare (ignore web-view))
  (with-protect ("Failed to process file chooser request: ~a" :condition)
    (when (native-dialogs *browser*)
      (gobject:g-object-ref (gobject:pointer file-chooser-request))
      (run-thread "file chooser"
                  (let* ((nyxt::*interactive-p* t)
                         (multiple (webkit:webkit-file-chooser-request-select-multiple
                                    file-chooser-request))
                         (files (mapcar
                                 #'uiop:native-namestring
                                 (handler-case
                                     (prompt :prompt (format nil "File~@[s~*~] to input" multiple)
                                             :input (or
                                                     (and
                                                      (webkit:webkit-file-chooser-request-selected-files
                                                       file-chooser-request)
                                                      (first
                                                       (webkit:webkit-file-chooser-request-selected-files
                                                        file-chooser-request)))
                                                     (uiop:native-namestring (uiop:getcwd)))
                                             :extra-modes 'nyxt/mode/file-manager:file-manager-mode
                                             :sources (make-instance 'nyxt/mode/file-manager:file-source
                                                                     :enable-marks-p multiple))
                                   (prompt-buffer-canceled ()
                                     nil)))))
                    (if files
                        (webkit:webkit-file-chooser-request-select-files
                         file-chooser-request
                         (cffi:foreign-alloc :string
                                             :initial-contents (mapcar #'cffi:foreign-string-alloc files)
                                             :count (if multiple
                                                        (length files)
                                                        1)
                                             :null-terminated-p t))
                        (webkit:webkit-file-chooser-request-cancel file-chooser-request))))
      t)))

(defun process-color-chooser-request (web-view color-chooser-request)
  (declare (ignore web-view))
  (with-protect ("Failed to process file chooser request: ~a" :condition)
    (when (native-dialogs *browser*)
      (gobject:g-object-ref (gobject:pointer color-chooser-request))
      (run-thread
          "color chooser"
        (ps-labels
          ((get-rgba
            (color)
            (let ((div (ps:chain document (create-element "div"))))
              (setf (ps:chain div style color)
                    (ps:lisp color))
              (ps:chain document body (append-child div))
              (ps:stringify (ps:chain window (get-computed-style div) color))))
           (get-opacity (color)
                        (let ((div (ps:chain document (create-element "div"))))
                          (setf (ps:chain div style color)
                                (ps:lisp color))
                          (ps:chain document body (append-child div))
                          (ps:stringify (ps:chain window (get-computed-style div) opacity)))))
          (let* ((rgba (gdk:make-gdk-rgba))
                 (rgba (progn (webkit:webkit-color-chooser-request-get-rgba
                               color-chooser-request rgba)
                              rgba))
                 (nyxt::*interactive-p* t)
                 (color-name (prompt1 :prompt "Color"
                                      :input (format nil "rgba(~d, ~d, ~d, ~d)"
                                                     (round (* 255 (gdk:gdk-rgba-red rgba)))
                                                     (round (* 255 (gdk:gdk-rgba-green rgba)))
                                                     (round (* 255 (gdk:gdk-rgba-blue rgba)))
                                                     (round (* 255 (gdk:gdk-rgba-alpha rgba))))
                                      :sources 'color-source))
                 (color (get-rgba color-name))
                 (opacity (sera:parse-float (get-opacity color-name)))
                 (rgba (gdk:gdk-rgba-parse color)))
            (unless (uiop:emptyp color)
              (webkit:webkit-color-chooser-request-set-rgba
               color-chooser-request
               (gdk:make-gdk-rgba :red (gdk:gdk-rgba-red rgba)
                                  :green (gdk:gdk-rgba-green rgba)
                                  :blue (gdk:gdk-rgba-blue rgba)
                                  :alpha (coerce opacity 'double-float)))
              (webkit:webkit-color-chooser-request-finish (g:pointer color-chooser-request))))))
      t)))

(defun process-script-dialog (web-view dialog)
  (declare (ignore web-view))
  (with-protect ("Failed to process dialog: ~a" :condition)
    (when (native-dialogs *browser*)
      (let ((dialog (gobject:pointer dialog)))
        (webkit:webkit-script-dialog-ref dialog)
        (run-thread "script dialog"
          (let ((nyxt::*interactive-p* t))
            (case (webkit:webkit-script-dialog-get-dialog-type dialog)
              (:webkit-script-dialog-alert (echo (webkit:webkit-script-dialog-get-message dialog)))
              (:webkit-script-dialog-prompt
               (let ((text (first (handler-case
                                      (prompt
                                       :prompt (webkit:webkit-script-dialog-get-message dialog)
                                       :input (webkit:webkit-script-dialog-prompt-get-default-text dialog)
                                       :sources 'prompter:raw-source)
                                    (prompt-buffer-canceled () nil)))))
                 (if text
                     (webkit:webkit-script-dialog-prompt-set-text dialog text)
                     (progn
                       (webkit:webkit-script-dialog-prompt-set-text dialog (cffi:null-pointer))
                       (webkit:webkit-script-dialog-close dialog)))))
              (:webkit-script-dialog-confirm
               (webkit:webkit-script-dialog-confirm-set-confirmed
                dialog (if-confirm ((webkit:webkit-script-dialog-get-message dialog)))))
              (:webkit-script-dialog-before-unload-confirm
               (webkit:webkit-script-dialog-confirm-set-confirmed
                dialog (if-confirm ((webkit:webkit-script-dialog-get-message dialog)
                                    :yes "leave" :no "stay"))))))
          (webkit:webkit-script-dialog-close dialog)
          (webkit:webkit-script-dialog-unref dialog))
        t))))

(defun process-permission-request (web-view request)
  (g:g-object-ref (g:pointer request))
  (run-thread "permission requester"
    (let ((nyxt::*interactive-p* t))
      (if-confirm ((format
                    nil "[~a] ~a"
                    (webkit:webkit-web-view-uri web-view)
                    (etypecase request
                      (webkit:webkit-geolocation-permission-request
                       "Grant this website geolocation access?")
                      (webkit:webkit-notification-permission-request
                       "Grant this website notifications access?")
                      (webkit:webkit-pointer-lock-permission-request
                       "Grant this website pointer access?")
                      (webkit:webkit-device-info-permission-request
                       "Grant this website device info access?")
                      (webkit:webkit-install-missing-media-plugins-permission-request
                       (format nil "Grant this website a media install permission for ~s?"
                               (webkit:webkit-install-missing-media-plugins-permission-request-get-description
                                request)))
                      (webkit:webkit-media-key-system-permission-request
                       (format nil "Grant this website an EME ~a key access?"
                               (webkit:webkit-media-key-system-permission-get-name request)))
                      (webkit:webkit-user-media-permission-request
                       (format nil "Grant this website a~@[~*n audio~]~@[~* video~] access?"
                               (webkit:webkit-user-media-permission-is-for-audio-device request)
                               (webkit:webkit-user-media-permission-is-for-video-device request)))
                      (webkit:webkit-website-data-access-permission-request
                       (format nil "Grant ~a an access to ~a data?"
                               (webkit:webkit-website-data-access-permission-request-get-requesting-domain
                                request)
                               (webkit:webkit-website-data-access-permission-request-get-current-domain
                                request)))))
                   :yes "grant" :no "deny")
                  (webkit:webkit-permission-request-allow request)
                  (webkit:webkit-permission-request-deny request)))))

(defun process-notification (web-view notification)
  (when (native-dialogs *browser*)
    (let* ((title (webkit:webkit-notification-get-title notification))
           (body (webkit:webkit-notification-get-body notification)))
      (echo "[~a] ~a: ~a" (webkit:webkit-web-view-uri web-view) title body)
      t)))

(defun buffer-destroy (buffer)
  (mapc (lambda (handler-id)
          (gobject:g-signal-handler-disconnect (gtk-object buffer) handler-id))
        (handler-ids buffer))
  (nyxt/web-extensions::tabs-on-removed buffer)
  (nyxt::buffer-hide buffer)
  (gtk:gtk-widget-destroy (gtk-object buffer))
  (setf (gtk-object buffer) nil))

(define-ffi-method ffi-buffer-make ((buffer gtk-buffer))
  "Initialize BUFFER's GTK web view."
  (unless (gtk-object buffer) ; Buffer may already have a view, e.g. the prompt-buffer.
    (setf (gtk-object buffer) (make-web-view (profile buffer) buffer)))
  (when (document-buffer-p buffer)
    (setf (ffi-buffer-smooth-scrolling-enabled-p buffer) (smooth-scrolling buffer)))
  ;; TODO: Maybe define an FFI method?
  (let ((settings (webkit:webkit-web-view-get-settings (gtk-object buffer))))
    (when (getf *options* :verbose)
      (setf (webkit:webkit-settings-enable-write-console-messages-to-stdout settings)
            t))
    (setf (webkit:webkit-settings-enable-resizable-text-areas settings) t
          (webkit:webkit-settings-enable-developer-extras settings) t
          (webkit:webkit-settings-enable-page-cache settings) t
          (webkit:webkit-settings-enable-encrypted-media settings) t))
  (connect-signal-function buffer "decide-policy" (make-decide-policy-handler buffer))
  (connect-signal buffer "resource-load-started" nil (web-view resource request)
    (declare (ignore web-view))
    (let* ((response (webkit:webkit-web-resource-response resource))
           (request-data (make-instance 'request-data
                                        :buffer buffer
                                        :url (quri:uri (webkit:webkit-uri-request-get-uri request))
                                        :event-type :other
                                        :new-window-p nil
                                        :resource-p t
                                        :http-method (webkit:webkit-uri-request-get-http-method request)
                                        :response-headers (when response
                                                            (let ((headers (webkit:webkit-uri-response-get-http-headers request)))
                                                              (unless (cffi:null-pointer-p headers)
                                                                (webkit:soup-message-headers-get-headers headers))))
                                        :request-headers (let ((headers (webkit:webkit-uri-request-get-http-headers request)))
                                                           (unless (cffi:null-pointer-p headers)
                                                             (webkit:soup-message-headers-get-headers headers)))
                                        :toplevel-p nil
                                        :mime-type (when response
                                                     (webkit:webkit-uri-response-mime-type response))
                                        :known-type-p t)))
      (setf (gtk-response request-data) response
            (gtk-request request-data) request
            (gtk-resource request-data) resource)
      (when (request-resource-hook buffer)
        (hooks:run-hook (request-resource-hook buffer) request-data))))
  (connect-signal buffer "load-changed" t (web-view load-event)
    (declare (ignore web-view))
    (on-signal-load-changed buffer load-event))
  (connect-signal buffer "mouse-target-changed" nil (web-view hit-test-result modifiers)
    (declare (ignore web-view))
    (on-signal-mouse-target-changed buffer hit-test-result modifiers))
  ;; Mouse events are captured by the web view first, so we must intercept them here.
  (connect-signal buffer "button-press-event" nil (web-view event)
    (declare (ignore web-view))
    (on-signal-button-press-event buffer event))
  (connect-signal buffer "scroll-event" nil (web-view event)
    (declare (ignore web-view))
    (on-signal-scroll-event buffer event))
  (connect-signal-function buffer "script-dialog" #'process-script-dialog)
  (connect-signal-function buffer "run-file-chooser" #'process-file-chooser-request)
  (connect-signal-function buffer "run-color-chooser" #'process-color-chooser-request)
  (connect-signal-function buffer "permission-request" #'process-permission-request)
  (connect-signal-function buffer "show-notification" #'process-notification)
  ;; TLS certificate handling
  (connect-signal buffer "load-failed-with-tls-errors" nil (web-view failing-url certificate errors)
    (declare (ignore web-view errors))
    (on-signal-load-failed buffer (quri:uri failing-url))
    (on-signal-load-failed-with-tls-errors buffer certificate (quri:uri failing-url)))
  (connect-signal buffer "notify::uri" nil (web-view param-spec)
    (declare (ignore param-spec))
    (nyxt/web-extensions::tabs-on-updated
     buffer `(("url" . ,(webkit:webkit-web-view-uri web-view))))
    (on-signal-notify-uri buffer nil))
  (connect-signal buffer "notify::title" nil (web-view param-spec)
    (declare (ignore  param-spec))
    (nyxt/web-extensions::tabs-on-updated
     buffer `(("title" . ,(webkit:webkit-web-view-title web-view))))
    (on-signal-notify-title buffer nil))
  (connect-signal buffer "web-process-terminated" nil (web-view reason)
    ;; TODO: Bind WebKitWebProcessTerminationReason in cl-webkit.
    (echo-warning
     "Web process terminated for buffer ~a (opening ~a) because ~[it crashed~;of memory exhaustion~;we had to close it~]"
     (id buffer)
     (url buffer)
     (cffi:foreign-enum-value 'webkit:webkit-web-process-termination-reason reason))
    (log:debug
     "Web process terminated for web view ~a because of ~[WEBKIT_WEB_PROCESS_CRASHED~;WEBKIT_WEB_PROCESS_EXCEEDED_MEMORY_LIMIT~;WEBKIT_WEB_PROCESS_TERMINATED_BY_API~]"
     web-view
     (cffi:foreign-enum-value 'webkit:webkit-web-process-termination-reason reason))
    (buffer-delete buffer))
  (connect-signal buffer "close" nil (web-view)
    (declare (ignore web-view))
    (buffer-destroy buffer))
  (connect-signal buffer "load-failed" nil (web-view load-event failing-url error)
    (declare (ignore load-event web-view))
    ;; TODO: WebKitGTK sometimes (when?) triggers "load-failed" when loading a
    ;; page from the webkit-history cache.  Upstream bug?  Anyways, we should
    ;; ignore these.
    (on-signal-load-failed buffer (quri:uri failing-url))
    (cond
      ((loading-webkit-history-p buffer)
       (setf (loading-webkit-history-p buffer) nil))
      ((= 302 (webkit::g-error-code error))
       (on-signal-load-canceled buffer (quri:uri failing-url)))
      ((or (member (slot-value buffer 'nyxt::status) '(:finished :failed))
           ;; WebKitGTK emits the WEBKIT_PLUGIN_ERROR_WILL_HANDLE_LOAD
           ;; (204) if the plugin will handle loading content of the
           ;; URL. This often happens with videos. The only thing we
           ;; can do is ignore it.
           ;;
           ;; TODO: Use cl-webkit provided error types. How
           ;; do we use it, actually?
           (= 204 (webkit::g-error-code error)))
       nil)
      (t
       (echo "Failed to load URL ~a in buffer ~a." failing-url (id buffer))
       (setf (nyxt::status buffer) :failed)
       (ffi-buffer-load-alternate-html
        buffer
        (spinneret:with-html-string
          (:head
           (:nstyle (style buffer)))
          (:h1 "Page could not be loaded.")
          (:h2 "URL: " failing-url)
          (:ul
           (:li "Try again in a moment, maybe the site will be available again.")
           (:li "If the problem persists for every site, check your Internet connection.")
           (:li "Make sure the URL is valid."
                (when (quri:uri-https-p (quri:uri failing-url))
                  "If this site does not support HTTPS, try with HTTP (insecure)."))))
        failing-url
        failing-url)))
    t)
  (connect-signal buffer "create" nil (web-view navigation-action)
    (declare (ignore web-view))
    (let ((url (webkit:webkit-uri-request-uri
                (webkit:webkit-navigation-action-get-request
                 (gobject:pointer navigation-action)))))
      (gtk-object (make-buffer-focus :url (quri:uri url) :parent-buffer (current-buffer)))))
  (connect-signal buffer "context-menu" nil (web-view context-menu event hit-test-result)
    (declare (ignore web-view event hit-test-result))
    (loop with length = (webkit:webkit-context-menu-get-n-items context-menu)
          for i below length
          for item = (webkit:webkit-context-menu-get-item-at-position context-menu i)
          when (and (or (status-buffer-p buffer) (message-buffer-p buffer))
                    (not (eq (webkit:webkit-context-menu-item-get-stock-action item)
                             :webkit-context-menu-action-inspect-element)))
            do (webkit:webkit-context-menu-remove context-menu item)
          else
            do (match (webkit:webkit-context-menu-item-get-stock-action item)
                 (:webkit-context-menu-action-open-link-in-new-window
                  (webkit:webkit-context-menu-remove context-menu item)
                  (webkit:webkit-context-menu-insert
                   context-menu
                   (webkit:webkit-context-menu-item-new-from-stock-action-with-label
                    :webkit-context-menu-action-open-link-in-new-window
                    "Open Link in New Buffer")
                   i))))
    (webkit:webkit-context-menu-append
     context-menu (webkit:webkit-context-menu-item-new-separator))
    (let* ((accessible-commands
             (mapcar #'name
                     (nyxt::list-commands
                      :global-p t
                      :mode-symbols (mapcar #'sera:class-name-of
                                            (sera:filter #'enabled-p (modes buffer)))))))
      (maphash (lambda (label function)
                 (flet ((make-item (label function)
                          ;; Using stock actions here, because cl-cffi-gtk has a terrible API
                          ;; for GActions, requiring an exact type to be passed and disallowing
                          ;; NULL as a type :/
                          (sera:lret ((item (webkit:webkit-context-menu-item-new-from-stock-action-with-label
                                             :webkit-context-menu-action-action-custom label)))
                            (gobject:g-signal-connect
                             (webkit:webkit-context-menu-item-get-g-action item) "activate"
                             (lambda (action parameter)
                               (declare (ignore action parameter))
                               (nyxt::run-async function))))))
                   (cond
                     ((or (and (command-p function)
                               (member function accessible-commands))
                          (functionp function))
                      (webkit:webkit-context-menu-append context-menu (make-item label function)))
                     ((listp function)
                      (let ((submenu (webkit:webkit-context-menu-new)))
                        (loop for (command command-label) in function
                              do (webkit:webkit-context-menu-append
                                  submenu (make-item command-label command)))
                        (webkit:webkit-context-menu-append
                         context-menu
                         (webkit:webkit-context-menu-item-new-with-submenu label submenu)))))))
               nyxt::*context-menu-commands*))
    nil)
  (connect-signal buffer "enter-fullscreen" nil (web-view)
    (declare (ignore web-view))
    (setf (nyxt::fullscreen-p (current-window)) t)
    (toggle-fullscreen :skip-renderer-resize t)
    nil)
  (connect-signal buffer "leave-fullscreen" nil (web-view)
    (declare (ignore web-view))
    (setf (nyxt::fullscreen-p (current-window)) nil)
    (toggle-fullscreen :skip-renderer-resize t)
    nil)
  (when (context-buffer-p buffer)
    (connect-signal buffer "user-message-received" nil (view message)
      (declare (ignorable view))
      (run-thread "Resolving browser.test.method"
        (log:info "Resolving browser.test.method with name ~s"
                  (webkit:webkit-user-message-get-name message))
        (sleep 0.1)
        (if (equal "browser.tabs.create" (webkit:webkit-user-message-get-name message))
            (let* ((props (elt (njson:decode (webkit2:g-variant-get-maybe-string
                                              (webkit:webkit-user-message-get-parameters message)))
                               0))
                   (buffer (make-buffer :url (j:get "url" props)))
                   (content (njson:encode (sera:dict "url" (quri:render-uri (url buffer)) "id" (id buffer)))))
              (log:info "Got a browser.tabs.create message with ~s URL" (j:get "url" props))
              (webkit:webkit-user-message-send-reply
               message
               (webkit:webkit-user-message-new
                "browser.tabs.create" (glib:g-variant-new-string content))))
            (webkit:webkit-user-message-send-reply
             message
             (webkit:webkit-user-message-new
              (webkit:webkit-user-message-get-name message)
              (cffi:null-pointer)))))
      t)
    (nyxt/web-extensions::tabs-on-created buffer))
  buffer)

(define-ffi-method ffi-buffer-delete ((buffer gtk-buffer))
  (cond
   ((not (slot-value buffer 'gtk-object))
    (nyxt::buffer-hide buffer))
   ((webkit:webkit-web-view-is-web-process-responsive (gtk-object buffer))
    (webkit:webkit-web-view-try-close (gtk-object buffer)))
   (t (buffer-destroy buffer))))

(define-ffi-method ffi-buffer-load ((buffer gtk-buffer) url)
  "Load URL in BUFFER.
An optimization technique is to make use of the renderer history cache.
For WebKit, if the URL matches an entry in the webkit-history then we fetch the
page from the cache.

We don't use the cache if URL matches BUFFER's URL since this means the user
requested a reload.

Note that we don't use the cache for internal pages (say nyxt:help) since it's
local anyways, and it's better to refresh it if a load was queried."
  (declare (type quri:uri url))
  (let* ((history (webkit-history buffer))
         (entry (or (find url history :test #'quri:uri= :key #'webkit-history-entry-url)
                    (find url history :test #'quri:uri= :key #'webkit-history-entry-original-url))))
    ;; Mark buffer as :loading right away so functions like `window-set-buffer'
    ;; don't try to reload if they are called before the "load-changed" signal
    ;; is emitted.
    (when (web-buffer-p buffer)
      (setf (nyxt::status buffer) :loading))
    (if (and entry
             (not (internal-url-p url))
             (not (quri:uri= url (url buffer))))
        (progn
          (log:debug "Load URL from history entry ~a" entry)
          (load-webkit-history-entry buffer entry))
        (webkit:webkit-web-view-load-uri (gtk-object buffer) (quri:render-uri url)))))

(define-ffi-method ffi-buffer-load-html ((buffer gtk-buffer) html-content url)
  (declare (type quri:uri url))
  (webkit:webkit-web-view-load-html (gtk-object buffer)
                                    html-content
                                    (if (url-empty-p url)
                                        "about:blank"
                                        (render-url url))))

(define-ffi-method ffi-buffer-load-alternate-html ((buffer gtk-buffer) html-content content-url url)
  (webkit:webkit-web-view-load-alternate-html (gtk-object buffer)
                                              html-content
                                              (quri:render-uri (ensure-url content-url))
                                              (let ((url (ensure-url url)))
                                                (if (url-empty-p url)
                                                    "about:blank"
                                                    (quri:render-uri url)))))

(defmethod ffi-buffer-evaluate-javascript ((buffer gtk-buffer) javascript &optional world-name)
  (%within-renderer-thread
   (lambda (&optional channel)
     (when (gtk-object buffer)
       (webkit2:webkit-web-view-evaluate-javascript
        (gtk-object buffer)
        javascript
        (if channel
            (lambda (result jsc-result)
              (declare (ignore jsc-result))
              (calispel:! channel result))
            (lambda (result jsc-result)
              (declare (ignore jsc-result))
              result))
        (lambda (condition)
          (nyxt::javascript-error-handler condition)
          ;; Notify the listener that we are done.
          (when channel
            (calispel:! channel nil)))
        world-name)))))

(defmethod ffi-buffer-evaluate-javascript-async ((buffer gtk-buffer) javascript &optional world-name)
  (%within-renderer-thread-async
   (lambda ()
     (when (gtk-object buffer)
       (webkit2:webkit-web-view-evaluate-javascript
        (gtk-object buffer)
        javascript
        nil
        #'nyxt::javascript-error-handler
        world-name)))))

(defun list-of-string-to-foreign (list)
  (if list
      (cffi:foreign-alloc :string
                          :count (length list)
                          :initial-contents list
                          :null-terminated-p t)
      (cffi:null-pointer)))

(define-class gtk-user-style ()
  ((gtk-object))
  (:export-class-name-p t)
  (:export-accessor-names-p t))

(define-ffi-method ffi-buffer-add-user-style ((buffer gtk-buffer) (style gtk-user-style))
  (let* ((content-manager
           (webkit:webkit-web-view-get-user-content-manager
            (gtk-object buffer)))
         (frames (if (nyxt/mode/user-script:all-frames-p style)
                     :webkit-user-content-inject-all-frames
                     :webkit-user-content-inject-top-frame))
         (style-level (if (eq (nyxt/mode/user-script:level style) :author)
                          :webkit-user-style-level-author
                          :webkit-user-style-level-user))
         (style-sheet
           (if (nyxt/mode/user-script:world-name style)
               (webkit:webkit-user-style-sheet-new-for-world
                (nyxt/mode/user-script:code style)
                frames style-level
                (nyxt/mode/user-script:world-name style)
                (list-of-string-to-foreign (nyxt/mode/user-script:include style))
                (list-of-string-to-foreign (nyxt/mode/user-script:exclude style)))
               (webkit:webkit-user-style-sheet-new
                (nyxt/mode/user-script:code style)
                frames style-level
                (list-of-string-to-foreign (nyxt/mode/user-script:include style))
                (list-of-string-to-foreign (nyxt/mode/user-script:exclude style))))))
    (setf (gtk-object style) style-sheet)
    (webkit:webkit-user-content-manager-add-style-sheet
     content-manager style-sheet)
    style))

(define-ffi-method ffi-buffer-remove-user-style ((buffer gtk-buffer) (style gtk-user-style))
  (let ((content-manager
          (webkit:webkit-web-view-get-user-content-manager
           (gtk-object buffer))))
    (webkit:webkit-user-content-manager-remove-style-sheet
     content-manager (gtk-object style))))

(define-class gtk-user-script ()
  ((gtk-object))
  (:export-class-name-p t)
  (:export-accessor-names-p t))




(define-ffi-method ffi-buffer-add-user-script ((buffer gtk-buffer) (script gtk-user-script))
  (alex:if-let ((code (nyxt/mode/user-script:code script)))
    (let* ((content-manager
             (webkit:webkit-web-view-get-user-content-manager
              (gtk-object buffer)))
           (frames (if (nyxt/mode/user-script:all-frames-p script)
                       :webkit-user-content-inject-all-frames
                       :webkit-user-content-inject-top-frame))
           (inject-time (if (eq :document-start (nyxt/mode/user-script:run-at script))
                            :webkit-user-script-inject-at-document-start
                            :webkit-user-script-inject-at-document-end))
           (allow-list (list-of-string-to-foreign
                        (or (nyxt/mode/user-script:include script)
                            '("http://*/*" "https://*/*"))))
           (block-list (list-of-string-to-foreign
                        (nyxt/mode/user-script:exclude script)))
           (user-script (if (nyxt/mode/user-script:world-name script)
                            (webkit:webkit-user-script-new-for-world
                             code frames inject-time
                             (nyxt/mode/user-script:world-name script) allow-list block-list)
                            (webkit:webkit-user-script-new
                             code frames inject-time allow-list block-list))))
      (setf (gtk-object script) user-script)
      (webkit:webkit-user-content-manager-add-script
       content-manager user-script)
      script)
    (echo-warning "User script ~a is empty." script)))

(define-ffi-method ffi-buffer-remove-user-script ((buffer gtk-buffer) (script gtk-user-script))
  (let ((content-manager
          (webkit:webkit-web-view-get-user-content-manager
           (gtk-object buffer))))
    (when (and script (gtk-object script))
      (webkit:webkit-user-content-manager-remove-script
       content-manager (gtk-object script)))))

(defmacro define-ffi-settings-accessor (setting-name webkit-setting)
  (let ((full-name (intern (format nil "FFI-BUFFER-~a" setting-name))))
    (symbol-function full-name)
    `(progn
       (define-ffi-method ,full-name ((buffer gtk-buffer))
         (,webkit-setting
          (webkit:webkit-web-view-get-settings (gtk-object buffer))))
       (define-ffi-method (setf ,full-name) (value (buffer gtk-buffer))
         (setf (,webkit-setting
                (webkit:webkit-web-view-get-settings (gtk-object buffer)))
               value)))))

(define-ffi-settings-accessor javascript-enabled-p webkit:webkit-settings-enable-javascript)
(define-ffi-settings-accessor javascript-markup-enabled-p webkit:webkit-settings-enable-javascript-markup)
(define-ffi-settings-accessor smooth-scrolling-enabled-p webkit:webkit-settings-enable-smooth-scrolling)
#+webkit2-media
(define-ffi-settings-accessor media-enabled-p webkit:webkit-settings-enable-media)
(define-ffi-settings-accessor webgl-enabled-p webkit:webkit-settings-enable-webgl)
(define-ffi-settings-accessor auto-load-image-enabled-p webkit:webkit-settings-auto-load-images)

#+webkit2-mute
(defmethod ffi-buffer-sound-enabled-p ((buffer gtk-buffer))
  (not (webkit:webkit-web-view-get-is-muted (gtk-object buffer))))
#+webkit2-mute
(defmethod (setf ffi-buffer-sound-enabled-p) (value (buffer gtk-buffer))
  (nyxt/web-extensions::tabs-on-updated
   buffer (alex:alist-hash-table `(("audible" . ,value))))
  (webkit:webkit-web-view-set-is-muted (gtk-object buffer) (not value)))

;; KLUDGE: PDF.js in WebKit (actual for version 2.41.4) always saves
;; PDFs as "document.pdf". This is because WebKit does not pass "file"
;; parameter to the viewer. See
;; https://stackoverflow.com/questions/47098206/pdf-js-downloading-as-document-pdf-instead-of-filename .
;; Here we restore the original file name from an URL if a suggested
;; file name looks suspicious.
(sera:-> maybe-fix-pdfjs-filename (string quri:uri)
         (values string &optional))
(defun maybe-fix-pdfjs-filename (suggested-file-name uri)
  (let ((pathname (pathname (quri:uri-path uri))))
    (if (and (string= suggested-file-name "document.pdf")
             (string= (pathname-type pathname) "pdf"))
        (uiop:native-namestring
         (make-pathname :name (pathname-name pathname)
                        :type "pdf"))
        suggested-file-name)))

(defun wrap-download (webkit-download)
  (sera:lret ((original-url (url (current-buffer)))
              (download (make-instance 'nyxt/mode/download:download
                                       :url (webkit:webkit-uri-request-uri
                                             (webkit:webkit-download-get-request webkit-download))
                                       :gtk-object webkit-download)))
    (nyxt/mode/download:list-downloads)
    (setf (nyxt/mode/download::cancel-function download)
          #'(lambda ()
              (setf (nyxt/mode/download:status download) :canceled)
              (webkit:webkit-download-cancel webkit-download)))
    (hooks:run-hook (nyxt/mode/download:before-download-hook download) download)
    (push download (downloads *browser*))
    (connect-signal download "received-data" nil (webkit-download data-length)
      (declare (ignore data-length))
      (setf (nyxt/mode/download:bytes-downloaded download)
            (webkit:webkit-download-get-received-data-length webkit-download))
      (setf (nyxt/mode/download:completion-percentage download)
            (* 100 (webkit:webkit-download-estimated-progress webkit-download))))
    (connect-signal download "decide-destination" nil (webkit-download suggested-file-name)
      (alex:when-let* ((suggested-file-name (maybe-fix-pdfjs-filename suggested-file-name original-url))
                       (download-dir (or (ignore-errors
                                          (download-directory
                                           (find (webkit:webkit-download-get-web-view webkit-download)
                                                 (buffer-list) :key #'gtk-object)))
                                         (make-instance 'download-directory)))
                       (download-directory (files:expand download-dir))
                       (native-download-directory (unless (files:nil-pathname-p download-directory)
                                                    (uiop:native-namestring download-directory)))
                       (path (str:concat native-download-directory suggested-file-name))
                       (unique-path (download-manager::ensure-unique-file path))
                       (file-path (format nil "file://~a" unique-path)))
        (if (string= path unique-path)
            (log:debug "Downloading file to ~s." unique-path)
            (echo "Destination ~s exists, saving as ~s." path unique-path))
        (webkit:webkit-download-set-destination webkit-download file-path)))
    (connect-signal download "created-destination" nil (webkit-download destination)
      (declare (ignore destination))
      (setf (nyxt/mode/download:destination-path download)
            (uiop:ensure-pathname
             (quri:uri-path (quri:uri
                             (webkit:webkit-download-destination webkit-download))))))
    (connect-signal download "failed" nil (webkit-download error)
      (declare (ignore error))
      (unless (eq (nyxt/mode/download:status download) :canceled)
        (setf (nyxt/mode/download:status download) :failed))
      (echo "Download failed for ~s."
            (webkit:webkit-uri-request-uri
             (webkit:webkit-download-get-request webkit-download))))
    (connect-signal download "finished" nil (webkit-download)
      (declare (ignore webkit-download))
      (unless (member (nyxt/mode/download:status download) '(:canceled :failed))
        (setf (nyxt/mode/download:status download) :finished)
        ;; If download was too small, it may not have been updated.
        (setf (nyxt/mode/download:completion-percentage download) 100)
        (hooks:run-hook (nyxt/mode/download:after-download-hook download) download)))))

(defmethod ffi-buffer-download ((buffer gtk-buffer) url)
  (let* ((webkit-download (webkit:webkit-web-view-download-uri (gtk-object buffer) url))
         (download (make-instance 'nyxt/mode/download:download
                                  :url url
                                  :gtk-object webkit-download)))
    (wrap-download webkit-download)
    download))

(define-ffi-method ffi-buffer-user-agent ((buffer gtk-buffer))
  (alex:when-let ((settings (webkit:webkit-web-view-get-settings (gtk-object buffer))))
    (webkit:webkit-settings-user-agent settings)))

(define-ffi-method (setf ffi-buffer-user-agent) (value (buffer gtk-buffer))
  (alex:when-let ((settings (webkit:webkit-web-view-get-settings (gtk-object buffer))))
    (setf (webkit:webkit-settings-user-agent settings) value)))

(define-ffi-method ffi-buffer-proxy ((buffer gtk-buffer))
  "Return the proxy URL and list of ignored hosts (a list of strings) as second value."
  (the (values (or quri:uri null) (list-of string))
       (values (gtk-proxy-url buffer)
               (proxy-ignored-hosts buffer))))
(define-ffi-method (setf ffi-buffer-proxy) (proxy-specifier
                                            (buffer gtk-buffer))
  "Redirect network connections of BUFFER to proxy server PROXY-URL.
Hosts in IGNORE-HOSTS (a list of strings) ignore the proxy.
For the user-level interface, see `proxy-mode'.

PROXY-SPECIFIER is either a PROXY-URL or a pair of (PROXY-URL IGNORE-HOSTS).

Note: WebKit supports three proxy 'modes': default (the system proxy),
custom (the specified proxy) and none."
  (let ((proxy-url (first (alex:ensure-list proxy-specifier)))
        (ignore-hosts (or (second (alex:ensure-list proxy-specifier))
                          nil)))
    (declare (type quri:uri proxy-url))
    (setf (gtk-proxy-url buffer) proxy-url)
    (setf (proxy-ignored-hosts buffer) ignore-hosts)
    (let* ((context (webkit:webkit-web-view-web-context (gtk-object buffer)))
           (settings (cffi:null-pointer))
           (mode :webkit-network-proxy-mode-no-proxy)
           (ignore-hosts (cffi:foreign-alloc :string
                                             :initial-contents ignore-hosts
                                             :null-terminated-p t)))
      (unless (url-empty-p proxy-url)
        (setf mode :webkit-network-proxy-mode-custom)
        (setf settings
              (webkit:webkit-network-proxy-settings-new
               (render-url proxy-url)
               ignore-hosts)))
      (cffi:foreign-free ignore-hosts)
      (webkit:webkit-web-context-set-network-proxy-settings
       context mode settings))))

(define-ffi-method ffi-buffer-zoom-level ((buffer gtk-buffer))
  (webkit:webkit-web-view-zoom-level (gtk-object buffer)))
(define-ffi-method (setf ffi-buffer-zoom-level) (value (buffer gtk-buffer))
  (when (and (floatp value) (>= value 0))
    (setf (webkit:webkit-web-view-zoom-level (gtk-object buffer)) value)))

(define-ffi-method ffi-generate-input-event ((window gtk-window) event)
  (when event
    ;; The "send_event" field is used to mark the event as an "unconsumed"
    ;; keypress.  The distinction allows us to avoid looping indefinitely.
    (etypecase event
      (gdk:gdk-event-button
       (setf (gdk:gdk-event-button-send-event event) t))
      (gdk:gdk-event-key
       (setf (gdk:gdk-event-key-send-event event) t))
      (gdk:gdk-event-scroll
       (setf (gdk:gdk-event-scroll-send-event event) t)))
    (gtk:gtk-main-do-event event)))

(define-ffi-method ffi-generated-input-event-p ((window gtk-window) event)
  (gdk:gdk-event-send-event event))

(define-ffi-method ffi-inspector-show ((buffer gtk-buffer))
  (webkit:webkit-web-inspector-show
   (webkit:webkit-web-view-get-inspector (gtk-object buffer))))

;; This method does not need a renderer, so no need to use `define-ffi-method'
;; which is prone to race conditions.
(defmethod ffi-display-url ((browser gtk-browser) text)
  (declare (ignore browser))
  (webkit:webkit-uri-for-display text))

(defmethod ffi-buffer-cookie-policy ((buffer gtk-buffer))
  (if (renderer-thread-p nyxt::*renderer*)
      (progn
        (log:warn "Querying cookie policy in WebKitGTK is only supported from a non-renderer thread.")
        nil)
      (let ((result-channel (nyxt::make-channel 1)))
        (run-thread "WebKitGTK cookie-policy"
          (within-gtk-thread
            (let* ((context (webkit:webkit-web-view-web-context (gtk-object buffer)))
                   (cookie-manager (webkit:webkit-web-context-get-cookie-manager context)))
              ;; TODO: Update upstream to export and fix `with-g-async-ready-callback'.
              (webkit::with-g-async-ready-callback (callback
                                                     (declare (ignorable webkit::user-data webkit::source-object))
                                                     (calispel:! result-channel
                                                                 (webkit:webkit-cookie-manager-get-accept-policy-finish
                                                                  cookie-manager
                                                                  webkit::result)))
                (webkit:webkit-cookie-manager-get-accept-policy
                 cookie-manager
                 (cffi:null-pointer)
                 callback
                 (cffi:null-pointer))))))
        (calispel:? result-channel))))
(defmethod (setf ffi-buffer-cookie-policy) (value (buffer gtk-buffer))
  "Set the cookie policy to VALUE.
Valid values are determined by the `cookie-policy' type."
  (let* ((context (webkit:webkit-web-view-web-context (gtk-object buffer)))
         (cookie-manager (webkit:webkit-web-context-get-cookie-manager context)))
    (setf (ffi-buffer-cookie-policy cookie-manager) value)
    buffer))
(defmethod (setf ffi-buffer-cookie-policy) (value (cookie-manager webkit:webkit-cookie-manager))
  "Set the cookie policy to VALUE.
Valid values are determined by the `cookie-policy' type."
  (webkit:webkit-cookie-manager-set-accept-policy
   cookie-manager
   (match value
     (:accept :webkit-cookie-policy-accept-always)
     (:never :webkit-cookie-policy-accept-never)
     (:no-third-party :webkit-cookie-policy-accept-no-third-party))))

(defmethod ffi-preferred-languages ((buffer gtk-buffer))
  "Not supported by WebKitGTK.
Only the setf method is."
  nil)
(defmethod (setf ffi-preferred-languages) (language-list (buffer gtk-buffer))
  "LANGUAGE-LIST is a list of strings like '(\"en_US\" \"fr_FR\")."
  (let ((langs (cffi:foreign-alloc :string
                                   :initial-contents language-list
                                   :null-terminated-p t)))
    (webkit:webkit-web-context-set-preferred-languages
     (webkit:webkit-web-view-web-context (gtk-object buffer))
     langs)))

(defstruct webkit-history-entry
  title
  url
  original-url
  gtk-object)

(define-ffi-method webkit-history ((buffer gtk-buffer))
  "Return a list of `webkit-history-entry's for the current buffer.
Oldest entries come last.

This represents the history as remembered by WebKit.  Note that it is linear so
it does not map 1:1 with Nyxt's history tree.  Nonetheless it allows us to make
use of the WebKit history case for the current branch.  See `ffi-buffer-load'.

As a second value, return the current buffer index starting from 0."
  (let* ((bf-list (webkit:webkit-web-view-get-back-forward-list (gtk-object buffer)))
         (length (webkit:webkit-back-forward-list-get-length bf-list))
         (current (webkit:webkit-back-forward-list-get-current-item bf-list))
         (history-list nil)
         (current-index 0))
    ;; The back-forward list is both negatively and positively indexed.  Seems
    ;; that we can't easily know the first index nor the last one.  So let's
    ;; iterate over the length backwards and forwards to make sure we get all
    ;; elements in order.
    (loop for i from (- length) to length
          for item = (webkit:webkit-back-forward-list-get-nth-item bf-list i)
          when (eq item current)
          do (setf current-index (- length (length history-list))) ; Index from 0.
          when item
          do (push (make-webkit-history-entry
                    :title (webkit:webkit-back-forward-list-item-get-title item)
                    :url (quri:uri (webkit:webkit-back-forward-list-item-get-uri item))
                    :original-url (quri:uri (webkit:webkit-back-forward-list-item-get-original-uri item))
                    :gtk-object item)
                   history-list))
    (values history-list current-index)))

(defmethod load-webkit-history-entry ((buffer gtk-buffer) history-entry)
  (setf (loading-webkit-history-p buffer) t)
  (webkit:webkit-web-view-go-to-back-forward-list-item
   (gtk-object buffer)
   (webkit-history-entry-gtk-object history-entry)))

(define-ffi-method ffi-focused-p ((buffer gtk-buffer))
  (gtk:gtk-widget-is-focus (gtk-object buffer)))

(define-ffi-method ffi-muted-p ((buffer gtk-buffer))
  #+webkit2-mute
  (webkit:webkit-web-view-is-muted (gtk-object buffer)))

(define-ffi-method ffi-tracking-prevention ((buffer gtk-buffer))
  #+webkit2-tracking
  (webkit:webkit-website-data-manager-get-itp-enabled
   (webkit:webkit-web-context-website-data-manager
    (webkit:webkit-web-view-web-context (gtk-object buffer)))))
(define-ffi-method (setf ffi-tracking-prevention) (value (buffer gtk-buffer))
  #+webkit2-tracking
  (webkit:webkit-website-data-manager-set-itp-enabled
   (webkit:webkit-web-context-website-data-manager
    (webkit:webkit-web-view-web-context (gtk-object buffer)))
   value))

(define-ffi-method ffi-web-extension-send-message ((buffer gtk-buffer)
                                                   javascript
                                                   result-callback
                                                   error-callback)
  (webkit:webkit-web-view-send-message-to-page
   (gtk-object buffer)
   javascript
   result-callback
   error-callback))

(defmethod ffi-buffer-copy ((gtk-buffer gtk-buffer) &optional (text nil text-provided-p))
  (if text-provided-p
      (trivial-clipboard:text text)
      (let ((channel (nyxt::make-channel 1)))
        (webkit:webkit-web-view-can-execute-editing-command
         (gtk-object gtk-buffer) webkit2:+webkit-editing-command-copy+
         (lambda (can-execute?)
           (if can-execute?
               (progn
                 (webkit:webkit-web-view-execute-editing-command
                  (gtk-object gtk-buffer) webkit2:+webkit-editing-command-copy+)
                 (calispel:! channel t))
               (calispel:! channel nil)))
         (lambda (e) (echo-warning "Cannot copy: ~a" e)))
        (if (calispel:? channel)
            (trivial-clipboard:text)
            nil))))

(defmethod ffi-buffer-paste ((gtk-buffer gtk-buffer) &optional (text nil text-provided-p))
  (webkit:webkit-web-view-can-execute-editing-command
   (gtk-object gtk-buffer) webkit2:+webkit-editing-command-paste+
   (lambda (can-execute?)
     (when can-execute?
       (when text-provided-p
         (trivial-clipboard:text text))
       (webkit:webkit-web-view-execute-editing-command
        (gtk-object gtk-buffer) webkit2:+webkit-editing-command-paste+)))
   (lambda (e) (echo-warning "Cannot paste: ~a" e))))

(defmethod ffi-buffer-cut ((gtk-buffer gtk-buffer))
  (let ((channel (nyxt::make-channel 1)))
    (webkit:webkit-web-view-can-execute-editing-command
     (gtk-object gtk-buffer) webkit2:+webkit-editing-command-cut+
     (lambda (can-execute?)
       (if can-execute?
           (progn
             (webkit:webkit-web-view-execute-editing-command
              (gtk-object gtk-buffer) webkit2:+webkit-editing-command-cut+)
             (calispel:! channel t))
           (calispel:! channel nil)))
     (lambda (e) (echo-warning "Cannot cut: ~a" e)))
    (if (calispel:? channel)
        (trivial-clipboard:text)
        nil)))

(defmethod ffi-buffer-select-all ((gtk-buffer gtk-buffer))
  (webkit:webkit-web-view-can-execute-editing-command
   (gtk-object gtk-buffer) webkit2:+webkit-editing-command-select-all+
   (lambda (can-execute?)
     (when can-execute?
       (webkit:webkit-web-view-execute-editing-command
        (gtk-object gtk-buffer) webkit2:+webkit-editing-command-select-all+)))
   (lambda (e) (echo-warning "Cannot select all: ~a" e))))

(defmethod ffi-buffer-undo ((gtk-buffer gtk-buffer))
  (webkit:webkit-web-view-can-execute-editing-command
   (gtk-object gtk-buffer) webkit2:+webkit-editing-command-undo+
   (lambda (can-execute?)
     (when can-execute?
       (webkit:webkit-web-view-execute-editing-command
        (gtk-object gtk-buffer) webkit2:+webkit-editing-command-undo+)))
   (lambda (e) (echo-warning "Cannot undo: ~a" e))))

(defmethod ffi-buffer-redo ((gtk-buffer gtk-buffer))
  (webkit:webkit-web-view-can-execute-editing-command
   (gtk-object gtk-buffer) webkit2:+webkit-editing-command-redo+
   (lambda (can-execute?)
     (when can-execute?
       (webkit:webkit-web-view-execute-editing-command
        (gtk-object gtk-buffer) webkit2:+webkit-editing-command-redo+)))
   (lambda (e) (echo-warning "Cannot redo: ~a" e))))

(defmethod reload-buffer :around (&optional (buffer (prompt
                                                     :prompt "Reload buffer(s)"
                                                     :sources (make-instance 'buffer-source))))
  "Override Nyxt reload with a DWIM one from WebKit."
  ;; FIXME: We use `reload-buffer' on unloaded buffers???
  (if (member (nyxt::status buffer) '(:unloaded))
      (call-next-method)
      (webkit:webkit-web-view-reload (gtk-object buffer))))

(define-command-global force-reload-buffers (&optional (buffer (prompt
                                                                :prompt "Reload buffer(s)"
                                                                :sources (make-instance 'buffer-source))))
  "Reload the BUFFER(s) bypassing renderer CSS cache.
WebKit-specific."
  (webkit:webkit-web-view-reload-bypass-cache (gtk-object buffer)))

(define-class context-source (prompter:source)
  ((prompter:name "Context list")
   (prompter:constructor (sort (delete-duplicates (append (mapcar #'context-name (buffer-list))
                                                          (list +internal+ +default+))
                                                  :test 'equal)
                               'string<)))
  (:export-class-name-p t))

(define-command-global make-buffer-with-context (&rest args
                                                 &key title modes url load-url-p
                                                 context-name)
  "Create a new buffer with a given context (separate cookies and data).
See the `context-name' documentation.
See `make-buffer' for a description of the other arguments."
  (declare (ignorable title modes url load-url-p))
  (setf (getf args :context-name)
        (or context-name
            (prompt1
             :prompt "Choose context"
             :sources (list (make-instance 'prompter:raw-source :name "New context")
                            'context-source))))
  (apply #'make-buffer args))

(defmethod install ((renderer gtk-renderer))
  (flet ((set-superclasses (renderer-class-sym+superclasses)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class (first renderer-class-sym+superclasses)
                                     :direct-superclasses (rest renderer-class-sym+superclasses)
                                     :metaclass 'interface-class))))
    (mapc #'set-superclasses '((renderer-browser gtk-browser)
                               (renderer-window gtk-window)
                               (renderer-buffer gtk-buffer)
                               (nyxt/mode/download:renderer-download gtk-download)
                               (renderer-request-data gtk-request-data )
                               (renderer-scheme gtk-scheme)
                               (nyxt/mode/user-script:renderer-user-style gtk-user-style)
                               (nyxt/mode/user-script:renderer-user-script gtk-user-script)))))

(defmethod uninstall ((renderer gtk-renderer))
  (flet ((remove-superclasses (renderer-class-sym)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class renderer-class-sym
                                     :direct-superclasses '()
                                     :metaclass 'interface-class))))
    (mapc #'remove-superclasses '(renderer-browser
                                  renderer-window
                                  renderer-buffer
                                  nyxt/mode/download:renderer-download
                                  renderer-request-data
                                  renderer-scheme
                                  nyxt/mode/user-script:renderer-user-style
                                  nyxt/mode/user-script:renderer-user-script))))

(setf nyxt::*renderer* (make-instance 'gtk-renderer))

(defmethod browser-schemes append ((browser gtk-browser))
  '("webkit" "webkit-pdfjs-viewer"))
