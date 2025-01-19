;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/renderer/gtk
    (:documentation "GTK renderer using direct CFFI bindings."))
(in-package :nyxt/renderer/gtk)

(push :nyxt-gtk *features*)

(define-class gtk-renderer (renderer)
  ((name "GTK"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "WebKit renderer class."))

(defmethod renderer-thread-p ((renderer gtk-renderer) &optional (thread (bt:current-thread)))
  (string= "cl-cffi-gtk main thread" (bt:thread-name thread)))

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
                               (renderer-request-data gtk-request-data)
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

(define-class gtk-browser ()
  ((web-contexts
    (make-hash-table :test 'equal)
    :export nil
    :documentation "A table mapping strings to `webkit-web-context' objects."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "WebKit browser class."))

(defmethod get-web-context ((browser gtk-browser) name)
  (alexandria:ensure-gethash name
                             (web-contexts browser)
                             (make-web-context)))

(defmethod browser-schemes append ((browser gtk-browser))
  '("webkit" "webkit-pdfjs-viewer"))

(define-class gtk-window ()
  ((gtk-object
    :export nil)
   (handler-ids
    :documentation "Store all GObject signal handler IDs so that we can
disconnect the signal handler when the object is finalized.")
   (root-box-layout)
   (horizontal-box-layout)
   (main-buffer-container)
   (prompt-buffer-container)
   (prompt-buffer-view
    :documentation "A web view shared by all prompt buffers of this window.
This is done so that the UI is computed efficiently.")
   (status-container)
   (message-container)
   (key-string-buffer))
  (:export-class-name-p t)
  (:export-accessor-names-p nil)
  (:documentation "WebKit window class."))

(define-class gtk-buffer ()
  ((gtk-object)
   (modifier-plist
    '(:control-mask "control"
      :mod1-mask "meta"
      :mod5-mask nil
      :shift-mask "shift"
      :super-mask "super"
      :hyper-mask "hyper"
      :meta-mask nil
      :lock-mask nil)
    :type list
    :documentation "A map between GTK's and Nyxt's terminology for modifier keys.
Note that by changing the default value, modifier keys can be remapped.")
   (handler-ids
    :export nil
    :documentation "Store all GObject signal handler IDs so that we can
disconnect the signal handler when the object is finalized.")
   (gtk-proxy-url (quri:uri ""))
   (proxy-ignored-hosts '())
   (loading-renderer-history-p
    nil
    :type boolean
    :export nil
    :documentation "Internal hack, do not use me!  WebKitGTK may trigger
'load-failed' when loading a page from the WebKit-history cache.  Upstream bug?
We use this slot to know when to ignore these load failures.")
   (handle-permission-requests-p
    nil
    :documentation "Whether permission requests are handled.
When non-nil, they are handled by `process-permission-request'.  Otherwise, all
requests are denied."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "WebKit buffer class."))

(defmethod input-modifier-translator ((buffer gtk-buffer) input-event-modifier-state)
  "Return a list of modifier keys understood by `keymaps:make-key'."
  (when-let ((state input-event-modifier-state))
    (delete nil
            (mapcar (lambda (modifier) (getf (modifier-plist buffer) modifier)) state))))

(defclass webkit-website-data-manager (webkit:webkit-website-data-manager) ()
  (:metaclass gobject:gobject-class))

(defvar gtk-running-p nil
  "Non-nil if the GTK main loop is running.
See `ffi-initialize' and `ffi-kill-browser'.

Restarting GTK within the same Lisp image breaks WebKitGTK.
As a workaround, we never leave the GTK main loop when running from a REPL.

See https://github.com/atlas-engineer/nyxt/issues/740")

(defmacro within-gtk-thread (&body body)
  "Protected `gtk:within-gtk-thread'."
  `(gtk:within-gtk-thread
     (with-protect ("Error on GTK thread: ~a" :condition)
       ,@body)))

(defmethod ffi-within-renderer-thread (thunk)
  (within-gtk-thread (funcall thunk)))

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
  "gtk:within-main-loop handles all the GTK initialization."
  (declare (ignore urls startup-timestamp))
  (log:debug "Initializing GTK Interface")
  (if gtk-running-p
      (within-gtk-thread (call-next-method))
      (progn
        (setf gtk-running-p t)
        (glib:g-set-prgname "nyxt")
        (gdk:gdk-set-program-class "Nyxt")
        (gtk:within-main-loop
          (with-protect ("Error on GTK thread: ~a" :condition)
            (call-next-method)))
        (unless nyxt::*run-from-repl-p*
          (gtk:join-gtk-main)
          (uiop:quit (nyxt:exit-code browser) #+bsd nil)))))

(define-ffi-method ffi-kill-browser ((browser gtk-browser))
  ;; TODO: Terminating the GTK thread from the REPL seems to prevent Nyxt from
  ;; starting again.
  (unless nyxt::*run-from-repl-p* (gtk:leave-gtk-main)))

(define-class gtk-extensions-directory (nyxt-file)
  ((files:name "gtk-extensions")
   (files:base-path (uiop:merge-pathnames* "nyxt/" nasdf:*libdir*)))
  (:export-class-name-p t)
  (:documentation "Directory to load WebKitWebExtensions from."))

(define-class gtk-download ()
  ((gtk-object)
   (handler-ids
    :export nil
    :documentation "See `gtk-buffer' slot of the same name."))
  (:documentation "WebKit download class."))

(defun make-web-context ()
  (let* ((context (make-instance 'webkit:webkit-web-context
                                 :website-data-manager
                                 (make-instance 'webkit-website-data-manager)))
         (cookie-manager (webkit:webkit-web-context-get-cookie-manager context))
         (gtk-extensions-path (files:expand (make-instance 'gtk-extensions-directory))))
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
    (maphash (lambda (scheme-name callbacks)
               (ffi-register-custom-scheme (make-instance 'scheme
                                                          :name scheme-name
                                                          :web-context context
                                                          :callback (first callbacks)
                                                          :error-callback (second callbacks))))
             nyxt::*schemes*)
    (webkit:webkit-cookie-manager-set-persistent-storage
     cookie-manager
     (uiop:native-namestring (files:expand (make-instance 'nyxt-data-directory
                                                          :base-path "cookies")))
     :webkit-cookie-persistent-storage-text)
    (setf (ffi-buffer-cookie-policy cookie-manager) (default-cookie-policy *browser*))
    context))

(define-class gtk-request-data ()
  ((gtk-request
    :type (maybe webkit:webkit-uri-request))
   (gtk-response
    :type (maybe webkit:webkit-uri-response))
   (gtk-resource
    :type (maybe webkit:webkit-web-resource)))
  (:export-class-name-p t)
  ;; We export these accessors because it can be useful to inspect the guts of a
  ;; request, plus the upstream WebKit API is stable enough.
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Related to WebKit's request objects."))

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
                  main-buffer-container
                  prompt-buffer-container prompt-buffer-view
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
                                              :orientation :vertical))
         (setf horizontal-box-layout (make-instance 'gtk:gtk-box
                                                    :orientation :horizontal))
         (setf main-buffer-container (make-instance 'gtk:gtk-box
                                                    :orientation :vertical))
         (setf prompt-buffer-container (make-instance 'gtk:gtk-box
                                                      :orientation :vertical))
         (setf message-container (make-instance 'gtk:gtk-box
                                                :orientation :vertical))
         (setf status-container (make-instance 'gtk:gtk-box
                                               :orientation :vertical))
         (setf key-string-buffer (make-instance 'gtk:gtk-entry))
         (gtk:gtk-box-pack-start horizontal-box-layout
                                 main-buffer-container
                                 :expand t :fill t)
         (gtk:gtk-box-pack-start root-box-layout
                                 horizontal-box-layout
                                 :expand t :fill t)
         (gtk:gtk-box-pack-end root-box-layout
                               message-container
                               :expand nil)
         (gtk:gtk-box-pack-start root-box-layout
                                 message-container
                                 :expand nil)
         (gtk:gtk-box-pack-start message-container
                                 (gtk-object message-buffer)
                                 :expand t)
         (setf (gtk:gtk-widget-height-request message-container)
               (height message-buffer))
         (gtk:gtk-box-pack-end root-box-layout
                               status-container
                               :expand nil)
         (gtk:gtk-box-pack-start status-container
                                 (gtk-object status-buffer)
                                 :expand t)
         (setf (gtk:gtk-widget-height-request status-container)
               (height status-buffer))
         (setf prompt-buffer-view (make-instance 'webkit:webkit-web-view))
         (gtk:gtk-box-pack-end root-box-layout
                               prompt-buffer-container
                               :expand nil)
         (gtk:gtk-box-pack-start prompt-buffer-container
                                 prompt-buffer-view
                                 :expand t)
         (gtk:gtk-container-add gtk-object root-box-layout)
         (connect-signal window "destroy" nil (widget)
           (declare (ignore widget))
           (on-signal-destroy window))
         (connect-signal window "window-state-event" nil (widget event)
           (declare (ignore widget))
           (let ((fullscreen-p)
                 (maximized-p))
             (dolist (state (gdk:gdk-event-window-state-new-window-state event))
               (case state
                 (:fullscreen
                  (setq fullscreen-p t)
                  (ffi-window-fullscreen window :user-event-p nil))
                 (:maximized
                  (setq maximized-p t)
                  (ffi-window-maximize window :user-event-p nil))))
             (unless fullscreen-p (ffi-window-unfullscreen window :user-event-p nil))
             (unless maximized-p (ffi-window-unmaximize window :user-event-p nil)))
           nil))
       (unless *headless-p* (gtk:gtk-widget-show-all gtk-object))))))

(defmethod update-instance-for-redefined-class :after ((window window) added deleted plist &key)
  (declare (ignore added deleted plist))
  (customize-instance window))

(define-ffi-method on-signal-destroy ((window gtk-window))
  ;; Then remove buffer from window container to avoid corruption of buffer.
  (gtk:gtk-container-remove (main-buffer-container window)
                            (gtk-object (active-buffer window))))

(define-ffi-method ffi-window-delete ((window gtk-window))
  (gtk:gtk-widget-destroy (gtk-object window)))

(define-ffi-method ffi-window-fullscreen ((window gtk-window) &key &allow-other-keys)
  (gtk:gtk-window-fullscreen (gtk-object window)))

(define-ffi-method ffi-window-unfullscreen ((window gtk-window) &key &allow-other-keys)
  (gtk:gtk-window-unfullscreen (gtk-object window)))

(define-ffi-method ffi-window-maximize ((window gtk-window) &key &allow-other-keys)
  (gtk:gtk-window-maximize (gtk-object window)))

(define-ffi-method ffi-window-unmaximize ((window gtk-window) &key &allow-other-keys)
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
          (" " "space")
          ("-" "hyphen")
          (character character))
      (setf (gtk:gtk-entry-text (key-string-buffer window)) ""))))

(define-ffi-method on-signal-key-press-event ((sender gtk-buffer) event)
  (let* ((keycode (gdk:gdk-event-key-hardware-keycode event))
         (keyval (gdk:gdk-event-key-keyval event))
         (keyval-name (gdk:gdk-keyval-name keyval))
         (character (gdk:gdk-keyval-to-unicode keyval))
         (printable-value (printable-p (current-window) event))
         (key-string (or printable-value
                         (derive-key-string keyval-name character)))
         (modifiers (input-modifier-translator sender (gdk:gdk-event-key-state event))))
    (log:debug sender key-string keycode character keyval-name modifiers)
    ;; Do not forward modifier-only presses to the renderer.
    (if key-string
        (flet ((key ()
                 (keymaps:make-key :code keycode
                                   :value key-string
                                   :modifiers modifiers
                                   :status :pressed)))
          (alex:appendf (key-stack sender)
                        (list (key)))
          (run-thread "on-signal-key-press"
            (on-signal-key-press sender (key)))
          (dispatch-input-event event sender))
        t)))

(define-ffi-method on-signal-button-press-event ((sender gtk-buffer) event)
  (let ((key-string (format nil "button~s" (gdk:gdk-event-button-button event)))
        (modifiers (input-modifier-translator sender (gdk:gdk-event-button-state event)))
        (buffer (or (current-prompt-buffer) sender)))
    ;; Handle mode-specific logic here (e.g. VI switch to insertion) to not
    ;; interfere with regular keybinding logic.
    (flet ((key ()
             ;; It's a function so we instantiate multiple objects and avoid sharing.
             (keymaps:make-key :value key-string
                               :modifiers modifiers
                               :status :pressed)))
      (run-thread "on-signal-button-press"
        (on-signal-button-press buffer (key)))
      (when key-string
        (alex:appendf (key-stack buffer)
                      (list (key)))
        (dispatch-input-event event sender)))))

(define-ffi-method on-signal-scroll-event ((sender gtk-buffer) event)
  (let* ((button (match (gdk:gdk-event-scroll-direction event)
                   (:up 4)
                   (:down 5)
                   (:left 6)
                   (:right 7)
                   (:smooth (cond ((>= 0 (gdk:gdk-event-scroll-delta-y event)) 4)
                                  ((< 0 (gdk:gdk-event-scroll-delta-y event)) 5)
                                  ((>= 0 (gdk:gdk-event-scroll-delta-x event)) 6)
                                  ((< 0 (gdk:gdk-event-scroll-delta-x event)) 7)))))
         (key-string (format nil "button~s" button))
         (modifiers (input-modifier-translator sender (gdk:gdk-event-scroll-state event))))
    (when key-string
      (alex:appendf (key-stack sender)
                    (list (keymaps:make-key :value key-string
                                            :modifiers modifiers
                                            :status :pressed)))
      (dispatch-input-event event sender))))

(define-class gtk-scheme ()
  ((web-context
    nil
    :writer nil
    :reader t
    :documentation "See `webkit-web-context'.")
   (local-p
    nil
    :writer nil
    :reader t
    :documentation "Whether pages of other URI schemes cannot access URIs of
this scheme.")
   (no-access-p
    nil
    :writer nil
    :reader t
    :documentation "Whether pages of this URI scheme cannot access other URI schemes.")
   (secure-p
    nil
    :writer nil
    :reader t
    :documentation "Whether mixed content warnings aren't generated for this
scheme when included by an HTTPS page.

See https://developer.mozilla.org/en-US/docs/Web/Security/Mixed_content.")
   (cors-enabled-p
    nil
    :writer nil
    :reader t
    :documentation "Whether CORS requests are allowed.")
   (display-isolated-p
    nil
    :writer nil
    :reader t
    :documentation "Whether pages cannot display URIs unless they are from the
same scheme.
For example, pages in another origin cannot create iframes or hyperlinks to URIs
with this scheme.")
   (empty-document-p
    nil
    :writer nil
    :reader t
    :documentation "Whether pages are allowed to be loaded synchronously."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Related to WebKit's custom schemes."))

(defmethod manager ((scheme gtk-scheme))
  (webkit:webkit-web-context-get-security-manager (web-context scheme)))

(defmethod (setf local-p) (value (scheme gtk-scheme))
  (when value
    (webkit:webkit-security-manager-register-uri-scheme-as-local (manager scheme)
                                                                 (name scheme)))
  (setf (slot-value scheme 'local-p) value))

(defmethod (setf no-access-p) (value (scheme gtk-scheme))
  (when value
    (webkit:webkit-security-manager-register-uri-scheme-as-no-access (manager scheme)
                                                                     (name scheme)))
  (setf (slot-value scheme 'no-access-p) value))

(defmethod (setf secure-p) (value (scheme gtk-scheme))
  (when value
    (webkit:webkit-security-manager-register-uri-scheme-as-secure (manager scheme)
                                                                  (name scheme)))
  (setf (slot-value scheme 'secure-p) value))

(defmethod (setf cors-enabled-p) (value (scheme gtk-scheme))
  (when value
    (webkit:webkit-security-manager-register-uri-scheme-as-cors-enabled (manager scheme)
                                                                        (name scheme)))
  (setf (slot-value scheme 'cors-enabled-p) value))

(defmethod (setf display-isolated-p) (value (scheme gtk-scheme))
  (when value
    (webkit:webkit-security-manager-register-uri-scheme-as-display-isolated (manager scheme)
                                                                            (name scheme)))
  (setf (slot-value scheme 'display-isolated-p) value))

(defmethod (setf empty-document-p) (value (scheme gtk-scheme))
  (when value
    (webkit:webkit-security-manager-register-uri-scheme-as-empty-document (manager scheme)
                                                                          (name scheme)))
  (setf (slot-value scheme 'empty-document-p) value))

(defmethod initialize-instance :after ((scheme gtk-scheme) &key)
  ;; NOTE: No security settings for the nyxt scheme since:
  ;; - :local-p makes it inaccessible from other schemes.
  ;; - :display-isolated-p does not allow embedding a nyxt scheme page inside a
  ;;   page of the same scheme.
  ;; - :secure-p and :cors-enabled-p are too permissive for a scheme that allows
  ;;   evaluating Lisp code.
  ;; Therefore, no settings provide the best configuration so that:
  ;; - <iframe> embedding and exploitation are impossible.
  ;; - Redirection—both as window.location.(href|assign|replace) and
  ;;   HTTP status code 301—works.
  ;; - nyxt scheme pages are linkable from pages of other schemes.
  (match (name scheme)
    ("nyxt-resource" (setf (secure-p scheme) t))
    ("lisp" (setf (cors-enabled-p scheme) t))
    ("view-source" (setf (no-access-p scheme) t))
    (_ t)))

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

(defmethod ffi-register-custom-scheme ((scheme gtk-scheme))
  ;; FIXME If a define-internal-scheme is updated at runtime, it is not honored.
  (webkit:webkit-web-context-register-uri-scheme-callback
   (web-context scheme)
   (name scheme)
   (lambda (request)
     (funcall* (callback scheme)
               (webkit:webkit-uri-scheme-request-get-uri request)))
   (or (error-callback scheme)
       (lambda (c) (echo-warning "Error while routing ~s resource: ~a" scheme c)))))

(defmethod customize-instance :after ((buffer gtk-buffer) &key extra-modes
                                                            no-hook-p
                                      &allow-other-keys)
  "Make BUFFER with EXTRA-MODES.
See `finalize-buffer'."
  (ffi-buffer-initialize-foreign-object buffer)
  (finalize-buffer buffer :extra-modes extra-modes :no-hook-p no-hook-p))

(define-ffi-method ffi-buffer-url ((buffer gtk-buffer))
  (quri:uri (webkit:webkit-web-view-uri (gtk-object buffer))))

(define-ffi-method ffi-buffer-title ((buffer gtk-buffer))
  (or (webkit:webkit-web-view-title (gtk-object buffer)) ""))

(define-ffi-method on-signal-load-failed-with-tls-errors ((buffer gtk-buffer) certificate url)
  "Return nil to propagate further (i.e. raise load-failed signal), T otherwise."
  (let ((context (webkit:webkit-web-view-web-context (gtk-object buffer)))
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
       (setf navigation-type
             (webkit:webkit-navigation-policy-decision-navigation-type response-policy-decision)))
      (:webkit-policy-decision-type-new-window-action
       (setf navigation-type
             (webkit:webkit-navigation-policy-decision-navigation-type response-policy-decision))
       (setf is-new-window t))
      (:webkit-policy-decision-type-response
       (setf request
             (webkit:webkit-response-policy-decision-request response-policy-decision))
       (setf is-known-type
             (webkit:webkit-response-policy-decision-is-mime-type-supported
              response-policy-decision))
       (setf response
             (webkit:webkit-response-policy-decision-response response-policy-decision))
       (setf mime-type
             (webkit:webkit-uri-response-mime-type response))
       (setf method
             (webkit:webkit-uri-request-get-http-method request))
       (setf file-name
             (webkit:webkit-uri-response-suggested-filename response))))
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
      (setf navigation-action
            (webkit:webkit-navigation-policy-decision-get-navigation-action
             response-policy-decision))
      (setf request
            (webkit:webkit-navigation-action-get-request navigation-action))
      (setf mouse-button
            (format nil "button~d"
                    (webkit:webkit-navigation-action-get-mouse-button navigation-action)))
      (setf modifiers
            (input-modifier-translator buffer
                                 (webkit:webkit-navigation-action-get-modifiers navigation-action))))
    (setf url (quri:uri (webkit:webkit-uri-request-uri request)))
    (setf request-headers
          (let ((headers (webkit:webkit-uri-request-get-http-headers request)))
            (unless (cffi:null-pointer-p headers)
              (webkit:soup-message-headers-get-headers headers))))
    (setf response-headers
          (when response
            (let ((headers (webkit:webkit-uri-response-get-http-headers response)))
              (unless (cffi:null-pointer-p headers)
                (webkit:soup-message-headers-get-headers headers)))))
    (setf toplevel-p
          (quri:uri= url
                     (quri:uri (webkit:webkit-web-view-uri (gtk-object buffer)))))
    (when toplevel-p (apply-auto-rules url buffer))
    (let* ((request-data
            (hooks:run-hook
             (request-resource-hook buffer)
             (sera:lret ((data (make-instance
                                'request-data
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
        (log:debug "Resource request key sequence ~a" (keyspecs-with-keycode (keys request-data)))
        (funcall bound-function :url url :buffer buffer)
        (webkit:webkit-policy-decision-ignore response-policy-decision))
       ((new-window-p request-data)
        (log:debug "Load URL in new buffer: ~a" (render-url (url request-data)))
        (nyxt::open-urls (list (url request-data)))
        (webkit:webkit-policy-decision-ignore response-policy-decision))
       ((null (valid-scheme-p (quri:uri-scheme (url request-data))))
        (log:warn "Unsupported URI scheme: ~s." (quri:uri-scheme (url request-data))))
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
             (on-signal-load-started buffer url)
             (unless (internal-url-p url)
               (echo "Loading ~s." (render-url url))))
            ((eq load-event :webkit-load-redirected)
             (setf (url buffer) url)
             (on-signal-load-redirected buffer url))
            ((eq load-event :webkit-load-committed)
             (on-signal-load-committed buffer url))
            ((eq load-event :webkit-load-finished)
             (setf (loading-renderer-history-p buffer) nil)
             (unless (eq (slot-value buffer 'nyxt::status) :failed)
               (setf (nyxt::status buffer) :finished))
             (on-signal-load-finished buffer url)
             (unless (internal-url-p url)
               (echo "Finished loading ~s." (render-url url))))))))

(define-ffi-method on-signal-mouse-target-changed ((buffer gtk-buffer)
                                                   hit-test-result
                                                   modifiers)
  (declare (ignore modifiers))
  (if-let ((url (or (webkit:webkit-hit-test-result-link-uri hit-test-result)
                    (webkit:webkit-hit-test-result-image-uri hit-test-result)
                    (webkit:webkit-hit-test-result-media-uri hit-test-result))))
    (progn
      (nyxt::print-message (str:concat "→ " (render-url url)))
      (setf (url-at-point buffer) (quri:uri url)))
    (progn
      (nyxt::print-message "")
      (setf (url-at-point buffer) (quri:uri "")))))

(define-ffi-method ffi-window-to-foreground ((window gtk-window))
  "Show window in foreground."
  (unless *headless-p* (gtk:gtk-window-present (gtk-object window)))
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
  (when-let ((buried-buffer (gtk-object (active-buffer window))))
    ;; Just a precaution for the buffer to not be destroyed until we say so.
    (g:g-object-ref (g:pointer buried-buffer))
    (gtk:gtk-container-remove (main-buffer-container window) buried-buffer))
  (gtk:gtk-box-pack-start (main-buffer-container window)
                          (gtk-object buffer)
                          :expand t :fill t)
  (unless *headless-p* (gtk:gtk-widget-show (gtk-object buffer)))
  (when focus (gtk:gtk-widget-grab-focus (gtk-object buffer))))

(define-ffi-method ffi-height ((buffer prompt-buffer))
  (gtk:gtk-widget-height-request (prompt-buffer-container (window buffer))))
(define-ffi-method (setf ffi-height) ((height integer) (buffer prompt-buffer))
  (setf (gtk:gtk-widget-height-request (prompt-buffer-container (window buffer)))
        height))

(define-ffi-method ffi-focus-prompt-buffer ((prompt-buffer prompt-buffer))
  "Focus PROMPT-BUFFER in WINDOW."
  (gtk:gtk-widget-grab-focus (gtk-object prompt-buffer))
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
                  (let* ((multiple (webkit:webkit-file-chooser-request-select-multiple
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
                                  :yes "leave" :no "stay")))))
          (webkit:webkit-script-dialog-close dialog)
          (webkit:webkit-script-dialog-unref dialog))
        t))))

(defun process-permission-request (web-view request)
  (g:g-object-ref (g:pointer request))
  (run-thread "permission requester"
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
        (webkit:webkit-permission-request-deny request))))

(defun process-notification (web-view notification)
  (when (native-dialogs *browser*)
    (let* ((title (webkit:webkit-notification-get-title notification))
           (body (webkit:webkit-notification-get-body notification)))
      (echo "[~a] ~a: ~a" (webkit:webkit-web-view-uri web-view) title body)
      t)))

(define-ffi-method ffi-buffer-initialize-foreign-object ((buffer gtk-buffer))
  "Initialize BUFFER's GTK web view."
  (setf (gtk-object buffer)
        (if (prompt-buffer-p buffer)
            ;; A single web view is shared by all prompt buffers of a window.
            (prompt-buffer-view (window buffer))
            (make-instance 'webkit:webkit-web-view
                           :web-context (get-web-context *browser* "default"))))
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
           (request-data (make-instance
                          'request-data
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
  (connect-signal buffer "key_press_event" nil (widget event)
    (declare (ignore widget))
    (on-signal-key-press-event buffer event))
  (connect-signal buffer "scroll-event" nil (web-view event)
    (declare (ignore web-view))
    (on-signal-scroll-event buffer event))
  (connect-signal-function buffer "script-dialog" #'process-script-dialog)
  (connect-signal-function buffer "run-file-chooser" #'process-file-chooser-request)
  (connect-signal-function buffer "run-color-chooser" #'process-color-chooser-request)
  (when (handle-permission-requests-p buffer)
    (connect-signal-function buffer "permission-request" #'process-permission-request))
  (connect-signal-function buffer "show-notification" #'process-notification)
  ;; TLS certificate handling
  (connect-signal buffer "load-failed-with-tls-errors" nil (web-view failing-url certificate errors)
    (declare (ignore web-view errors))
    (on-signal-load-failed buffer (quri:uri failing-url))
    (on-signal-load-failed-with-tls-errors buffer certificate (quri:uri failing-url)))
  (connect-signal buffer "notify::uri" nil (web-view param-spec)
    (declare (ignore web-view param-spec))
    (on-signal-notify-uri buffer nil))
  (connect-signal buffer "notify::title" nil (web-view param-spec)
    (declare (ignore web-view param-spec))
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
    (log:debug "Closed ~a" buffer))
  (connect-signal buffer "load-failed" nil (web-view load-event failing-url error)
    (declare (ignore load-event web-view))
    ;; TODO: WebKitGTK sometimes (when?) triggers "load-failed" when loading a
    ;; page from the renderer-history cache.  Upstream bug?  Anyways, we should
    ;; ignore these.
    (on-signal-load-failed buffer (quri:uri failing-url))
    (cond
      ((loading-renderer-history-p buffer)
       (setf (loading-renderer-history-p buffer) nil))
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
                          ;; Using stock actions here, because cl-cffi-gtk has a
                          ;; terrible API for GActions, requiring an exact type
                          ;; to be passed and disallowing NULL as a type.
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
    (ffi-window-fullscreen (current-window) :user-event-p nil)
    ;; As to account for JS's Fullscreen API.
    (disable-message-buffer (current-window))
    (disable-status-buffer (current-window))
    nil)
  (connect-signal buffer "leave-fullscreen" nil (web-view)
    (declare (ignore web-view))
    (ffi-window-unfullscreen (current-window) :user-event-p nil)
    ;; Ideally, the UI state prior to fullscreen must be recovered.
    (enable-message-buffer (current-window))
    (enable-status-buffer (current-window))
    nil)
  buffer)

(define-ffi-method ffi-buffer-delete ((buffer gtk-buffer))
  (with-slots (gtk-object handler-ids) buffer
    (webkit:webkit-web-view-try-close gtk-object)
    (mapc (lambda (id) (gobject:g-signal-handler-disconnect gtk-object id))
          handler-ids)
    (nyxt::buffer-hide buffer)
    (unless (prompt-buffer-p buffer) (gtk:gtk-widget-destroy gtk-object))
    (setf gtk-object nil)
    (when (prompt-buffer-p buffer) (setf (ffi-height buffer) 0))))

(define-ffi-method ffi-buffer-load ((buffer gtk-buffer) url)
  "Load URL in BUFFER.
An optimization technique is to make use of the renderer history cache.
For WebKit, if the URL matches an entry in the renderer-history then we fetch the
page from the cache.

We don't use the cache if URL matches BUFFER's URL since this means the user
requested a reload.

Note that we don't use the cache for internal pages (say nyxt:help) since it's
local anyways, and it's better to refresh it if a load was queried."
  (declare (type quri:uri url))
  (let* ((history (renderer-history buffer))
         (entry (or (find url history :test #'quri:uri= :key #'renderer-history-entry-url)
                    (find url history :test #'quri:uri= :key #'renderer-history-entry-original-url))))
    ;; Mark buffer as :loading right away so functions like
    ;; `ffi-window-set-buffer' don't try to reload if they are called before the
    ;; "load-changed" signal is emitted.
    (when (web-buffer-p buffer)
      (setf (nyxt::status buffer) :loading))
    (if (and entry
             (not (internal-url-p url))
             (not (quri:uri= url (url buffer))))
        (progn
          (log:debug "Load URL from history entry ~a" entry)
          (load-renderer-history-entry buffer entry))
        (webkit:webkit-web-view-load-uri (gtk-object buffer) (quri:render-uri url)))))

(define-ffi-method ffi-buffer-reload ((buffer gtk-buffer))
  "Reload BUFFER."
  (when (web-buffer-p buffer)
    (setf (nyxt::status buffer) :loading))
  (webkit:webkit-web-view-reload (gtk-object buffer)))

(define-ffi-method ffi-buffer-load-alternate-html ((buffer gtk-buffer)
                                                   html-content
                                                   content-url
                                                   url)
  (webkit:webkit-web-view-load-alternate-html (gtk-object buffer)
                                              html-content
                                              (quri:render-uri (url content-url))
                                              (if (uiop:emptyp url) "about:blank" url)))

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
  (:export-accessor-names-p t)
  (:documentation "Related to WebKit's user style sheets."))

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
  (webkit:webkit-user-content-manager-remove-style-sheet
   (webkit:webkit-web-view-get-user-content-manager (gtk-object buffer))
   (gtk-object style)))

(define-class gtk-user-script ()
  ((gtk-object))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:documentation "Related to WebKitUserScript."))

(define-ffi-method ffi-buffer-add-user-script ((buffer gtk-buffer) (script gtk-user-script))
  (if-let ((code (nyxt/mode/user-script:code script)))
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
(define-ffi-settings-accessor media-enabled-p webkit:webkit-settings-enable-media)
(define-ffi-settings-accessor webgl-enabled-p webkit:webkit-settings-enable-webgl)
(define-ffi-settings-accessor auto-load-image-enabled-p webkit:webkit-settings-auto-load-images)

(defmethod ffi-buffer-sound-enabled-p ((buffer gtk-buffer))
  (not (webkit:webkit-web-view-get-is-muted (gtk-object buffer))))
(defmethod (setf ffi-buffer-sound-enabled-p) (value (buffer gtk-buffer))
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
      (when-let* ((suggested-file-name (maybe-fix-pdfjs-filename suggested-file-name original-url))
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
  (when-let ((settings (webkit:webkit-web-view-get-settings (gtk-object buffer))))
    (webkit:webkit-settings-user-agent settings)))

(define-ffi-method (setf ffi-buffer-user-agent) (value (buffer gtk-buffer))
  (when-let ((settings (webkit:webkit-web-view-get-settings (gtk-object buffer))))
    (setf (webkit:webkit-settings-user-agent settings) value)))

(define-ffi-method ffi-buffer-proxy ((buffer gtk-buffer))
  "Return the proxy URL and list of ignored hosts (a list of strings) as second value."
  (the (values (or quri:uri null) (list-of string))
       (values (gtk-proxy-url buffer)
               (proxy-ignored-hosts buffer))))
(define-ffi-method (setf ffi-buffer-proxy) (proxy-specifier (buffer gtk-buffer))
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
              (webkit:webkit-network-proxy-settings-new (render-url proxy-url)
                                                        ignore-hosts)))
      (cffi:foreign-free ignore-hosts)
      (webkit:webkit-web-context-set-network-proxy-settings context
                                                            mode
                                                            settings))))

(define-ffi-method ffi-buffer-zoom-level ((buffer gtk-buffer))
  (webkit:webkit-web-view-zoom-level (gtk-object buffer)))
(define-ffi-method (setf ffi-buffer-zoom-level) (value (buffer gtk-buffer))
  (when (and (floatp value) (>= value 0))
    (setf (webkit:webkit-web-view-zoom-level (gtk-object buffer)) value)))

(define-ffi-method ffi-inspector-show ((buffer gtk-buffer))
  (webkit:webkit-web-inspector-show
   (webkit:webkit-web-view-get-inspector (gtk-object buffer))))

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

(defstruct renderer-history-entry
  title
  url
  original-url
  gtk-object)

(define-ffi-method renderer-history ((buffer gtk-buffer))
  "Return a list of `renderer-history-entry's for the current buffer.
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
          do (push (make-renderer-history-entry
                    :title (webkit:webkit-back-forward-list-item-get-title item)
                    :url (quri:uri (webkit:webkit-back-forward-list-item-get-uri item))
                    :original-url (quri:uri (webkit:webkit-back-forward-list-item-get-original-uri item))
                    :gtk-object item)
                   history-list))
    (values history-list current-index)))

(defmethod load-renderer-history-entry ((buffer gtk-buffer) history-entry)
  (setf (loading-renderer-history-p buffer) t)
  (webkit:webkit-web-view-go-to-back-forward-list-item
   (gtk-object buffer)
   (renderer-history-entry-gtk-object history-entry)))

(define-ffi-method ffi-focused-p ((buffer gtk-buffer))
  (gtk:gtk-widget-is-focus (gtk-object buffer)))

(defmethod itp-enabled-p ((buffer gtk-buffer))
  "Return non-nil when Intelligent Tracking Prevention is enabled."
  (webkit:webkit-website-data-manager-get-itp-enabled
   (webkit:webkit-web-context-website-data-manager
    (webkit:webkit-web-view-web-context (gtk-object buffer)))))
(defmethod (setf itp-enabled-p) (value (buffer gtk-buffer))
  (webkit:webkit-website-data-manager-set-itp-enabled
   (webkit:webkit-web-context-website-data-manager
    (webkit:webkit-web-view-web-context (gtk-object buffer)))
   value))

(defmethod enable :after ((mode nyxt/mode/reduce-tracking:reduce-tracking-mode) &key)
  (setf (itp-enabled-p (buffer mode)) t))

(defmethod disable :after ((mode nyxt/mode/reduce-tracking:reduce-tracking-mode) &key)
  (setf (itp-enabled-p (buffer mode)) nil))

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
                 (calispel:! channel t)
                 (echo "~s copied to clipboard." text))
               (calispel:! channel nil)))
         (lambda (e) (echo-warning "~s failed to copy to clipboard." e)))
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
   (lambda (e) (echo-warning "~s failed to paste." e))))

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
