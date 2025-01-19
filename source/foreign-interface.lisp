;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defmacro define-ffi-generic (name arguments &body options)
  "Like `defgeneric' but export NAME and define default dummy method if none is provided.
If the `:setter-p' option is non-nil, then a dummy setf method is defined."
  (let* ((methods (sera:filter (sera:eqs :method) options :key #'first))
         (setter? (assoc-value options :setter-p))
         (normalized-options (set-difference options methods :key #'first))
         (normalized-options (setf (assoc-value normalized-options :setter-p) nil)))
    `(progn
       (export-always ',name)
       (prog1
           (defgeneric ,name (,@arguments)
             ,@(if methods
                   methods
                   `((:method (,@arguments)
                       (declare (ignore ,@(set-difference arguments lambda-list-keywords))))))
             ,@normalized-options)
         ,(when setter?
            `(defmethod (setf ,name) (value ,@arguments)
               (declare (ignore value ,@arguments))))))))

(define-ffi-generic ffi-window-delete (window)
  (:method :around ((window window))
    (with-slots (windows) *browser*
      (cond ((or *quitting-nyxt-p*
                 (> (hash-table-count windows) 1))
             (hooks:run-hook (window-delete-hook window) window)
             (remhash (id window) windows)
             (call-next-method))
            (t
             (echo "Can't delete sole window.")))))
  (:documentation "Delete WINDOW."))

(define-ffi-generic ffi-window-fullscreen (window &key &allow-other-keys)
  (:method :around ((window window) &key (user-event-p t) &allow-other-keys)
    (setf (slot-value window 'fullscreen-p) t)
    (when user-event-p (call-next-method)))
  (:documentation "Set fullscreen WINDOW state on.
USER-EVENT-P helps to distinguish events requested by the user or
renderer (e.g. fullscreen a video stream)."))
(define-ffi-generic ffi-window-unfullscreen (window &key &allow-other-keys)
  (:method :around ((window window) &key (user-event-p t) &allow-other-keys)
    (setf (slot-value window 'fullscreen-p) nil)
    (when user-event-p (call-next-method)))
  (:documentation "Set fullscreen WINDOW state off.
See `ffi-window-fullscreen'."))

(define-ffi-generic ffi-window-maximize (window &key &allow-other-keys)
  (:method :around ((window window) &key (user-event-p t) &allow-other-keys)
    (setf (slot-value window 'maximized-p) t)
    (when user-event-p (call-next-method)))
  (:documentation "Set WINDOW to a maximized state.
USER-EVENT-P helps to distinguish events requested by the user or renderer."))
(define-ffi-generic ffi-window-unmaximize (window &key &allow-other-keys)
  (:method :around ((window window) &key (user-event-p t) &allow-other-keys)
    (setf (slot-value window 'maximized-p) nil)
    (when user-event-p (call-next-method)))
  (:documentation "Set WINDOW to an unmaximized state.
See `ffi-window-maximize'."))

(define-ffi-generic ffi-buffer-url (buffer)
  (:documentation "Return the URL associated with BUFFER as a `quri:uri'.
This is used to set the BUFFER `url' slot."))
(define-ffi-generic ffi-buffer-title (buffer)
  (:documentation "Return a string corresponding to the BUFFER's title."))

(define-ffi-generic ffi-window-make (browser)
  (:method ((browser t))
    (make-instance 'window))
  (:documentation "Return a `window' and display it."))

(define-ffi-generic ffi-window-to-foreground (window)
  (:method ((window t))
    (setf (slot-value *browser* 'last-active-window) window))
  (:documentation "Show WINDOW in the foreground.
The specialized method must invoke `call-next-method' last."))

(define-ffi-generic ffi-window-title (window)
  (:setter-p t)
  (:documentation "Return a string corresponding to the WINDOW's title.
Setf-able."))

(define-ffi-generic ffi-window-active (browser)
  (:method :around ((browser t))
    (setf (slot-value browser 'last-active-window)
          (call-next-method)))
  (:method ((browser t))
    (or (slot-value browser 'last-active-window)
        (first (window-list))))
  (:documentation "Return the focused window.

The specialized method must fallback on the primary method below, as to account
for the case when the renderer reports that none of the windows are focused.

The `:around' method ensures that `last-active-window' is set."))

(define-ffi-generic ffi-window-set-buffer (window buffer &key focus)
  (:method :around ((window window) (buffer buffer) &key focus &allow-other-keys)
    (hooks:run-hook (window-set-buffer-hook window) window buffer)
    ;; As to ensure `current-buffer' returns the right value if
    ;; `ffi-window-set-buffer' is called inside `with-current-buffer'.
    (setf %buffer nil)
    (when focus
      (let ((buried-buffer (active-buffer window))
            (now (time:now)))
        (when (slot-exists-p buried-buffer 'last-access)
          (setf (last-access buried-buffer) now))
        (when (slot-exists-p buffer 'last-access)
          (setf (last-access buffer) now))))
    (when (and (slot-exists-p buffer 'status)
               (eq (slot-value buffer 'status) :unloaded))
      (buffer-load (url buffer) :buffer buffer))
    (call-next-method)
    buffer)
  (:method :after ((window window) (buffer buffer) &key focus &allow-other-keys)
    (declare (ignore focus))
    (setf (active-buffer window) buffer))
  (:documentation "Return BUFFER and display it in WINDOW as a side effect.
Run `window-set-buffer-hook' over WINDOW and BUFFER before proceeding."))

(define-ffi-generic ffi-focus-prompt-buffer (prompt-buffer)
  (:documentation "Return PROMPT-BUFFER and focus it as a side effect."))

(define-ffi-generic ffi-height (object)
  (:setter-p t)
  (:documentation "Return the OBJECT's height in pixels.
Dispatches over `window' and classes inheriting from `buffer'.
Usually setf-able."))
(define-ffi-generic ffi-width (object)
  (:setter-p t)
  (:documentation "Return the OBJECT's width in pixels.
Dispatches over `window' and classes inheriting from `buffer'.
Usually setf-able."))

(define-ffi-generic ffi-buffer-make (browser)
  (:method ((browser t))
    (make-instance 'buffer))
  (:documentation "Return BUFFER and display it."))
(define-ffi-generic ffi-buffer-initialize-foreign-object (buffer)
  (:documentation "Create and configure the foreign object for a given buffer. This differs from
`ffi-buffer-make' because it takes an existing buffer object and creates the
foreign objects necessary for rendering the buffer."))
(define-ffi-generic ffi-buffer-delete (buffer)
  (:documentation "Delete BUFFER."))

(define-ffi-generic ffi-buffer-load (buffer url)
  (:documentation "Load URL into BUFFER through the renderer."))

(define-ffi-generic ffi-buffer-reload (buffer)
  (:documentation "Reload BUFFER via the renderer."))

(define-ffi-generic ffi-buffer-load-alternate-html (buffer html-content content-url url)
  (:documentation "Load HTML-CONTENT for CONTENT-URL into BUFFER through the renderer.
Meant to display page-loading errors."))

(define-ffi-generic ffi-register-custom-scheme (scheme)
  (:documentation "Register internal custom SCHEME.
See `scheme'."))

(define-ffi-generic ffi-buffer-evaluate-javascript (buffer javascript &optional world-name)
  (:documentation "Evaluate JAVASCRIPT, encoded as a string, in BUFFER."))
(define-ffi-generic ffi-buffer-evaluate-javascript-async (buffer javascript &optional world-name)
  (:documentation "Asynchronous version of `ffi-buffer-evaluate-javascript'."))

(define-ffi-generic ffi-buffer-add-user-style (buffer style)
  (:documentation "Apply the CSS style to BUFFER."))
(define-ffi-generic ffi-buffer-remove-user-style (buffer style)
  (:documentation "Remove the STYLE installed with `ffi-buffer-add-user-style'."))

(define-ffi-generic ffi-buffer-add-user-script (buffer user-script)
  (:documentation "Install the JAVASCRIPT into the BUFFER web view."))
(define-ffi-generic ffi-buffer-remove-user-script (buffer script)
  (:documentation "Remove the SCRIPT installed with `ffi-buffer-add-user-script'."))

(define-ffi-generic ffi-buffer-javascript-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return non-nil when JavaScript is enabled in BUFFER.
Setf-able."))
(define-ffi-generic ffi-buffer-javascript-markup-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return non-nil when JavaScript can mutate the BUFFER' contents.
Setf-able."))
(define-ffi-generic ffi-buffer-smooth-scrolling-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return non-nil when smooth scrolling is enabled in BUFFER.
Setf-able."))
(define-ffi-generic ffi-buffer-media-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return non-nil when video and audio playback are enabled in BUFFER.
Setf-able."))
(define-ffi-generic ffi-buffer-webgl-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return non-nil when WebGL is enabled in BUFFER.
Setf-able."))
(define-ffi-generic ffi-buffer-auto-load-image-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return non-nil when images are displayed in BUFFER.
Setf-able."))
(define-ffi-generic ffi-buffer-sound-enabled-p (buffer)
  (:setter-p t)
  (:documentation "Return non-nil when the sound is enabled in BUFFER.
Setf-able."))

(define-ffi-generic ffi-buffer-user-agent (buffer)
  (:setter-p t)
  (:documentation "Return the user agent as a string.
Setf-able."))

(define-ffi-generic ffi-buffer-proxy (buffer)
  (:setter-p t)
  (:documentation "Return the proxy URL as a `quri:uri'.
Return the list of ignored hosts (list of strings) as a second value.

Setf-able.  The value is either a PROXY-URL or a pair of (PROXY-URL IGNORE-HOSTS).
PROXY-URL is a `quri:uri' and IGNORE-HOSTS a list of strings."))

(define-ffi-generic ffi-buffer-download (buffer url)
  (:documentation "Download URL using the BUFFER web view."))

(define-ffi-generic ffi-buffer-zoom-level (buffer)
  (:method ((buffer t))
    (ps-eval :buffer buffer (ps:chain document body style zoom)))
  (:setter-p t)
  (:documentation "Return the zoom level of the document.
Setf-able."))
(defmethod (setf ffi-buffer-zoom-level) (value (buffer buffer))
  "Use JavaScript, if the renderer does not allow zooming natively."
  (ps-eval :buffer buffer
    (ps:let ((style (ps:chain document body style)))
      (setf (ps:@ style zoom)
            (ps:lisp value)))))

(define-ffi-generic ffi-buffer-get-document (buffer)
  (:method ((buffer t))
    (ps-labels :buffer buffer
      ((get-html
        (start end)
        (ps:chain document document-element |innerHTML| (slice (ps:lisp start)
                                                               (ps:lisp end))))
       (get-html-length
        ()
        (ps:chain document document-element |innerHTML| length)))
      (let ((slice-size 10000))
        (reduce #'str:concat
                (loop for i from 0 to (truncate (get-html-length)) by slice-size
                      collect (get-html i (+ i slice-size)))))))
  (:documentation "Return the BUFFER raw HTML as a string."))

(define-ffi-generic ffi-within-renderer-thread (thunk)
  (:method ((thunk t))
    (funcall thunk))
  (:documentation "Run THUNK (a lambda of no argument) in the renderer's thread.
It is particularly useful for renderer procedures required to be executed in
specific threads."))

(define-ffi-generic ffi-kill-browser (browser)
  (:documentation "Terminate the renderer process."))

(define-ffi-generic ffi-initialize (browser urls startup-timestamp)
  (:method ((browser t) urls startup-timestamp)
    (finalize-startup browser urls startup-timestamp))
  (:documentation "Renderer-specific initialization.
A specialization of this method must call `call-next-method' to conclude the
startup routine."))

(define-ffi-generic ffi-inspector-show (buffer)
  (:documentation "Show the renderer built-in inspector."))

(define-ffi-generic ffi-print-status (window html-body)
  (:method ((window t) html-body)
    (with-slots (status-buffer) window
      (html-write (spinneret:with-html-string
                    (:head (:nstyle (style status-buffer)))
                    (:body (:raw html-body)))
                  status-buffer)))
  (:documentation "Display status buffer in WINDOW according to HTML-BODY.
The `style' of the `status-buffer' is honored."))

(define-ffi-generic ffi-print-message (window html-body)
  (:method ((window t) html-body)
    (with-slots (message-buffer) window
      (html-write (spinneret:with-html-string
                    (:head (:nstyle (style message-buffer)))
                    (:body (:raw html-body)))
                  message-buffer)))
  (:documentation "Print HTML-BODY in the WINDOW's message buffer.
The `style' of the `message-buffer' is honored."))

(define-ffi-generic ffi-buffer-cookie-policy (buffer)
  (:setter-p t)
  (:documentation "Return the cookie policy.
Setf-able.  Valid values are determined by the `cookie-policy' type."))

(define-ffi-generic ffi-preferred-languages (buffer)
  (:setter-p t)
  (:documentation "Set the list of preferred languages in the HTTP header \"Accept-Language:\".
Setf-able, where the languages value is a list of strings like '(\"en_US\"
\"fr_FR\")."))

(define-ffi-generic ffi-focused-p (buffer)
  (:documentation "Return non-nil when BUFFER is focused."))

(define-ffi-generic ffi-buffer-copy (buffer &optional text)
  (:method :around ((buffer t) &optional text)
    (declare (ignore text))
    ;; Save the top of clipboard before it's overridden.
    (ring-insert-clipboard (clipboard-ring *browser*))
    (sera:lret ((result (call-next-method)))
      (ring-insert-clipboard (clipboard-ring *browser*))))
  (:method ((buffer t) &optional (text nil text-provided-p))
    (ps-labels :buffer buffer ((copy () (ps:chain window (get-selection) (to-string))))
      ;; On some systems like Xorg, clipboard pasting happens just-in-time.  So if we
      ;; copy something from the context menu 'Copy' action, upon pasting we will
      ;; retrieve the text from the GTK thread.  This is prone to create
      ;; dead-locks (e.g. when executing a Parenscript that acts upon the clipboard).
      ;;
      ;; To avoid this, we can 'flush' the clipboard to ensure that the copied text
      ;; is present the clipboard and need not be retrieved from the GTK thread.
      ;; TODO: Do we still need to flush now that we have multiple threads?
      ;; (trivial-clipboard:text (trivial-clipboard:text))

      (sera:lret ((input (if text-provided-p text (copy))))
        (copy-to-clipboard input)
        (echo "~s copied to clipboard." input))))
  (:documentation "Copy selected text in BUFFER to the system clipboard.
If TEXT is provided, add it to system clipboard instead of selected text.
Should return the copied text or NIL, if something goes wrong."))

(define-ffi-generic ffi-buffer-paste (buffer &optional text)
  ;; While it may sound counterintuitive, it helps to keep track of the system
  ;; clipboard, both in Nyxt->OS and OS->Nyxt directions.
  (:method :around ((buffer t) &optional text)
    (declare (ignore text))
    ;; Save the top of clipboard before it's overridden.
    (ring-insert-clipboard (clipboard-ring *browser*))
    (sera:lret ((result (call-next-method)))
      (ring-insert-clipboard (clipboard-ring *browser*))))
  (:method ((buffer t) &optional (text nil text-provided-p))
    (ps-labels :buffer buffer
      ((paste
        (&optional (input-text (ring-insert-clipboard (clipboard-ring *browser*))))
        (let ((active-element (nyxt/ps:active-element document))
              (tag (ps:@ (nyxt/ps:active-element document) tag-name))
              (text-to-paste (or (ps:lisp input-text)
                                 (ps:chain navigator clipboard (read-text)))))
          (when (nyxt/ps:element-editable-p active-element)
            (nyxt/ps:insert-at active-element text-to-paste))
          text-to-paste)))
      (if text-provided-p
          (paste text)
          (paste))))
  (:documentation "Paste the last clipboard entry into BUFFER.
If TEXT is provided, paste it instead."))

(define-ffi-generic ffi-buffer-cut (buffer)
  (:method :around ((buffer t))
    ;; Save the top of clipboard before it's overridden.
    (ring-insert-clipboard (clipboard-ring *browser*))
    (sera:lret ((result (call-next-method)))
      (ring-insert-clipboard (clipboard-ring *browser*))))
  (:method ((buffer t))
    (ps-labels :buffer buffer
      ((cut
        ()
        (let ((active-element (nyxt/ps:active-element document)))
          (when (nyxt/ps:element-editable-p active-element)
            (let ((selection-text (ps:chain window (get-selection) (to-string))))
              (nyxt/ps:insert-at active-element "")
              selection-text)))))
      (sera:lret ((input (cut)))
        (when input
          (copy-to-clipboard input)
          (echo "Text cut: ~s" input)))))
  (:documentation "Cut selected text in BUFFER to the system clipboard.
Return the text cut."))

(define-ffi-generic ffi-buffer-select-all (buffer)
  (:method ((buffer t))
    (ps-eval :async t :buffer buffer
      (let ((active-element (nyxt/ps:active-element document)))
        (when (nyxt/ps:element-editable-p active-element)
          (ps:chain active-element (set-selection-range 0 (ps:@ active-element value length)))))))
  (:documentation "Select all text in BUFFER web view."))

(define-ffi-generic ffi-buffer-undo (buffer)
  (:documentation "Undo the last text edit performed in BUFFER's web view."))

(define-ffi-generic ffi-buffer-redo (buffer)
  (:documentation "Redo the last undone text edit performed in BUFFER's web view."))

;; TODO: Move to alists for arbitrary number of params?
(defvar *context-menu-commands* (make-hash-table :test #'equal)
  "A hash table from labels to context menu commands.
Once a context menu appears, those commands will be added to it as actions with
the labels they have as hash keys.")

;; TODO: Add TEST arg to decide on whether to display?
(define-ffi-generic ffi-add-context-menu-command (command label)
  (:method ((command command) (label string))
    (setf (gethash label *context-menu-commands*)
          command))
  (:method ((command list) (label string))
    (flet ((thing->function (thing)
             (typecase thing
               (symbol (symbol-function thing))
               (function thing))))
      (setf (gethash label *context-menu-commands*)
            (mapcar (lambda (pair)
                      ;; Convert to an undotted alist.
                      (match pair
                        ((cons command (list label))
                         (list (thing->function command) label))
                        ((cons command label)
                         (list (thing->function command) label))))
                    command))))
  (:method ((command function) (label string))
    (setf (gethash label *context-menu-commands*)
          command))
  (:method ((command symbol) (label string))
    (ffi-add-context-menu-command (symbol-function command) label))
  (:documentation "Add COMMAND as accessible in context menus with LABEL displayed for it.
COMMAND can be a:
- `command',
- `function',
- symbol naming either a command or function,
- or an alist (dotted or undotted) of COMMAND (any of above types, but not list)
  to LABEL pairs.

In case COMMAND is an alist, every command in this alist is bound to its own
label, and all of those are available under LABEL-named submenu.

Example:

\(ffi-add-context-menu-command
 (list (list 'reload-current-buffer \"Reload it\")
       (list #'(lambda () (delete-buffer :buffers (current-buffer))) \"Delete it\"))
 \"Buffer actions\")"))


;;; Signals

(define-ffi-generic on-signal-notify-uri (object url)
  (:method ((buffer buffer) no-url)
    (declare (ignore no-url))
    ;; Need to run the mode-specific actions first so that modes can modify the
    ;; behavior of buffer.
    (dolist (mode (modes buffer))
      (on-signal-notify-uri mode (url buffer)))
    (let ((view-url (ffi-buffer-url buffer)))
      (unless (or (load-failed-p buffer)
                  (url-empty-p view-url))
        ;; When a buffer fails to load and `ffi-buffer-url' returns an empty
        ;; URL, we don't set (url buffer) to keep access to the old value.
        (setf (url buffer) (ffi-buffer-url buffer))))
    (url buffer))
  (:method ((mode mode) url)
    url)
  (:documentation "Invoked when URL changes in OBJECT.
Dispatches on buffers and modes."))

(define-ffi-generic on-signal-notify-title (object title)
  (:method ((buffer buffer) no-title)
    (declare (ignore no-title))
    (setf (title buffer) (ffi-buffer-title buffer))
    (dolist (mode (modes buffer))
      (on-signal-notify-title mode (url buffer)))
    (title buffer))
  (:method ((mode mode) title)
    (on-signal-notify-uri mode (url (buffer mode)))
    title)
  (:documentation "Invoked when page TITLE is set in OBJECT.
Dispatches on buffers and modes."))

;; See https://webkitgtk.org/reference/webkit2gtk/stable/WebKitWebView.html#WebKitLoadEvent
(define-ffi-generic on-signal-load-started (object url)
  (:method ((buffer buffer) url)
    (dolist (mode (modes buffer))
      (on-signal-load-started mode url)))
  (:method ((mode mode) url)
    url)
  (:documentation "Invoked when URL starts loading in OBJECT.
Dispatches on buffers and modes."))

(define-ffi-generic on-signal-load-redirected (object url)
  (:method ((buffer buffer) url)
    (dolist (mode (modes buffer))
      (on-signal-load-redirected mode url)))
  (:method ((mode mode) url)
    url)
  (:documentation "Invoked when the request gets redirected to URL in OBJECT.
Dispatches on buffers and modes."))

(define-ffi-generic on-signal-load-canceled (object url)
  (:method ((buffer buffer) url)
    (dolist (mode (modes buffer))
      (on-signal-load-canceled mode url)))
  (:method ((mode mode) url)
    url)
  (:documentation "Invoked when URL loading is canceled in OBJECT.
Dispatches on buffers and modes."))

(define-ffi-generic on-signal-load-committed (object url)
  (:method ((buffer buffer) url)
    (dolist (mode (modes buffer))
      (on-signal-load-committed mode url)))
  (:method ((mode mode) url)
    url)
  (:documentation "Invoked when URL loading is approved in OBJECT.
Dispatches on buffers and modes."))

(define-ffi-generic on-signal-load-finished (object url)
  (:method ((buffer buffer) url)
    (update-document-model :buffer buffer)
    (dolist (mode (modes buffer))
      (on-signal-load-finished mode url))
    (run-thread "buffer-loaded-hook"
      (hooks:run-hook (buffer-loaded-hook buffer) buffer)))
  (:method ((mode mode) url)
    url)
  (:documentation "Invoked when done loading URL in OBJECT.
Dispatches on buffers and modes."))

(define-ffi-generic on-signal-load-failed (object url)
  (:method ((buffer buffer) url)
    (dolist (mode (modes buffer))
      (on-signal-load-failed mode url)))
  (:method ((mode mode) url)
    url)
  (:documentation "Invoked when URL loading has failed in OBJECT.
Dispatches on buffers and modes."))

(define-ffi-generic on-signal-button-press (object button-key)
  (:method ((buffer buffer) button-key)
    (dolist (mode (modes buffer))
      (on-signal-button-press mode button-key)))
  (:method ((mode mode) button-key)
    (declare (ignorable button-key))
    nil)
  (:documentation "Invoked on BUTTON-KEY press.
Dispatches on buffers and modes."))

(define-ffi-generic on-signal-key-press (object key)
  (:method ((buffer buffer) key)
    (dolist (mode (modes buffer))
      (on-signal-key-press mode key)))
  (:method ((mode mode) key)
    (declare (ignorable key))
    nil)
  (:documentation "Invoked on KEY press.
Dispatches on buffers and modes."))
