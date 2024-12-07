;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/renderer/electron
  (:documentation "Electron renderer."))
(in-package :nyxt/renderer/electron)

(push :nyxt-electron *features*)

(define-class electron-renderer (renderer)
  ((name "Electron"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Electron renderer class."))

(defmethod install ((renderer electron-renderer))
  (flet ((set-superclasses (renderer-class-sym+superclasses)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class (first renderer-class-sym+superclasses)
                                     :direct-superclasses (rest renderer-class-sym+superclasses)
                                     :metaclass 'interface-class))))
    (mapc #'set-superclasses '((renderer-browser electron-browser)
                               (renderer-scheme electron-scheme)
                               (renderer-window electron-window)
                               (renderer-buffer electron-buffer)))))

(defmethod uninstall ((renderer electron-renderer))
  (flet ((remove-superclasses (renderer-class-sym)
           (closer-mop:ensure-finalized
            (closer-mop:ensure-class renderer-class-sym
                                     :direct-superclasses '()
                                     :metaclass 'interface-class))))
    (mapc #'remove-superclasses '(renderer-browser
                                  renderer-scheme
                                  renderer-window
                                  renderer-buffer))))

(setf nyxt::*renderer* (make-instance 'electron-renderer))

(define-class electron-scheme (electron:protocol)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Electron scheme class."))

(defmethod initialize-instance :after ((scheme electron-scheme) &key)
  ;; Set the scheme name from child (`scheme') to parent class
  ;; (`electron-scheme').
  (setf (slot-value scheme 'electron:scheme-name)
        (name scheme)))

(defmethod ffi-register-custom-scheme ((scheme electron-scheme))
  (electron:handle-callback scheme (callback scheme)))

(define-class electron-browser (electron:interface)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Electron browser class."))

(defmethod ffi-initialize ((browser electron-browser) urls startup-timestamp)
  (declare (ignore urls startup-timestamp))
  (log:debug "Initializing Electron Interface")
  (setf electron:*interface* (make-instance 'electron:interface))
  ;; Schemes' privileges (security settings) need to be set before launch.
  (setf (electron:protocols electron:*interface*)
        (list (make-instance 'electron:protocol
                             :scheme-name "nyxt"
                             :privileges "{}")
             (make-instance 'electron:protocol
                             :scheme-name "nyxt-resource"
                             :privileges "{secure:true}")
              (make-instance 'electron:protocol
                             :scheme-name "lisp"
                             :privileges "{supportFetchAPI:true,corsEnabled:true}")
              (make-instance 'electron:protocol
                             :scheme-name "view-source"
                             :privileges "{}")
              (make-instance 'electron:protocol
                             :scheme-name "editor"
                             :privileges "{}")
              (make-instance 'electron:protocol
                             :scheme-name "gopher"
                             :privileges "{}")
              (make-instance 'electron:protocol
                             :scheme-name "gemini"
                             :privileges "{}")))
  (electron:launch electron:*interface*)
  (maphash (lambda (scheme-name callbacks)
             (ffi-register-custom-scheme (make-instance 'scheme
                                                        :name scheme-name
                                                        :callback (first callbacks)
                                                        :error-callback (second callbacks))))
           nyxt::*schemes*)
  (call-next-method)
  (unless nyxt::*run-from-repl-p*
    (uiop:wait-process (electron:process electron:*interface*))
    (uiop:quit (nyxt:exit-code browser) #+bsd nil)))

(defmethod ffi-kill-browser ((browser electron-browser))
  (declare (ignore browser))
  (electron:terminate))

(define-class electron-buffer (electron:view)
  ((electron:options
    ""
    :export t
    :reader t
    :writer nil
    :type string
    :documentation "A string that specifies the buffer's behavior.")
   (modifier-plist
    '(:shift "shift"
      :control "control"
      :alt "meta"
      :meta "super")
    :type list
    :documentation "A map between Electron's and Nyxt's terminology for modifier keys.
Note that by changing the default value, modifier keys can be remapped."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Electron buffer class."))

(defmethod initialize-instance :after ((buffer electron-buffer) &key extra-modes no-hook-p)
  ;; Needed for buffers whose HTML document is set via JS
  ;; (e.g. `status-buffer').
  (electron:load-url buffer "about:blank")
  (electron:add-listener buffer :before-input-event
                         (lambda (buffer event)
                           (on-signal-key-press-event buffer event)))
  (finalize-buffer buffer :extra-modes extra-modes :no-hook-p no-hook-p))

;; TODO Needed for reopen-dead-buffer.
;; (defmethod ffi-buffer-make ((buffer electron-buffer))
;;   (make-instance 'electron-buffer))

(defmethod ffi-buffer-delete ((buffer electron-buffer))
  ;; TODO support multi-window setup.
  (electron:remove-view (current-window) buffer))

(defmethod ffi-buffer-url ((buffer electron-buffer))
  (quri:uri (electron:get-url buffer)))

(defmethod ffi-buffer-title ((buffer electron-buffer))
  (electron:get-title buffer))

(defmethod ffi-buffer-load ((buffer electron-buffer) url)
  ;; Primitive way to introduce the auto-rules logic.
  (apply-auto-rules url buffer)
  ;; Hack until on-signal-* methods are handled.
  (setf (slot-value buffer 'url) url)
  ;; Taken from the GTK port, although it shows bad design:
  ;; Mark buffer as :loading right away so functions like
  ;; `ffi-window-set-buffer' don't try to reload if they are called before the
  ;; "load-changed" signal is emitted.
  (when (web-buffer-p buffer) (setf (nyxt::status buffer) :loading))
  (electron:load-url buffer url)
  (electron:add-listener (electron:web-contents buffer) :did-finish-load
                          (lambda (_) (declare (ignore _))
                            (setf (nyxt::status buffer) :finished)
                            (on-signal-load-finished buffer url)
                            (unless (internal-url-p url)
                              (echo "Finished loading ~s." (render-url url))))
                          :once-p t))

(defmethod ffi-buffer-evaluate-javascript ((buffer electron-buffer) javascript
                                           &optional world-name)
  ;; TODO world-name is used in user-script mode.
  (declare (ignore world-name))
  (electron:execute-javascript-synchronous (electron:web-contents buffer) javascript))

(defmethod ffi-buffer-evaluate-javascript-async ((buffer electron-buffer) javascript
                                                 &optional world-name)
  (declare (ignore world-name))
  (electron:execute-javascript (electron:web-contents buffer) javascript))

(defmethod ffi-inspector-show ((buffer electron-buffer))
  (electron:open-dev-tools buffer))

(defmethod ffi-focused-p ((buffer electron-buffer))
  (electron:is-focused buffer))

;; ffi-buffer-load-alternate-html handles bogus URLs (https://bogusfoo.com/).
;; (defmethod ffi-buffer-load-alternate-html ((buffer electron-buffer) html-content content-url url))

(defmethod ffi-buffer-copy ((buffer electron-buffer) &optional (text nil text-provided-p))
  (if text-provided-p
      (trivial-clipboard:text text)
      (progn
        (electron:copy (electron:web-contents buffer))
        (trivial-clipboard:content))))

(defmethod ffi-buffer-paste ((buffer electron-buffer) &optional (text nil text-provided-p))
  (if text-provided-p
      (electron:insert-text (electron:web-contents buffer) text)
      (electron:paste (electron:web-contents buffer))))

(defmethod ffi-buffer-cut ((buffer electron-buffer))
  (electron:cut (electron:web-contents buffer))
  (trivial-clipboard:text))

(defmethod ffi-buffer-select-all ((buffer electron-buffer))
  (electron:select-all (electron:web-contents buffer)))

(defmethod ffi-buffer-undo ((buffer electron-buffer))
  ;; There is no way to check if an undo operation is possible. There exists a
  ;; `context-menu' event that when invoked can check whether `canUndo' exists
  ;; within the `editFLags' of the renderer, but we cannot manually trigger this
  ;; event.
  (electron:undo (electron:web-contents buffer)))

(defmethod ffi-buffer-redo ((buffer electron-buffer))
  ;; There is no way to check if a redo operation is possible. There exists a
  ;; `context-menu' event that when invoked can check whether `canRedo' exists
  ;; within the `editFLags' of the renderer, but we cannot manually trigger this
  ;; event.
  (electron:redo (electron:web-contents buffer)))

;; TODO
;; (defmethod ffi-buffer-cookie-policy ((buffer electron-buffer)))

(defmethod ffi-height ((buffer electron-buffer))
  (assoc-value (electron:get-bounds buffer) :height))

(defmethod (setf ffi-height) ((height integer) (buffer electron-buffer))
  (let ((bounds (electron:get-bounds buffer)))
    (electron:set-bounds buffer
                         :x (assoc-value bounds :x)
                         :y (assoc-value bounds :y)
                         :width (assoc-value bounds :width)
                         :height height)))

(defmethod ffi-focus-prompt-buffer ((prompt-buffer prompt-buffer))
  (electron:focus prompt-buffer)
  prompt-buffer)

(defmethod (setf ffi-height) ((height integer) (prompt-buffer prompt-buffer))
  (with-slots (window) prompt-buffer
    (let ((window-bounds (electron:get-bounds window)))
      (electron:set-bounds (active-buffer window)
                           :x 0
                           :y 0
                           :width (assoc-value window-bounds :width)
                           :height (- (assoc-value window-bounds :height)
                                      (+ height
                                         (height (status-buffer window))
                                         (height (message-buffer window))))))
    (electron:add-bounded-view window
                               prompt-buffer
                               :window-bounds-alist-var bounds
                               :x 0
                               :y (- (assoc-value bounds :height)
                                     (+ height
                                        (height (status-buffer window))
                                        (height (message-buffer window))))
                               :width (assoc-value bounds :width)
                               :height height)))

(defmethod ffi-width ((buffer electron-buffer))
  (assoc-value (electron:get-bounds buffer) :height))

(defmethod (setf ffi-width) (width (buffer electron-buffer))
  (let ((bounds (electron:get-bounds buffer)))
    (electron:set-bounds buffer
                         :x (assoc-value bounds :x)
                         :y (assoc-value bounds :y)
                         :width width
                         :height (assoc-value bounds :height))))

(defmethod ffi-buffer-sound-enabled-p ((buffer electron-buffer))
  (not (electron:muted-p (electron:web-contents buffer))))
(defmethod (setf ffi-buffer-sound-enabled-p) ((buffer electron-buffer) value)
  (electron:set-audio-muted (electron:web-contents buffer) (not value)))

;; TODO Support download-mode.
;; (defmethod ffi-buffer-download ((buffer electron-buffer) url))

;; TODO Support user-script mode.
;; (defmethod ffi-buffer-add-user-style ((buffer electron-buffer) style))
;; (defmethod ffi-buffer-remove-user-style ((buffer electron-buffer) style))
;; (defmethod ffi-buffer-add-user-script ((buffer electron-buffer) script))
;; (defmethod ffi-buffer-remove-user-script ((buffer electron-buffer) script))

;; TODO Support no-image mode.
;; (defmethod ffi-buffer-auto-load-image-enabled-p ((buffer electron-buffer)))
;; (defmethod (setf ffi-buffer-auto-load-image-enabled-p) ((buffer electron-buffer)))

;; TODO Support reduce-tracking mode.
;; See https://stackoverflow.com/a/35672988
;; See https://www.electronjs.org/docs/latest/api/web-contents#contentsloadurlurl-options
;; (defmethod ffi-buffer-user-agent ((buffer electron-buffer)))
;; (defmethod (setf ffi-buffer-user-agent) (value (buffer electron-buffer)))
;; (defmethod ffi-tracking-prevention ((buffer electron-buffer)))
;; (defmethod (setf ffi-tracking-prevention) (value (buffer electron-buffer)))
;; See https://www.electronjs.org/docs/latest/api/session#sessetuseragentuseragent-acceptlanguages.
;; (defmethod ffi-preferred-languages ((buffer electron-buffer)))

(define-class electron-window (electron:window)
  ()
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:metaclass user-class)
  (:documentation "Electron window class."))

(defmethod initialize-instance :after ((window electron-window) &key)
  (let ((message-buffer (message-buffer window))
        (status-buffer (status-buffer window)))
    (electron:add-bounded-view window
                               message-buffer
                               :window-bounds-alist-var bounds
                               :x 0
                               :y (- (assoc-value bounds :height)
                                     (height message-buffer))
                               :width (assoc-value bounds :width)
                               :height (height message-buffer))
    (electron:add-bounded-view window
                               status-buffer
                               :window-bounds-alist-var bounds
                               :x 0
                               :y (- (assoc-value bounds :height)
                                     (+ (height status-buffer)
                                        (height message-buffer)))
                               :width (assoc-value bounds :width)
                               :height (height status-buffer))))

(defmethod ffi-window-delete ((window electron-window))
  (electron:kill window))

(defmethod ffi-window-title ((window electron-window))
  (electron:get-title window))
(defmethod (setf ffi-window-title) (title (window electron-window))
  (electron:set-title window title))

(defmethod ffi-window-set-buffer ((window electron-window)
                                  (buffer electron-buffer)
                                  &key (focus t))
  (electron:add-bounded-view window
                             buffer
                             :window-bounds-alist-var bounds
                             :x 0
                             :y 0
                             :width (assoc-value bounds :width)
                             :height (- (assoc-value bounds :height)
                                        (+ (height (status-buffer window))
                                           (height (message-buffer window))
                                           (if-let ((prompt (current-prompt-buffer)))
                                             (ffi-height prompt)
                                             0))))
  (when focus (electron:focus buffer))
  buffer)

(defmethod ffi-height ((window electron-window))
  (assoc-value (electron:get-bounds window) :height))

(defmethod ffi-width ((window electron-window))
  (assoc-value (electron:get-bounds window) :width))

(defmethod ffi-window-fullscreen ((window electron-window) &key &allow-other-keys)
  (electron:fullscreen window))

(defmethod ffi-window-unfullscreen ((window electron-window) &key &allow-other-keys)
  (electron:unfullscreen window))

(defmethod ffi-window-maximize ((window electron-window) &key &allow-other-keys)
  (electron:maximize window))

(defmethod ffi-window-unmaximize ((window electron-window) &key &allow-other-keys)
  (electron:unmaximize window))

;; Input handling

(defmethod input-modifier-translator ((buffer electron-buffer) input-event-modifier-state)
  "Return a list of modifier keys understood by `keymaps:make-key'."
  (when-let ((state input-event-modifier-state))
    (mapcar (lambda (modifier) (getf (modifier-plist buffer) modifier)) state)))

(defun translate-key-string (key-string)
  "Return string representation of a keyval.
Return nil when key must be discarded, e.g. for modifiers."
  (match key-string
    ;; Compatibility layer between GDK keycode names and those of Browsers.
    ;; https://gitlab.gnome.org/GNOME/gtk/-/blob/main/gdk/gdkkeysyms.h
    ;; https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
    ("ControlLeft" nil)
    ("ControlRight" nil)
    ("ShiftLeft" nil)
    ("ShiftRight" nil)
    ("MetaLeft" nil)
    ("MetaRight" nil)
    ("AltLeft" nil)
    ("AltRight" nil)
    ("Minus" "hyphen")
    ("Equal" "=")
    ("Space" "space")
    ("Enter" "return")
    ("Escape" "escape")
    ("Tab" "tab")
    ("Backspace" "backspace")
    ("ArrowUp" "up")
    ("ArrowDown" "down")
    ("ArrowRight" "right")
    ("ArrowLeft" "left")
    ("PageUp" "pageup")
    ("PageDown" "pagedown")
    ("Home" "home")
    ("End" "end")
    ("F1" "f1")
    ("F2" "f2")
    ("F3" "f3")
    ("F4" "f4")
    ("F5" "f5")
    ("F6" "f6")
    ("F7" "f7")
    ("F8" "f8")
    ("F9" "f9")
    ("F10" "f10")
    ("F11" "f11")
    ("F12" "f12")
    ((simple-string #\K #\e #\y value) (string-downcase (string value)))
    (_ key-string)))

(defmethod on-signal-key-press-event ((sender electron-buffer) event)
  (when (string= "keyDown" (assoc-value event :type))
    (let ((modifiers (delete nil (list (when (assoc-value event :shift) :shift)
                                       (when (assoc-value event :control) :control)
                                       (when (assoc-value event :alt) :alt)
                                       (when (assoc-value event :meta) :meta))))
          (key-string (translate-key-string (assoc-value event :code))))
      (flet ((key () (keymaps:make-key :value key-string
                                       :modifiers (input-modifier-translator sender modifiers)
                                       :status :pressed)))
        (when key-string
          (alex:appendf (key-stack sender)
                        (list (key)))
          (run-thread "on-signal-key-press" (on-signal-key-press sender (key)))
          (dispatch-input-event event sender))))))

(defmethod on-signal-key-release-event ((sender electron-window) event)
  (declare (ignore sender event)))

;; TODO on-signal-* methods
