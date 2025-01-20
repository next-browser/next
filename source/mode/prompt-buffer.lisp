;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/prompt-buffer
  (:documentation "Package for `prompt-buffer-mode' to interact with the `prompt-buffer'."))
(in-package :nyxt/mode/prompt-buffer)

(define-mode prompt-buffer-mode ()
  "The prompt buffer is where all interactions between Nyxt and the user take place.
It displays a list of suggestions which are filtered as the user types.

Many prompter-buffer-specific commands are available; you can list them with
`run-prompt-buffer-command', bound to \"f1 b\" by default.

The prompt buffer can have multiple `prompter:source's of suggestions.  Each
source has its own properties, such as the ability to mark multiple suggestions.
The same source can be used by different prompt buffers.

Each source offers a set of `actions-on-return' for marked items.  These can be
listed and chosen from with the command `set-action-on-return' (bound to
\"M-return\" by default)."
  ((visible-in-status-p nil)
   (keyscheme-map
    (define-keyscheme-map "prompt-buffer-mode" ()
      keyscheme:default
      (list
       "up" 'previous-suggestion
       "button4" 'previous-suggestion
       "down" 'next-suggestion
       "button5" 'next-suggestion
       "home" 'first-suggestion
       "pagehome" 'first-suggestion
       "end" 'last-suggestion
       "pageend" 'last-suggestion
       "pageup" 'previous-page
       "pagedown" 'next-page
       "shift-up" 'previous-source
       "shift-down" 'next-source
       "shift-left" 'first-suggestion-within-source
       "shift-right" 'last-suggestion-within-source
       "M-o" 'toggle-prompt-buffer-focus
       "escape" 'quit-prompt-buffer
       "M-a" 'mark-all
       "M-u" 'unmark-all
       "C-space" 'toggle-mark-forwards
       "M-space" 'toggle-mark-forwards
       "shift-space" 'toggle-mark-backwards
       "M-shift-space" 'toggle-mark-backwards
       "M-m" 'toggle-mark-all
       "M-h" 'history
       "f1 b" 'run-prompt-buffer-command
       "f1 m" 'describe-prompt-buffer
       "return" 'run-action-on-return
       "keypadenter" 'run-action-on-return
       "M-return" 'set-action-on-return
       "M-keypadenter" 'set-action-on-return
       "C-return" 'toggle-mark-forwards
       "C-keypadenter" 'toggle-mark-forwards
       "shift-return" 'toggle-mark-forwards
       "shift-keypadenter" 'toggle-mark-forwards
       "C-j" 'run-action-on-current-suggestion
       "C-c C-j" 'set-action-on-current-suggestion
       "tab" 'insert-current-suggestion
       ; TODO: This is the Emacs Helm binding.  Better?
       "C-c C-f" 'toggle-actions-on-current-suggestion
       ; TODO: This is the Emacs Helm binding.  Better?
       "C-]" 'toggle-attributes-display
       "M-shift-up" 'toggle-suggestions-display)
      keyscheme:cua
      (list
       "C-up" 'first-suggestion
       "C-shift-up" 'first-suggestion-within-source
       "C-down" 'last-suggestion
       "C-shift-down" 'last-suggestion-within-source
       "C-v" 'paste
       "C-x" 'cut
       "C-a" 'select-input-text)
      keyscheme:emacs
      (list
       "C-p" 'previous-suggestion
       "C-n" 'next-suggestion
       "M-<" 'first-suggestion
       "M-," 'first-suggestion-within-source
       "M->" 'last-suggestion
       "M-." 'last-suggestion-within-source
       "C-x o" 'toggle-prompt-buffer-focus
       "M-v" 'previous-page
       "C-v" 'next-page
       "M-p" 'previous-source
       "M-n" 'next-source
       "M-[" 'previous-source    ; Emacs Helm binding.
       "M-]" 'next-source        ; Emacs Helm binding.
       "C-M-n" 'scroll-other-buffer-down
       "C-M-p" 'scroll-other-buffer-up
       "C-M-v" 'scroll-page-down-other-buffer
       "C-M-V" 'scroll-page-up-other-buffer
       "C-g" 'quit-prompt-buffer
       "C-e" 'move-end-of-input
       "C-a" 'move-start-of-input
       "C-b" 'nyxt/mode/input-edit:cursor-backwards
       "C-f" 'nyxt/mode/input-edit:cursor-forwards
       "C-d" 'nyxt/mode/input-edit:delete-forwards
       "M-b" 'nyxt/mode/input-edit:cursor-backwards-word
       "M-f" 'nyxt/mode/input-edit:cursor-forwards-word
       "C-backspace" 'nyxt/mode/input-edit:delete-backwards-word
       "M-backspace" 'nyxt/mode/input-edit:delete-backwards-word
       "M-d" 'nyxt/mode/input-edit:delete-forwards-word
       "C-x h" 'select-input-text
       "M-w" 'copy-selection
       "C-y" 'paste
       "C-w" 'cut
       "C-h m" 'describe-prompt-buffer
       "C-h b" 'run-prompt-buffer-command
       "C-j" 'run-action-on-current-suggestion)
      keyscheme:vi-normal
      (list
       "k" 'previous-suggestion
       "j" 'next-suggestion
       "C-k" 'previous-suggestion
       ;; C-j and C-k are useful in insert mode since "j", "k" are taken.
       ;; We bind C-j and C-k in normal mode for consistency between the two modes.
       "C-j" 'next-suggestion
       "g g" 'first-suggestion
       "G" 'last-suggestion
       "C-b" 'previous-page
       "C-f" 'next-page
       "K" 'previous-source
       "J" 'next-source
       "C-K" 'previous-source
       ;; Same as with C-j.
       "C-J" 'next-source
       "M-j" 'scroll-other-buffer-down
       "M-k" 'scroll-other-buffer-up
       "C-M-j" 'scroll-page-down-other-buffer
       "C-M-k" 'scroll-page-up-other-buffer
       "$" 'move-end-of-input
       "^" 'move-start-of-input
       "l" 'nyxt/mode/input-edit:cursor-forwards
       "h" 'nyxt/mode/input-edit:cursor-backwards
       "w" 'nyxt/mode/input-edit:cursor-forwards-word
       "b" 'nyxt/mode/input-edit:cursor-backwards-word
       "x" 'nyxt/mode/input-edit:delete-forwards
       "d b" 'nyxt/mode/input-edit:delete-backwards-word
       "d w" 'nyxt/mode/input-edit:delete-forwards-word
       "z f" 'toggle-actions-on-current-suggestion
       "z a" 'toggle-attributes-display
       "y" 'copy-selection
       "p" 'paste
       "d d" 'cut)
      keyscheme:vi-insert
      (list
       "return" 'run-action-on-return
       "keypadenter" 'run-action-on-return
       "M-return" 'set-action-on-return
       "M-keypadenter" 'set-action-on-return
       "C-k" 'previous-suggestion
       "C-j" 'next-suggestion
       "C-b" 'previous-page
       "C-f" 'next-page
       "C-K" 'previous-source
       "C-J" 'next-source))))
  (:toggler-command-p nil))

(export-always 'define-command-prompt)
(defmacro define-command-prompt (name (prompt-buffer &rest arglist) &body body)
  "Like `define-command', but the first argument is special:
- it is considered a keyword argument if `&keyword' is in arglist, `&optional' otherwise,
- it is bound to `current-prompt-buffer' if unspecified,
- the body is skipped and a warning is emitted unless non-nil."
  (multiple-value-bind (forms declares documentation)
      (alex:parse-body body :documentation t)
    (multiple-value-bind (required optional rest keywords aok? aux key?)
        (alex:parse-ordinary-lambda-list arglist)
      (flet ((unparse-arguments (prompt-buffer-sym)
               (if keywords
                   (push `((,(intern (string prompt-buffer-sym) "KEYWORD") ,prompt-buffer-sym) (current-prompt-buffer) nil)
                         keywords)
                   (push `(,prompt-buffer-sym (current-prompt-buffer) nil)
                         optional))
               (sera:unparse-ordinary-lambda-list required optional rest keywords aok? aux key?)))
        `(define-command ,name ,(unparse-arguments prompt-buffer)
           ,@(sera:unsplice documentation)
           ,@declares
           (if prompt-buffer
               (progn ,@forms)
               (log:warn "Can't call ~a on nil prompt buffer" ',name)))))))

(define-command-prompt next-suggestion (prompt-buffer)
  "Select next entry in prompt buffer."
  (prompter:next-suggestion prompt-buffer)
  ;; TODO: Update display?  The library should probably decide when to update
  ;; it.  Drawback is that it maybe result in too many draws.  If the caller
  ;; decides when redraw, it has more control.
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt previous-suggestion (prompt-buffer)
  "Select previous entry in PROMPT-BUFFER."
  (prompter:previous-suggestion prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt first-suggestion (prompt-buffer)
  "Select first entry in PROMPT-BUFFER."
  (prompter:first-suggestion prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt last-suggestion (prompt-buffer)
  "Select last entry in PROMPT-BUFFER."
  (prompter:last-suggestion prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt first-suggestion-within-source (prompt-buffer)
  "Select first entry in the current PROMPT-BUFFER's source."
  (let ((first-source-p (eq (prompter:current-source prompt-buffer)
                            (first (prompter:previous-source prompt-buffer)))))
    (if first-source-p
        (prompter:first-suggestion prompt-buffer)
        (prompter:next-suggestion prompt-buffer)))
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt last-suggestion-within-source (prompt-buffer)
  "Select last entry in the current PROMPT-BUFFER's source."
  (let ((last-source-p (eq (prompter:current-source prompt-buffer)
                           (first (prompter:next-source prompt-buffer)))))
    (if last-source-p
        (prompter:last-suggestion prompt-buffer)
        (prompter:previous-suggestion prompt-buffer)))
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt next-source (prompt-buffer)
  "Select next source in PROMPT-BUFFER."
  (prompter:next-source prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt previous-source (prompt-buffer)
  "Select previous source in PROMPT-BUFFER."
  (prompter:previous-source prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt next-page (prompt-buffer &key (n 1))
  "Select entry by N next pages in PROMPT-BUFFER.
If N is negative, go to previous pages instead."
  (unless (= 0 n)
    (let ((step-page-index              ; TODO: Add multi-source support.
            (ps-eval :buffer prompt-buffer
              (defun step-row (row)
                (ps:chain
                 (aref (ps:chain row parent-node rows)
                       (max 0
                            (min (- (ps:chain row parent-node rows length) 1)
                                 (+ (if (< 0 (ps:lisp n)) 1 -1)
                                    (ps:chain row row-index)))))))
              (defun find-first-element-out-of-view (row)
                (if (nyxt/ps:element-in-view-port-p row)
                    (let ((new-row (step-row row)))
                      (if (eq new-row row)
                          row
                          (find-first-element-out-of-view new-row)))
                    row))
              (-
               (ps:chain
                (find-first-element-out-of-view (nyxt/ps:qs document "#selection"))
                row-index)
               (ps:chain (nyxt/ps:qs document "#selection") row-index)))))
      (and-let* ((index-diff step-page-index))
        (prompter:next-suggestion prompt-buffer
                                  index-diff)))
    ;; TODO: Update display?  The library should probably decide when to update
    ;; it.  Drawback is that it maybe result in too many draws.  If the caller
    ;; decides when redraw, it has more control.
    (prompt-render-suggestions prompt-buffer)))

(define-command-prompt previous-page (prompt-buffer &key (n 1))
  "Select entry by N previous pages in PROMPT-BUFFER.
If N is negative, go to next pages instead."
  (next-page :prompt-buffer prompt-buffer :n (- n)))

(define-command-prompt run-action-on-return (prompt-buffer)
  "Have the PROMPT-BUFFER return the marks, then quit."
  (prompter:run-action-on-return prompt-buffer))

(defun make-attribute-suggestion (attribute &optional source input)
  "Return a `suggestion' wrapped around ATTRIBUTE."
  (declare (ignore source input))
  (make-instance 'prompter:suggestion
                 :value attribute
                 :attributes `(("Attribute key" ,attribute))))

(define-class attribute-source (prompter:source)
  ((prompter:name "List of prompter attributes")
   (prompter:enable-marks-p t)
   (prompter:suggestion-maker 'make-attribute-suggestion)
   (prompter:actions-on-return #'return-marks-only)))

(defun return-marks-only (suggestion-values)
  "Return marked suggestions only.
They are returned untouched.
This is useful for prompters where we want either marks or nothing, but not the
current unmarked suggestion."
  (multiple-value-bind (suggestion source)
      (prompter:%current-suggestion (current-prompt-buffer))
    (if (and (typep source 'attribute-source)
             (not (prompter:marks source)))
        (remove (prompter:value suggestion) suggestion-values
                :test #'equal)
        suggestion-values)))

(define-command-prompt toggle-attributes-display (prompt-buffer
                                                  &key (source (current-source prompt-buffer)))
  "Prompt for which prompter attributes to display."
  (let ((attributes (prompt :prompt "Mark attributes to display"
                            :sources
                            (make-instance
                             'attribute-source
                             :marks (intersection (prompter:active-attributes-keys source)
                                                  (prompter:attributes-keys-non-default source)
                                                  :test #'string=)
                             :constructor (prompter:attributes-keys-non-default source)))))
    (setf (prompter:active-attributes-keys source)
          attributes)
    (prompt-render-suggestions prompt-buffer)))

(define-command-prompt toggle-suggestions-display (prompt-buffer)
  "Toggle between showing suggestions."
  (setf (height prompt-buffer)
        (if (eq (height prompt-buffer) :fit-to-prompt) :default :fit-to-prompt)))

(define-class prompt-buffer-command-source (command-source)
  ((prompter:name "Prompt buffer commands")
   (parent-prompt-buffer (error "Parent prompt buffer required"))
   (global-p nil)
   (buffer (current-prompt-buffer))
   (prompter:suggestion-maker 'make-prompt-buffer-command-suggestion)))

(defun make-prompt-buffer-command-suggestion (command source)
  "Return a `suggestion' wrapped around COMMAND."
  (make-instance
   'prompter:suggestion
   :value command
   :attributes (nyxt::command-attributes command (parent-prompt-buffer source))))

(define-command-prompt run-prompt-buffer-command (prompt-buffer)
  "Prompt for a command to call in PROMPT-BUFFER."
  (let ((command (prompt1 :prompt "Command to run in current prompt buffer"
                          :sources (make-instance 'prompt-buffer-command-source
                                                  :parent-prompt-buffer prompt-buffer))))
    (funcall* command)))

(defun prompt-buffer-actions-on-return (&optional (window (current-window)))
  (or (and-let* ((first-prompt-buffer (first (nyxt::active-prompt-buffers window))))
        (prompter:actions-on-return first-prompt-buffer))
      (progn
        (echo-warning "No actions to choose from.")
        (error 'prompt-buffer-canceled))))

(defun prompt-buffer-actions-on-current-suggestion (&optional (window (current-window)))
  (and-let* ((first-prompt-buffer (first (nyxt::active-prompt-buffers window))))
    (prompter:actions-on-current-suggestion (prompter:current-source first-prompt-buffer))))

;; TODO: Should actions-on-return be commands?  For now, they can be either
;; commands or symbols.
(defun make-action-suggestion (action &optional source input)
  "Return a `suggestion' wrapped around ACTION."
  (declare (ignore source input))
  (make-instance
   'prompter:suggestion
   :value action
   ;; TODO: Include bindings in attributes.
   :attributes `(("Name" ,(or (ignore-errors
                               (symbol-name (typecase action
                                              (command (name action))
                                              (function (slynk-backend:function-name action))
                                              (t action))))
                              "Lambda"))
                 ("Documentation" ,(documentation-line action 'function "")))))

(define-class action-on-return-source (prompter:source)
  ((prompter:name "List of actions-on-return")
   (prompter:constructor (prompt-buffer-actions-on-return))
   (prompter:suggestion-maker 'make-action-suggestion)))

(define-class action-on-current-suggestion-source (prompter:source)
  ((prompter:name "List of actions-on-current-suggestion")
   (prompter:constructor (prompt-buffer-actions-on-current-suggestion))
   (prompter:suggestion-maker 'make-action-suggestion)))

(define-command-prompt set-action-on-return (prompt-buffer)
  "Prompt for an action to run over PROMPT-BUFFER `prompter:marks'."
  (if (equal (mapcar #'type-of (prompter:sources (current-prompt-buffer)))
             '(action-on-return-source))
      (echo "Already displaying actions-on-return of previous prompt buffer.")
      (when-let ((action (prompt1 :prompt "Set return action to run over marks"
                                  :sources 'action-on-return-source)))
        (prompter:run-action-on-return prompt-buffer action))))

(define-command-prompt run-action-on-current-suggestion (prompt-buffer)
  "Run `prompter::default-action-on-current-suggestion' without closing PROMPT-BUFFER."
  (prompter:run-action-on-current-suggestion prompt-buffer))

(define-command-prompt set-action-on-current-suggestion (prompt-buffer)
  "Set `prompter:actions-on-current-suggestion' without closing PROMPT-BUFFER."
  (when-let ((action (prompt1 :prompt "Set current suggestion action"
                                   :sources 'action-on-current-suggestion-source)))
    (prompter:set-action-on-current-suggestion action prompt-buffer)))

(define-command-prompt quit-prompt-buffer (prompt-buffer)
  "Close the PROMPT-BUFFER without further action."
  (prompter:destroy prompt-buffer))

(define-command-prompt toggle-actions-on-current-suggestion (prompt-buffer)
  "Toggle whether `prompter:actions-on-current-suggestion' are enabled for PROMPT-BUFFER."
  (prompter:toggle-actions-on-current-suggestion prompt-buffer)
  (echo "Current suggestion actions: ~:[dis~;en~]abled."
        (prompter:actions-on-current-suggestion-enabled-p (current-source prompt-buffer))))

(define-command-prompt toggle-mark-forwards (prompt-buffer &key (direction :forward))
  "Mark current suggestion and `next-suggestion'.
Only available if current PROMPT-BUFFER source `enable-marks-p' is non-nil.
DIRECTION can be `:forward' or `:backward' and specifies which suggestion to
select next."
  (prompter:toggle-mark prompt-buffer)
  (match direction
    (:forward (next-suggestion prompt-buffer))
    (:backward (previous-suggestion prompt-buffer))))

(define-command-prompt toggle-mark-backwards (prompt-buffer)
  "Mark current suggestion and `previous-suggestion'.
Only available if `prompter:enable-marks-p' is non-nil."
  (toggle-mark-forwards :prompt-buffer prompt-buffer
                        :direction :backward))

(define-command-prompt mark-all (prompt-buffer)
  "Mark all suggestions in the current source.
Only available if `prompter:enable-marks-p' is non-nil."
  (prompter:mark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt unmark-all (prompt-buffer)
  "Unmark all visible suggestions in current source.
Only available if `prompter:enable-marks-p' is non-nil."
  (prompter:unmark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt toggle-mark-all (prompt-buffer)
  "Toggle the mark over all visible suggestions in current source.
Only available if `prompter:enable-marks-p' is non-nil."
  (prompter:toggle-mark-all prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(define-command-prompt copy-selection (prompt-buffer)
  "Save default property of selection to clipboard."
  (let* ((marks (prompter:all-marks prompt-buffer))
         (props (if marks
                    (mapcar #'prompter:attributes-default marks)
                    (list (prompter:attributes-default (prompter:%current-suggestion
                                                       prompt-buffer)))))
         ;; Reverse so that text is ordered from oldest mark to newest.
         (text (str:join +newline+ (reverse props))))
    (unless (str:emptyp text)
      (trivial-clipboard:text text)
      (echo "~s copied to clipboard." text))))

(define-command-prompt paste (prompt-buffer)
  "Paste clipboard text to input."
  (ffi-buffer-paste prompt-buffer)
  (nyxt::update-prompt-input prompt-buffer))

(define-command-prompt cut (prompt-buffer)
  "Cut the input text to clipboard."
  (ffi-buffer-cut prompt-buffer)
  (nyxt::update-prompt-input prompt-buffer))

(defun history-entries (&optional (window (current-window)))
  (and-let* ((first-prompt-buffer (first (nyxt::active-prompt-buffers window))))
    (containers:container->list
     (prompter:history first-prompt-buffer))))

(define-class prompt-buffer-history-source (prompter:source)
  ((prompter:name "Prompt buffer input history")
   (prompter:constructor (history-entries))))

(define-command-prompt history (prompt-buffer)
  "Choose a PROMPT-BUFFER input history entry to insert as input."
  (let ((history (prompter:history prompt-buffer)))
    (if (and history (not (containers:empty-p history)))
        (let ((input (prompt1 :prompt "Input history"
                              :sources 'prompt-buffer-history-source)))
          (unless (str:empty? input)
            (nyxt:set-prompt-buffer-input input)))
        (echo "Prompt buffer has no history."))))

(define-command-prompt insert-current-suggestion (prompt-buffer)
  "Insert current suggestion default property in the PROMPT-BUFFER input."
  (when-let ((selection (prompter:attributes-default
                         (prompter:%current-suggestion prompt-buffer))))
    (nyxt:set-prompt-buffer-input selection)))

(define-command-prompt move-start-of-input (prompt-buffer)
  "Move to the beginning of PROMPT-BUFFER input."
  (ps-eval :async t :buffer prompt-buffer
    (let ((input (nyxt/ps:qs document "#input")))
      (setf (ps:@ input selection-start) 0
            (ps:@ input selection-end) 0))))

(define-command-prompt move-end-of-input (prompt-buffer)
  "Move to the end of PROMPT-BUFFER input."
  (ps-eval :async t :buffer prompt-buffer
    (let ((input (nyxt/ps:qs document "#input")))
      (setf (ps:@ input selection-start) (ps:@ input value length)
            (ps:@ input selection-end) (ps:@ input value length)))))

(define-command-prompt select-input-text (prompt-buffer)
  "Select the prompt buffer's input text."
  (ffi-buffer-select-all prompt-buffer))

;; FIXME: Move scroll.lisp from document-mode so that prompt-buffer.lisp can reach
;; it.  Ideas?

(define-command-prompt scroll-other-buffer-up (prompt-buffer
                                               &key (scroll-distance
                                                     (scroll-distance (current-buffer))))
  "Scroll up the buffer behind the prompt."
  (ps-eval :async t :buffer (current-buffer)
    (ps:chain window (scroll-by 0 (ps:lisp (- scroll-distance))))))

(define-command-prompt scroll-other-buffer-down (prompt-buffer
                                                 &key (scroll-distance
                                                       (scroll-distance (current-buffer))))
  "Scroll down the buffer behind the prompt."
  (ps-eval :async t :buffer (current-buffer)
    (ps:chain window (scroll-by 0 (ps:lisp scroll-distance)))))

(define-command-prompt scroll-page-up-other-buffer (prompt-buffer)
  "Scroll up the buffer behind the prompt by one page."
  (ps-eval :async t :buffer (current-buffer)
    (ps:chain window (scroll-by 0 (- (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                        (ps:@ window inner-height)))))))

(define-command-prompt scroll-page-down-other-buffer (prompt-buffer)
  "Scroll down the buffer behind the prompt by one page."
  (ps-eval :async t :buffer (current-buffer)
    (ps:chain window (scroll-by 0 (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                     (ps:@ window inner-height))))))

(defmethod default-modes append ((buffer prompt-buffer))
  '(prompt-buffer-mode))
(defmethod default-modes :around ((buffer prompt-buffer))
  ;; TODO: `prompt-buffer' should not be a web-buffer.
  (set-difference (call-next-method) (list (sym:resolve-symbol :document-mode :mode)
                                           (sym:resolve-symbol :base-mode :mode))))

;; FIXME: Arglist used to have prompt-buffer, but it's not URL-serializable.
;; Maybe have prompt-buffers have IDs so that we can identify those by IDs?
;; How do we actually identify prompt-buffers?
(define-internal-page-command describe-prompt-buffer ()
    (buffer (str:concat "*Help-" (prompter:prompt (current-prompt-buffer)) "-prompter*")
            ;; TODO: Can we somehow fix the load order in the .asd?
            (sym:resolve-symbol :help-mode :mode))
  ;; FIXME: List the documentation of the command using the prompt instead of
  ;; the prompt-buffer documentation somehow?
  "Describe a prompt buffer instance."
  (let* ((prompt-buffer (current-prompt-buffer))
         (modes (modes prompt-buffer))
         (sources (prompter:sources prompt-buffer)))
    (spinneret:with-html-string
      (:h1 (prompter:prompt prompt-buffer) "(" (:nxref :class-name 'prompt-buffer) ")")
      (:pre (:code (:raw (resolve-backtick-quote-links
                          (documentation 'prompt-buffer 'type) :nyxt/mode/prompt-buffer))))
      (:h2 "Modes and keybindings:")
      (:dl
       ;; TODO: Reuse `describe-bindings' infra.
       (loop for mode in modes
             collect (:dt (:nxref :class-name (sera:class-name-of mode)))
             if (gethash (keyscheme prompt-buffer) (keyscheme-map mode))
               collect  (:dd (:table
                              (:tr (:th "Binding") (:th "Command"))
                              (loop for (keyspec . binding)
                                      in (alex:hash-table-alist
                                          (keymaps:keymap->map
                                           (nyxt::get-keymap prompt-buffer (keyscheme-map mode))))
                                    collect (:tr (:td keyspec)
                                                 (:td (typecase binding
                                                        (symbol (:nxref :function binding))
                                                        (command (:nxref :function (name binding)))
                                                        (keyword (:nxref :function (sym:resolve-symbol binding :function)))
                                                        (t (:a :href (nyxt-url
                                                                      'describe-value
                                                                      :id (nyxt::ensure-inspected-id binding))
                                                               binding))))))))
             else
               collect (:dd "No binding")))
      (:h2 "Sources:")
      (:dl
       (loop for source in sources
             collect (:dt (:nxref :class-name (sera:class-name-of source)))
             collect (:dd (documentation (class-of source) t)))))))
