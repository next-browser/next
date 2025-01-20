;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/history
  (:documentation "Package for `history-mode', mode to store current buffer navigation into the global history.

In addition to the external APIs listed in `history-mode' documentation, there
are useful internal APIs for history reusal:
- `load-history-url' to load the URL that's already in history.
- `with-history' and `with-history-access' macros to (respectively) get access
  to history contents and have an opportunity to modify it.

- Unexported `prompt-buffer' `prompter:source's (most corresponding to the
  respective commands) like:
  - `history-backwards-source'
  - `direct-history-forwards-source'
  - `all-history-forwards-source'
  - `history-forwards-source'
  - `history-all-owner-nodes-source'
  - `history-all-source'."))
(in-package :nyxt/mode/history)

(define-mode history-mode ()
  "Mode to manage navigation history.

`history-mode' itself has several configurable options:
- `history-blocklist' to ignore certain pages and not include them in history.
- `backtrack-to-hubs-p' to optimize the pages one often gets back to as
  \"hubs\"."
  ((visible-in-status-p nil)
   (history-blocklist '("https://duckduckgo.com/l/")
                      ;; TODO: Find a more automated way to do it.  WebKitGTK
                      ;; automatically removes such redirections from its
                      ;; history.  How?
                      :type (list-of string)
                      :documentation "URL prefixes to not save in history.
Example: DuckDuckGo redirections should be ignored or else going backward in
history after consulting a result reloads the result, not the DuckDuckGo
search.")
   (backtrack-to-hubs-p
    nil
    :type boolean
    :documentation "Go back in history when encountering an already visited page.
For example
example.com/.../1 ->  example.com/hub -> example.com/.../3 -> example.com/hub
                                                              ^ current node here

Gets optimized into this structure with this setting on:

example.com/.../1 ->  example.com/hub -> example.com/.../3
                      ^ current node here

Experimental, may not always produce intuitive enough history trees.")
   (keyscheme-map
    (define-keyscheme-map "history-mode" ()
      keyscheme:default
      (list
       "M-left" 'history-backwards
       "M-button4" 'history-backwards
       ;; this should be C-[
       "M-[" 'history-backwards
       "button8" 'history-backwards
       "M-right" 'history-forwards
       "M-button5" 'history-forwards
       ;; this should be C-]
       "M-]" 'history-forwards
       "button9" 'history-forwards
       "M-shift-left" 'history-backwards-query
       "M-shift-right" 'history-forwards-query
       "C-M-right" 'history-forwards-all-query
       "C-M-left" 'history-all-query
       "C-H" 'history-all-query)
      keyscheme:emacs
      (list
       "C-b" 'history-backwards
       "M-b" 'history-backwards-query
       "M-f" 'history-forwards-query
       "C-M-f" 'history-forwards-all-query
       "C-f" 'history-forwards-maybe-query
       "C-M-b" 'history-all-query)
      keyscheme:vi-normal
      (list
       "H" 'history-backwards
       "M-h" 'history-backwards-query
       "M-l" 'history-forwards-query
       "M-L" 'history-forwards-all-query
       "L" 'history-forwards-maybe-query
       "M-H" 'history-all-query)))))

(define-configuration context-buffer
  ((default-modes (cons 'history-mode %slot-value%))))

(-> load-history-url (nyxt::url-designator
                      &key (:buffer (maybe buffer)) (:message string))
    t)
(defun load-history-url (url-or-node
                         &key (buffer (current-buffer))
                              (message "History entry is already the current URL."))
  "Go to URL-OR-NODE's history URL (in BUFFER), if any.
Error if there's no such URL in history.
Show MESSAGE if no need to move."
  (let ((url (url url-or-node)))
    (cond
      ((not (quri:uri-p url))
       (error "Not a URL nor a history node: ~a" url-or-node))
      ((quri:uri= url (url buffer))
       (echo message))
      (t (buffer-load url :buffer buffer)))))

(defmacro with-history ((history-sym buffer) &body body)
  "Run body if BUFFER has history entries, that is, if it owns some nodes.
This does not save the history to disk."
  `(let ((,history-sym (buffer-history (or ,buffer (current-buffer)))))
     (if (and ,history-sym
              (or (not ,buffer)
                  (htree:owner ,history-sym (id ,buffer))))
         (progn ,@body)
         (echo "Buffer ~a has no history." (id ,buffer)))))

(defmacro with-history-access ((history-sym buffer) &body body)
  "Run body if BUFFER has `history-entry'es, that is, if it owns some nodes.

This saves the possibly modified history to disk when BODY exits. Possible
history modification operations are:
- `htree:forward' and `htree:backward'.
- `htree:go-to-child' and `htree:visit'.
- `htree:add-entry', `htree:add-child', `htree:add-children',
  `htree:add-owner'."
  `(files:with-file-content (,history-sym (history-file (or ,buffer (current-buffer))))
     (if (and ,history-sym
              (or (not ,buffer)
                  (htree:owner ,history-sym (id ,buffer))))
         (progn ,@body)
         (echo "Buffer ~a has no history." (id ,buffer)))))

(define-command history-backwards (&key (buffer (current-buffer)))
  "Go to parent URL of BUFFER in history."
  (when-let ((new-node
              (with-history-access (history buffer)
                (cond
                  ;; Skip going backwards if the current URL is not in history. If
                  ;; it's not in history, then going backwards means canceling the
                  ;; current load operation.
                  ((and (htree:owner-node history (id buffer))
                        (not (quri:uri= (url buffer)
                                        (url (htree:data (htree:owner-node history (id buffer)))))))
                   nil)
                  (t
                   (htree:backward history (id buffer))))
                (htree:owner-node history (id buffer)))))
    (load-history-url new-node
                      :message "No backward history.")))

;; FIXME: Sometimes this command short-circuits on the same page. htree modes
;; seems to work just fine, and the right URL is passed to `buffer-load', but it
;; loads the wrong one somewhy. Heisenbug!
(define-command history-forwards (&optional (buffer (current-buffer)))
  "Go forward one step/URL in BUFFER's history."
  (let ((new-node
          (with-history-access (history buffer)
            (htree:forward history (id buffer))
            (htree:owner-node history (id buffer)))))
    (load-history-url
     new-node
     :message "No forward history.")))

(define-class history-backwards-source (prompter:source)
  ((prompter:name "Parent URLs")
   (buffer :initarg :buffer :accessor buffer :initform nil)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (let ((owner (htree:owner history (id (buffer source)))))
          (htree:all-parents history :owner owner))))))
  (:export-class-name-p t)
  (:metaclass user-class)
  (:documentation "Source for all the parent URLs/`history-entry'es of the current buffer."))

(defmethod prompter:object-attributes ((node history-tree:node) (source prompter:source))
  (declare (ignore source))
  (let ((entry (htree:data (history-tree:entry node))))
    `(("URL" ,(render-url (url entry)))
      ("Title" ,(title entry)))))

(define-command history-backwards-query (&optional (buffer (current-buffer)))
  "Query parent URL to navigate back to."
  (let ((input (prompt1
                :prompt "Navigate backwards to"
                :sources (make-instance 'history-backwards-source
                                        :buffer buffer))))
    (when input
      (with-history-access (history buffer)
        (loop until (eq input (htree:owner-node history (id buffer)))
              do (htree:backward history (id buffer))))
      (load-history-url input))))

(define-class direct-history-forwards-source (prompter:source)
  ((prompter:name "First child of all forward-branches")
   (buffer nil :type (maybe buffer))
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (htree:children (htree:owner-node history (id (buffer source))))))))
  (:documentation "Direct children of the current `history-entry'.")
  (:export-class-name-p t)
  (:metaclass user-class))

(define-command history-forwards-direct-children (&optional (buffer (current-buffer)))
  "Query child URL to navigate to."
  (let ((input (prompt1
                :prompt "Navigate forwards to"
                :sources (make-instance 'direct-history-forwards-source
                                        :buffer buffer))))
    (when input
      (with-history-access (history buffer)
        (htree:go-to-child (htree:data input) history (id buffer)))
      (load-history-url input))))

(define-command history-forwards-maybe-query (&optional (buffer (current-buffer)))
  "If current node has multiple children, query which one to navigate to.
Otherwise go forward to the only child."
  (with-history (history buffer)
    (if (<= 2 (length
               (htree:children (htree:owner-node history (id buffer)))))
        (history-forwards-direct-children)
        (history-forwards))))

(define-class history-forwards-source (prompter:source)
  ((prompter:name "All forwards children")
   (buffer :initarg :buffer :accessor buffer :initform nil)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (htree:all-forward-children history (id (buffer source)))))))
  (:export-class-name-p t)
  (:metaclass user-class)
  (:documentation "All children URLs of the current `history-entry'."))

(define-command history-forwards-query (&optional (buffer (current-buffer)))
  "Query forward-URL to navigate to."
  (let ((input (prompt1 :prompt "Navigate forwards to"
                        :sources (make-instance 'history-forwards-source
                                                :buffer buffer))))
    (when input
      (with-history-access (history buffer)
        ;; REVIEW: Alternatively, we could use the COUNT argument with
        ;; (1+ (position input (htree:all-forward-children history (id buffer))))
        ;; Same with `history-backwards-query'.
        (loop until (eq input (htree:owner-node history (id buffer)))
              do (htree:forward history (id buffer))))
      (load-history-url input))))

(define-class all-history-forwards-source (prompter:source)
  ((prompter:name "Child URLs")
   (buffer :initarg :buffer :accessor buffer :initform nil)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (let ((owner (htree:owner history (id (buffer source)))))
          (htree:all-children history :owner owner))))))
  (:export-class-name-p t)
  (:metaclass user-class)
  (:documentation "Source for `htree:all-children' of the current `hsitory-entry'."))

(define-command history-forwards-all-query (&optional (buffer (current-buffer)))
  "Query URL to forward to, from all child branches."
  (let ((input (prompt1 :prompt "Navigate forwards to (all branches)"
                        :sources (make-instance 'all-history-forwards-source
                                                :buffer buffer))))
    (when input
      (with-history-access (history buffer)
        (htree:visit-all history (id buffer) input))
      (load-history-url input))))

(define-class history-all-owner-nodes-source (prompter:source)
  ((prompter:name "All history URLs")
   (buffer :initarg :buffer :accessor buffer :initform nil)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (htree:all-branch-nodes
         history (htree:owner history (id (buffer source))))))))
  (:export-class-name-p t)
  (:metaclass user-class)
  (:documentation "All the nodes of the current buffer."))

(define-command history-all-owner-nodes-query (&optional (buffer (current-buffer)))
  "Query URL to go to, from the list of all nodes owned by BUFFER."
  (let ((input (prompt1 :prompt "Navigate to"
                        :sources (make-instance 'history-all-owner-nodes-source
                                                :buffer buffer))))
    (when input
      (with-history-access (history buffer)
        (htree:visit-all history (id buffer) input))
      (load-history-url input))))

(define-class history-all-source (prompter:source)
  ((prompter:name "All history URLs")
   (buffer :initarg :buffer :accessor buffer :initform nil)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:constructor
    (lambda (source)
      (with-history (history (buffer source))
        (htree:all-data history)))))
  (:export-class-name-p t)
  (:metaclass user-class)
  (:documentation "Source for all the `htree:entry'es (`htree:all-data') in history."))

(define-command history-all-query (&optional (buffer (current-buffer)))
  "Query URL to go to, from the whole history."

  (let ((input (prompt1 :prompt "Navigate to"
                        :sources (make-instance 'history-all-source :buffer buffer))))
    (when input
      (with-history (history buffer)
        (when-let ((matching-node
                         (find input
                               (htree:all-owner-nodes
                                history
                                (htree:owner history (id (current-buffer))))
                               :key (compose #'htree:data #'htree:entry))))
          (htree:visit-all history (id buffer) matching-node)))
      (load-history-url (url input)))))

(defun title-or-fallback (history-entry)
  "Return HISTORY-ENTRY title or, if empty, the URL."
  (let ((title (title history-entry)))
    (if (str:emptyp title)
        (render-url (url history-entry))
        title)))

(defun render-buffer-history-tree (buffer)
  "Return the HTML presentation of BUFFER history.
Clicking on a link navigates the history in the corresponding buffer."
  (with-history (history buffer)
    (let ((current-buffer-id (id (current-buffer))))
      (spinneret:with-html-string
        (:ul (:raw (or (htree:map-owned-tree
                        #'(lambda (node)
                            (spinneret:with-html-string
                              (:li
                               (:button :class "link"
                                        :title (render-url (url (htree:data node)))
                                        :onclick (ps:ps (nyxt/ps:lisp-eval
                                                         (:title "visit-history-for-buffer")
                                                         (let ((url (url (htree:data node))))
                                                           (with-current-buffer buffer
                                                             (with-history (history buffer)
                                                               (htree:visit-all history (id buffer) node))
                                                             (load-history-url url))
                                                           (switch-buffer :buffer buffer))))
                                        (let ((title (title-or-fallback (htree:data node))))
                                          (cond
                                            ((eq node (htree:owner-node history current-buffer-id))
                                             (:i (:b title)))
                                            ((eq node (htree:owner-node history (id buffer)))
                                             (:i title))
                                            ((htree:owned-p (htree:owner history current-buffer-id) node)
                                             (:b title))
                                            (t title)))))))
                        history
                        (htree:owner history (id buffer))
                        :include-root t
                        :collect-function #'(lambda (a b) (str:concat a (when b
                                                                          (spinneret:with-html-string
                                                                            (:ul (:raw (str:join "" b))))))))
                       "")))))))

(define-internal-page buffer-history-tree (&key (id (id (current-buffer))))
    (:title "*History Tree*" :page-mode 'nyxt/mode/history-tree:history-tree-mode)
  "Display the history tree of a buffer.
ID is a `buffer''s `id'."
  (let ((buffer (nyxt::buffers-get id))
        (mode (find-submode 'nyxt/mode/history-tree:history-tree-mode)))
    (spinneret:with-html-string
      (:nstyle (style mode))
      (:h1 (format nil "History of ~a" buffer))
      (:div (if buffer
                (:raw (render-buffer-history-tree buffer))
                "Buffer no longer exists.")))))

(define-command-global buffer-history-tree (&key (buffer (current-buffer)))
  "Display the history tree of a BUFFER."
  (buffer-load-internal-page-focus 'buffer-history-tree :id (id buffer)))

(define-internal-page-command-global history-tree ()
  (output-buffer "*History*" 'nyxt/mode/history-tree:history-tree-mode)
  "Display the whole, global history tree.
It displays one branch per buffer.

As such, when nodes are shared by multiple buffers, they are displayed multiple
times.

Thus it is not representative of how the Global History Tree deduplicates nodes
internally, but this display is clearer and more navigable."
  (let ((mode (find-submode 'nyxt/mode/history-tree:history-tree-mode output-buffer)))
    (spinneret:with-html-string
     (:nstyle (style mode))
     (:body
      (render-menu 'nyxt/mode/history:history-mode output-buffer)
      (:h1 "History")
      (dolist (buffer (buffer-list))
        (:div (:raw (render-buffer-history-tree buffer))))))))

(define-internal-page-command-global list-history (&key (limit 100))
    ;; TODO: Remove list-history-mode if we add a style slot to `internal-page's.
    (buffer "*History list*" 'nyxt/mode/list-history:list-history-mode)
  "Display the user history as a list.
Cut the display at LIMIT nodes."
  (spinneret:with-html-string
    (:nstyle (style (find-submode 'nyxt/mode/list-history:list-history-mode buffer)))
    (render-menu 'nyxt/mode/history:history-mode buffer)
    (:h1 "History")
    (:table :class "resizable-table"
            (:tr (:th "Title") (:th "URL"))
            (:raw (nyxt::history-html-list :limit limit)))))

(-> history-add (quri:uri &key (:title string) (:buffer buffer)) *)
(defun history-add (url &key (title "") (buffer (current-buffer)))
  "Add URL to the global/buffer-local history.
If `backtrack-to-hubs-p' is on, go back to the matching parent instead.
The `implicit-visits' count of the `htree:current' node is incremented in any
case."
  (files:with-file-content (history (history-file (current-buffer))
                            :default (nyxt::make-history-tree))
    (unless (or (url-empty-p url)
                ;; If buffer was not registered in the global history, don't
                ;; proceed.  See `buffer's `customize-instance' `:after' method.
                (not (htree:owner history (id buffer))))
      (let* ((mode (find-submode 'history-mode))
             (parent-position (when mode
                                (and (backtrack-to-hubs-p mode)
                                     (position url
                                               (htree:all-parents history
                                                                  :owner (id buffer))
                                               :test #'quri:uri-equal
                                               :key (compose #'url #'htree:data))))))
        (if parent-position
            (htree:backward history (id buffer) (1+ parent-position))
            (htree:add-child (make-instance 'nyxt::history-entry
                                            :url url
                                            :title title)
                             history
                             (id buffer))))
      (let ((entry (htree:data (htree:current (htree:owner history (id buffer))))))
        (setf (title entry) title)
        (incf (nyxt::implicit-visits entry))))))

(export-always 'blocked-p)
(defun blocked-p (url mode)
  "Check whether URL belongs to MODE's `history-blocklist'."
  (find-if (rcurry #'str:starts-with? (render-url url))
           (history-blocklist mode)))

(defun add-url-to-history (url buffer mode)
  "Add URL to BUFFER's `history-MODE'.
Uses `history-add' internally."
  (unless (or (url-empty-p url)
              (blocked-p url mode))
    (log:debug "Notify URL ~a for buffer ~a with load status ~a"
               url
               buffer
               (slot-value buffer 'nyxt::status))
    (unless (quri:uri= url (url buffer))
      (log:debug "Picking different buffer URL instead: ~a"
                 (url buffer)))
    (when (eq (slot-value buffer 'nyxt::status) :finished)
      ;; We also add history entries here when URL changes without initiating
      ;; any load, e.g. when clicking on an anchor.
      (with-current-buffer buffer
        ;; WARNING: We add buffer's URL instead of the URL argument because in
        ;; case of some redirects (like Wikipedia's) the buffer URL is the final
        ;; one.
        (history-add (url buffer) :title (title buffer)
                                  :buffer buffer)))
    url))

(defmethod nyxt:on-signal-notify-uri ((mode history-mode) url)
  (declare (type quri:uri url))
  (let ((buffer (buffer mode)))
    (when (web-buffer-p buffer)
      (when (eq :finished (slot-value buffer 'nyxt::status))
        (add-url-to-history url buffer mode))))
  url)

(defmethod nyxt:on-signal-notify-title ((mode history-mode) title)
  ;; Title may be updated after the URL, so we need to set the history entry again
  ;; with `on-signal-notify-uri'.
  (on-signal-notify-uri mode (url (buffer mode)))
  title)

(defmethod nyxt:on-signal-load-finished ((mode history-mode) url)
  (add-url-to-history url (buffer mode) mode)
  url)

(defmethod nyxt:on-signal-load-canceled ((mode history-mode) url)
  (add-url-to-history url (buffer mode) mode)
  url)
