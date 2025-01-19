;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun reopen-dead-buffer (buffer)
  (cond ((dead-buffer-p buffer)
         (ffi-buffer-initialize-foreign-object buffer)
         ;; Re-positions buffer in `recent-buffers'.
         (add-to-recent-buffers buffer)
         ;; Ensure buffer is seen by the `buffers' hash table.
         (buffers-set (id buffer) buffer)
         (buffer-load (url buffer) :buffer buffer))
        (t (log:info "~a isn't a recently deleted buffer" buffer))))

(define-class recent-buffer-source (prompter:source)
  ((prompter:name "Deleted buffers")
   (prompter:enable-marks-p t)
   (prompter:constructor
    (containers:container->list (recent-buffers *browser*)))
   (prompter:actions-on-return
    (list
     (lambda-command reopen-dead-buffer-focus (buffer-list)
       "Reopen BUFFER and switch to it."
       (mapc #'reopen-dead-buffer buffer-list)
       (set-current-buffer (or (first (prompter:marks (current-source)))
                               (current-suggestion-value (current-prompt-buffer)))))
     (lambda-mapped-command reopen-dead-buffer)))
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)))

(define-command reopen-buffer ()
  "Reopen queried deleted buffer(s)."
  (prompt :prompt "Reopen buffer(s)" :sources 'recent-buffer-source))

(define-command reopen-last-buffer ()
  "Open a new buffer with the URL of the most recently deleted buffer."
  (if (plusp (containers:size (recent-buffers *browser*)))
      (set-current-buffer
       (reopen-dead-buffer (containers:first-item (recent-buffers *browser*))))
      (echo "There are no recently-deleted buffers.")))
