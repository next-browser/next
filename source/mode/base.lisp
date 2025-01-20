;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-mode base-mode ()
  "Bind general-purpose commands defined by `define-command'.
This mode is a good candidate to be passed to `make-buffer'."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (keyscheme-map
    (define-keyscheme-map "base-mode" ()
      keyscheme:default
      (list
       "C-l" 'set-url
       "M-l" 'set-url-new-buffer
       "C-r" 'reload-current-buffer
       "M-r" 'reload-buffers
       "C-shift-tab" 'switch-buffer-previous
       "C-tab" 'switch-buffer-next
       "C-T" 'reopen-buffer
       "C-t" 'make-buffer-focus
       "M-o" 'toggle-prompt-buffer-focus
       "f1 r" 'manual
       "f1 t" 'tutorial
       "f1 a" 'describe-any
       "f1 b" 'describe-bindings
       "f1 C" 'describe-class
       "f1 c" 'describe-command
       "f1 f" 'describe-function
       "f1 k" 'describe-key
       "f1 p" 'describe-package
       "f1 s" 'describe-slot
       "f1 v" 'describe-variable
       "f11" 'toggle-fullscreen
       "C-Y" 'nyxt/mode/download:list-downloads
       "C-space" 'execute-command
       "C-M-space" 'execute-extended-command
       "C-shift-space" 'execute-predicted-command)
      keyscheme:cua
      (list
       "f5" 'reload-current-buffer
       "C-/" 'reopen-buffer
       "C-[" 'switch-buffer-previous
       "C-]" 'switch-buffer-next
       "C-pagedown" 'switch-buffer-next
       "M-down" 'switch-buffer
       "C-`" 'switch-buffer-last
       "C-pageup" 'switch-buffer-previous
       "C-w" 'delete-current-buffer
       "C-n" 'make-window
       "C-W" 'delete-current-window
       "M-w" 'delete-window
       "M-c l" 'copy-url
       "M-c t" 'copy-title
       "C-O" 'load-file
       "C-o" 'nyxt/mode/file-manager:open-file
       "C-q" 'quit)
      keyscheme:emacs
      (list
       "C-x left" 'switch-buffer-previous
       "C-x C-left" 'switch-buffer-previous
       "C-x right" 'switch-buffer-next
       "C-x C-right" 'switch-buffer-next
       "C-x o" 'toggle-prompt-buffer-focus
       "C-x b" 'switch-buffer
       "C-x k" 'delete-buffer
       "C-x C-k" 'delete-current-buffer
       "C-x C-b" 'nyxt/mode/buffer-listing::list-buffers
       "C-M-l" 'copy-url
       "C-M-t" 'copy-title
       "C-h t" 'tutorial
       "C-h r" 'manual
       "C-h a" 'describe-any
       "C-h b" 'describe-bindings
       "C-h C" 'describe-class
       "C-h c" 'describe-command
       "C-h f" 'describe-function
       "C-h k" 'describe-key
       "C-h p" 'describe-package
       "C-h s" 'describe-slot
       "C-h v" 'describe-variable
       "C-d" 'nyxt/mode/download:list-downloads
       "C-x 5 2" 'make-window
       "C-x 5 0" 'delete-current-window
       "C-x 5 1" 'delete-window
       "C-x C-f" 'nyxt/mode/file-manager:open-file
       "M-x" 'execute-command
       "C-M-x" 'execute-extended-command
       "M-1" (read-from-string "nyxt/mode/repeat:repeat-key")
       "M-2" (read-from-string "nyxt/mode/repeat:repeat-key")
       "M-3" (read-from-string "nyxt/mode/repeat:repeat-key")
       "M-4" (read-from-string "nyxt/mode/repeat:repeat-key")
       "M-5" (read-from-string "nyxt/mode/repeat:repeat-key")
       "M-6" (read-from-string "nyxt/mode/repeat:repeat-key")
       "M-7" (read-from-string "nyxt/mode/repeat:repeat-key")
       "M-8" (read-from-string "nyxt/mode/repeat:repeat-key")
       "M-9" (read-from-string "nyxt/mode/repeat:repeat-key")
       "C-x C-c" 'quit)
      keyscheme:vi-normal
      (list
       "o" 'set-url
       "O" 'set-url-new-buffer
       "R" 'reload-current-buffer
       "r" 'reload-buffers
       "u" 'reopen-buffer
       "g b" 'switch-buffer
       "[" 'switch-buffer-previous
       "]" 'switch-buffer-next
       "d" 'delete-buffer
       "D" 'delete-current-buffer
       "B" 'make-buffer-focus
       "W" 'make-window
       "C-w C-w" 'make-window
       "C-w q" 'delete-current-window
       "C-w C-q" 'delete-window
       "y u" 'copy-url
       "y t" 'copy-title
       ":" 'execute-command
       "1" (read-from-string "nyxt/mode/repeat:repeat-key")
       "2" (read-from-string "nyxt/mode/repeat:repeat-key")
       "3" (read-from-string "nyxt/mode/repeat:repeat-key")
       "4" (read-from-string "nyxt/mode/repeat:repeat-key")
       "5" (read-from-string "nyxt/mode/repeat:repeat-key")
       "6" (read-from-string "nyxt/mode/repeat:repeat-key")
       "7" (read-from-string "nyxt/mode/repeat:repeat-key")
       "8" (read-from-string "nyxt/mode/repeat:repeat-key")
       "9" (read-from-string "nyxt/mode/repeat:repeat-key")
       "Z Z" 'quit))))
  (:toggler-command-p nil))
