;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-extensions)

(defun send-event (name args)
  "Send a NAMEd event to all the extensions.
ARGS is a possibly empty list of JSON-encodeable values to pass to event
listeners."
  (flet ((send-event-message (context-name context)
           (declare (ignorable context-name))
           (webkit:webkit-web-context-send-message-to-all-extensions
            context
            (webkit:webkit-user-message-new
             "event" (glib:g-variant-new-string
                      (j:encode (sera:dict "name" name
                                           "args" (coerce args 'vector))))))))
    ;; Feels wrong and hard-coded.
    (maphash #'send-event-message (uiop:symbol-call :nyxt/renderer/gtk :ephemeral-web-contexts *browser*))
    (maphash #'send-event-message (uiop:symbol-call :nyxt/renderer/gtk :web-contexts *browser*))))


;; For window-set-buffer-hook.
(defun tabs-on-activated (window buffer)
  (let ((active-info (sera:dict "tabId" (id buffer)
                                "windowId" (id window)))
        (previous-buffer
          (first (sort (remove buffer (buffer-list)) #'>
                       :key #'last-access))))
    (when previous-buffer
      (setf (gethash "previousTabId" active-info) (id previous-buffer)))
    (send-event "tabs.onActivated" (list active-info))
    ;; Deprecated, a subset of tabs.onActivated.
    (send-event "tabs.onActiveChanged"
                (list (id buffer)
                      (sera:dict "windowId" (id window))))))

;; For buffer-make-hook.
(defun tabs-on-created (buffer)
  (send-event "tabs.onCreated" (list (buffer->tab-description buffer))))

;; For buffer-delete-hook.
(defun tabs-on-removed (buffer)
  (send-event "tabs.onRemoved"
              (list (id buffer)
                    ;; TODO: isWindowClosing.
                    (sera:dict "windowId" (id (current-window))))))

;; Call manually whenever the buffer changes.
(defun tabs-on-updated (buffer changed-field value)
  (send-event "tabs.onUpdated"
              (list (id buffer)
                    ;; REVIEW: Can there be more than one change? Yes, but
                    ;; can/should we accommodate for that?
                    (sera:dict changed-field value)
                    (buffer->tab-description buffer))))
