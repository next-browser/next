;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-extensions)

(defun extension->extension-info (extension)
  (when extension
    (sera:dict
     "description" (or (nyxt/web-extensions:description extension) "")
     "homepageUrl" (or (nyxt/web-extensions:homepage-url extension) "")
     "id" (id extension)
     "installType" "development"
     "mayDisable" t ;; Always.
     "name" (or (extension-name extension) "")
     "permissions" (nyxt/web-extensions:permissions extension)
     "version" (or (nyxt/web-extensions:version extension) "")
     "enabled" (enabled-p extension)
     ;; TODO: Make those meaningful.
     "disabledReason" "unknown"
     "hostPermissions" (vector)
     "icons" (vector)
     "offlineEnabled" nil
     "optionsUrl" ""
     "shortName" ""
     "type" "extension"
     "updateUrl" ""
     "versionName" "")))

(defun buffer->tab-description (buffer)
  (when buffer
    ;; FIXME: Previous version searched across all the current buffers of all
    ;; windows.
    (let* ((active (eq buffer (current-buffer)))
	   (description
	     (sera:dict
	      "active" active
	      "attention" (and active
			       (sera:true (nyxt::active-prompt-buffers (current-window))))
	      "audible" (not (ffi-muted-p buffer))
	      "height" (ffi-height buffer)
	      "width" (ffi-width buffer)
	      "highlighted" active
	      "id" (id buffer)
	      "incognito" (nosave-buffer-p buffer)
	      "lastAccessed" (* 1000.0 (time:timestamp-to-unix (nyxt::last-access buffer)))
	      "selected" active
	      "status" (if (network-buffer-p buffer)
			   (case (slot-value buffer 'nyxt::status)
			     ((:finished :failed) "complete")
			     ((:unloaded :loading) "loading"))
			   "complete")
	      ;; TODO: Check "tabs" permission for these two
	      "title"(title buffer)
	      "url" (render-url (url buffer))
	      "mutedInfo" (sera:dict "muted" (ffi-muted-p buffer))
	      "windowId" (id (current-window))
	      ;; TODO: Make these meaningful:
	      "autoDiscardable" nil
	      ;; "cookieStoreId" -1
	      "currentWindow" t
	      "discarded" nil
	      "hidden" nil
	      ;; "favIconUrl" ""
	      "index" 0
	      "isArticle" nil
	      "isInReaderMode" nil
	      ;; "sessionId" -1
	      ;;"successorTabId" -1
	      "pinned" nil)))
      (sera:and-let* ((history (buffer-history buffer))
		      (owner (htree:owner history (id buffer)))
		      (parent (htree:owner history (htree:creator-id owner))))
	(setf (gethash "openerTabId" description)
	      (htree:creator-id owner)))
      description)))

(defun all-extensions (&key (buffers (buffer-list)))
  (loop for buffer in buffers
        when (modable-buffer-p buffer)
          append (sera:filter #'nyxt/web-extensions::extension-p (modes buffer))))

(defmacro fire-extension-event (extension object event &rest args)
  (alex:once-only (extension)
    `(if (ffi-buffer-evaluate-javascript
          (buffer ,extension)
          (ps:ps (ps:instanceof (ps:chain browser ,object ,event) *Object))
          (extension-name ,extension))
         (ffi-buffer-evaluate-javascript
          (buffer ,extension)
          (ps:ps (ps:chain browser ,object ,event
                           (run ,@args)))
          (extension-name ,extension))
         (log:debug "Event not injected: ~a" (ps:ps (ps:@ ,object ,event))))))

(defmethod tabs-on-activated ((old-buffer buffer) (new-buffer buffer))
  (flet ((integer-id (object)
           (or (ignore-errors (parse-integer (id object)))
               0)))
    (dolist (extension (all-extensions))
      (fire-extension-event
       extension tabs on-activated
       (ps:create previous-tab-id (ps:lisp (integer-id old-buffer))
                  tab-id (ps:lisp (integer-id new-buffer))
                  window-id (ps:lisp (integer-id (current-window)))))
      ;; tabs.onActiveChanged is deprecated. No harm in having it, though.
      (fire-extension-event
       extension tabs on-active-changed
       (ps:lisp (integer-id new-buffer))
       ;; FIXME: Any way to get the window buffer belongs to?
       (ps:create window-id (ps:lisp (integer-id (current-window))))))))

(defmethod tabs-on-created ((buffer buffer))
  (dolist (extension (all-extensions))
    (fire-extension-event
     extension tabs on-created
     ;; buffer->tab-description returns the representation that Parenscript has
     ;; trouble encoding, thus this JSON parsing hack.
     (ps:chain *j-s-o-n (parse (ps:lisp (j:encode (buffer->tab-description (buffer extension)))))))))

(defmethod tabs-on-updated ((buffer buffer) properties)
  "Invoke the browser.tabs.onUpdated event with PROPERTIES being an alist of BUFFER changes."
  (dolist (extension (all-extensions))
    (fire-extension-event
     extension tabs on-updated
     (ps:lisp (parse-integer (id buffer)))
     (ps:chain *j-s-o-n (parse (ps:lisp (j:encode properties))))
     ;; buffer->tab-description returns the representation that Parenscript has
     ;; trouble encoding, thus this JSON parsing hack.
     (ps:chain *j-s-o-n (parse (ps:lisp (j:encode (buffer->tab-description (buffer extension)))))))))

(defmethod tabs-on-removed ((buffer buffer))
  (flet ((integer-id (object)
           (or (ignore-errors (parse-integer (symbol-name (id object))))
               0)))
    (dolist (extension (all-extensions))
      (fire-extension-event
       extension tabs on-removed
       (ps:lisp (integer-id buffer))
       (ps:create window-id (integer-id (current-window))
                  ;; FIXME: How do we know if it's closing?
                  is-window-closing false)))))

(defun tabs-query (query-object)
  (let ((descriptions (mapcar #'buffer->tab-description (buffer-list)))
	(meaninful-props '("active" "audible" "currentWindow" "hidden" "highlighted" "status" "windowId"
			   ;; "url" "title" ;; Should be patterns
			   ;; "autoDiscardable" "cookieStoreId" "discarded"
			   ;; "muted" "lastFocusedWindow" "pinned" "windowType"
			   )))
    (if query-object
	(loop for prop in meaninful-props
	      do (setf descriptions
		       (remove-if (lambda (d)
				    (and (nth-value 1 (gethash prop d))
					 (nth-value 1 (gethash prop query-object))
					 (not (equal (gethash prop d)
						     (gethash prop query-object)))))
				  descriptions))
	      finally (return descriptions))
	descriptions)))

(defun tabs-create (properties)
  (j:bind ("openerTabId" (opener-tab) "url" (url) "title" (title)
	    "active" (active-p) "selected" (selected-p) "discarded" (discarded-p) "muted" (muted-p))
    properties
    (let* ((parent-buffer (when opener-tab
			    (nyxt::buffers-get opener-tab)))
	   (url (quri:uri (or url "about:blank")))
	   ;; See https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/API/tabs/create
	   (url (if (str:s-member '("chrome" "javascript" "data" "file") (quri:uri-scheme url))
		    (quri:uri "about:blank")
		    url))
	   (buffer (make-buffer :url url
				:title (or title "")
				:load-url-p (not discarded-p)
				:parent-buffer parent-buffer)))
      ;; FIXME: passing it as `:modes' to `make-buffer' doesn't work...
      (when muted-p
	(nyxt/mode/no-sound:no-sound-mode :buffer buffer))
      (when (or active-p selected-p)
	(set-current-buffer buffer))
      (buffer->tab-description buffer))))

(defvar %message-channels% (make-hash-table)
  "A hash-table mapping message pointer addresses to the channels they return values from.

Introduced to communicate `process-user-message' and `reply-user-message'
running on separate threads. These run on separate threads because we need to
free GTK main thread to allow JS callbacks to run freely.")

(-> trigger-message (t buffer nyxt/web-extensions:extension webkit:webkit-user-message) string)
(defun trigger-message (message buffer extension original-message)
  "Send a MESSAGE to the WebKitWebPage associated with BUFFER and wait for the result.

Respond to ORIGINAL-MESSAGE once there's a result.

See `%message-channels%',`process-user-message', and `reply-user-message' for
the description of the mechanism that sends the results back."
  (let ((result-channel (nyxt::make-channel 1)))
    (run-thread
        "Send the message"
      (flet ((send-message (channel)
               (ffi-web-extension-send-message
                buffer
                (webkit:webkit-user-message-new
                 "message"
                 (glib:g-variant-new-string
                  (j:encode `(("message" . ,message)
                              ("sender" . (("tab" . ,(buffer->tab-description buffer))
                                           ("url" . ,(render-url (url buffer)))
                                           ("tlsChannelId" . "")
                                           ("frameId" . 0)
                                           ("id" . "")))
                              ("extensionName" . ,(extension-name extension))))))
                (lambda (reply)
                  (calispel:! channel (webkit:g-variant-get-maybe-string
                                       (webkit:webkit-user-message-get-parameters reply))))
                (lambda (condition)
                  (echo-warning "Message error: ~a" condition)
                  ;; Notify the listener that we are done.
                  (calispel:! channel nil)))))
        (if (not (member (slot-value buffer 'nyxt::status) '(:finished :failed)))
            (let ((channel (nyxt::make-channel 1)))
              (hooks:once-on (buffer-loaded-hook buffer) _
                (calispel:! channel (send-message result-channel)))
              (calispel:? channel))
            (send-message result-channel))))
    (setf (gethash (cffi:pointer-address (g:pointer original-message)) %message-channels%)
          result-channel))
  "")

(defun args->buffer+payload (args)
  (j:match args
    (#(a :null)
      (values (current-buffer) a))
    (#(id a)
      (values (nyxt::buffers-get id) a))))

(defvar %style-sheets% (make-hash-table :test #'equal)
  "Injected WebKitUserStyleSheet-s indexed by the JSON strings describing them.")

(defun tabs-insert-css (extension args)
  (multiple-value-bind (buffer params)
      (args->buffer+payload args)
    ;; FIXME: frameId, matchAboutBlank, runAt are not supported.
    (j:bind ("code" (code) "file" (file)
	      "allFrames" (all-frames-p)
	      "cssOrigin" (level))
      (let ((style-sheet
	      (ffi-buffer-add-user-style
               buffer
               (apply #'make-instance
                      'nyxt/mode/user-script:user-style
                      :level (if (not (and level
					   (stringp level)
					   (string= level "user")))
                                 :author
                                 :user)
                      :all-frames-p all-frames-p
                      :world-name (extension-name extension)
                      (if file
                          (list :base-path
                                (uiop:merge-pathnames*
                                 file (nyxt/web-extensions:extension-directory
                                       extension)))
                          (list :code code))))))
	(setf (gethash (j:encode params) %style-sheets%)
	      style-sheet)
	:null))))

(defun tabs-remove-css (args)
  (multiple-value-bind (buffer params)
      (args->buffer+payload args)
    (let* ((json (j:encode params))
	   (style-sheet (gethash json %style-sheets%)))
      (ffi-buffer-remove-user-style buffer style-sheet)
      (remhash json %style-sheets%)
      :null)))

(defun tabs-execute-script (extension args)
  (multiple-value-bind (buffer params)
      (args->buffer+payload args)
    ;; FIXME: Support matchAboutBlank?
    (j:bind ("code" (code) "file" (file)
	      "allFrames" (all-frames-p) "frameId" (frame-id)
	      "runAt" (run-at))
      params
      ;; TODO: permissions (once refactored).
      (ffi-buffer-add-user-script
       buffer
       (make-instance
	'nyxt/mode/user-script:user-script
	:code (if file
		  (uiop:read-file-string
		   (nyxt/web-extensions:merge-extension-path extension file))
		  code)
	:run-at (if (and run-at (string= run-at "document_start"))
		    :document-start
		    :document-end)
	:all-frames-p (or all-frames-p
			  (and frame-id
			       (not (zerop frame-id))))
	:world-name (extension-name extension)))
      ;; TODO: Collect results somehow?
      #())))

(defun storage-local-get (buffer message-params)
  (let* ((json (j:decode message-params))
         (extension (find (j:get "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal))
         (keys (j:get "keys" json)))
    (let ((data (or (files:content (nyxt/web-extensions:storage-path extension))
                    (make-hash-table))))
      (if (uiop:emptyp keys)
	  (sera:dict)
	  (typecase keys
	    (null data)
	    (list (mapcar (lambda (key-value)
			    (let ((key-value (uiop:ensure-list key-value))
				  (value (or (gethash (first key-value) data)
					     (rest key-value))))
			      (when value
				(cons (first key-value) value))))
			  keys))
	    (string (or (gethash keys data)
			(vector))))))))

(defun storage-local-set (buffer message-params)
  (let* ((json (j:decode message-params))
         (extension (find (j:get "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal))
         (keys (j:get "keys" json)))
    (let ((data (or (files:content (nyxt/web-extensions:storage-path extension))
                    (make-hash-table))))
      (unless (uiop:emptyp keys)
        (dolist (key-value keys)
          (setf (gethash (first key-value) data)
                (rest key-value))))))
  :null)

(defun storage-local-remove (buffer message-params)
  (let* ((json (j:decode message-params))
         (extension (find (j:get "extensionId" json)
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id
                          :test #'string-equal))
         (keys (uiop:ensure-list (j:get "keys" json))))
    (let ((data (or (files:content (nyxt/web-extensions:storage-path extension))
                    (make-hash-table))))
      (unless (uiop:emptyp keys)
        (dolist (key keys)
          (remhash key data)))))
  :null)

(defun storage-local-clear (buffer message-params)
  (let* ((extension (find message-params
                          (sera:filter #'nyxt/web-extensions::extension-p
                                       (modes buffer))
                          :key #'id)))
    (let ((data (or (files:content (nyxt/web-extensions:storage-path extension))
                    (make-hash-table))))
      (clrhash data)))
  "")

(defun %process-user-message (extension name args)
  "Process the NAMEd message intended for EXTENSION."
  (str:string-case name
    ("management.getSelf"
     (extension->extension-info extension))
    ("runtime.getPlatformInfo"
     (sera:dict
      "os"
      #+darwin "mac"
      #+(or openbsd freebsd) "openbsd"
      #+linux "linux"
      #+windows "win"
      "arch"
      #+X86-64 "x86-64"
      #+(or X86 X86-32) "x86-32"
      #+(or arm arm64) "arm"))
    ("runtime.getBrowserInfo"
     (multiple-value-bind (major minor patch commit)
	 (nyxt::version)
       (sera:lret ((info (sera:dict
			  "name" "Nyxt"
			  "vendor" "Atlas Engineer LLC"
			  "version" (format nil "~d.~d.~d" major (or minor 0) (or patch 0)))))
	 (when commit
	   (setf (gethash "build" info) commit)))))
    ;; ("storage.local.get"
    ;;  (storage-local-get buffer message-params))
    ;; ("storage.local.set"
    ;;  (storage-local-set buffer message-params))
    ;; ("storage.local.remove"
    ;;  (storage-local-remove buffer message-params))
    ;; ("storage.local.clear"
    ;;  (storage-local-clear buffer message-params))
    ("tabs.query"
     (tabs-query (elt args 0)))
    ("tabs.create"
     (tabs-create (elt args 0)))
    ("tabs.getCurrent"
     (buffer->tab-description (buffer extension)))
    ("tabs.print"
     (nyxt/mode/document:print-buffer)
     :null)
    ("tabs.get"
     (buffer->tab-description (nyxt::buffers-get (elt args 0))))
    ;; ("tabs.insertCSS"
    ;;  (tabs-insert-css buffer message-params))
    ;; ("tabs.removeCSS"
    ;;  (reply (tabs-remove-css message-params)))
    ("tabs.executeScript"
     (tabs-execute-script extension args))
    ))

(export-always 'process-user-message)
(defun process-user-message (buffer message)
  "A dispatcher for all the possible WebExtensions-related message types there can be.
Uses name of the MESSAGE as the type to dispatch on."
  (let* ((message-name (webkit:webkit-user-message-get-name message))
         (message-params (webkit:g-variant-get-maybe-string
                          (webkit:webkit-user-message-get-parameters message)))
	 (params (j:decode message-params))
	 (extension-name (gethash "extension" params))
	 (extension (find extension-name
			  (sera:filter #'nyxt/web-extensions::extension-p (modes buffer))
			  :key #'extension-name
			  :test #'string-equal))
	 (args (gethash "args" params)))
    (log:debug "Message ~a with ~s parameters received."
               message-name message-params)
    (webkit:webkit-user-message-send-reply
     message
     (webkit:webkit-user-message-new
      message-name (glib:g-variant-new-string
		    (j:encode (sera:dict "result"
					 (%process-user-message extension message-name args))))))))

(export-always 'reply-user-message)
(-> reply-user-message (buffer webkit:webkit-user-message) t)
(defun reply-user-message (buffer message)
  "Send the response to the MESSAGE received from the BUFFER-associated WebPage.
Wait on the channel associated to the MESSAGE until there's a result.
Time out and send an empty reply after 5 seconds of waiting."
  (declare (ignore buffer))
  (loop until (gethash (cffi:pointer-address (g:pointer message))
                       %message-channels%)
        finally (let* ((reply (calispel:? (gethash (cffi:pointer-address (g:pointer message))
                                                   %message-channels%)
                                          5))
                       (reply-message (webkit:webkit-user-message-new
                                       (webkit:webkit-user-message-get-name message)
                                       (if reply
                                           (glib:g-variant-new-string reply)
                                           (cffi:null-pointer)))))
                  (webkit:webkit-user-message-send-reply message reply-message))))
