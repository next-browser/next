;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-extensions)

;; TODO: We need to allow modifying :nyxt form here, for instance because we
;; create a `background-buffer' accessor on what is just a class in `nyxt'.
#+sb-package-locks
(sera:eval-always
  (sb-ext:add-implementation-package :nyxt/web-extensions :nyxt))

(sera:eval-always
  (define-class content-script ()
    ((match-patterns (error "Match pattern is required.")
                     :type list
                     :documentation "The match-pattern for URLs the script should run on.
Not a WebExtensions match pattern, but a renderer-friendly one.")
     (files nil
            :type list
            :documentation "The .js or .css files of the script as a list.")
     (user-styles nil
                  :type list
                  :documentation "The renderer-friendly representation of the CSS style sheets.
A list of objects. Does not necessarily have the same order as `files' of the script.")
     (user-scripts nil
                   :type list
                   :documentation "The renderer-friendly representation of the JS scripts.
A list of objects. Does not necessarily have the same order as `files' of the script."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)))

(defmethod remove-content-script ((buffer buffer) extension (script content-script))
  (declare (ignore extension))
  (dolist (s (user-scripts script))
    (ffi-buffer-remove-user-script buffer s))
  (dolist (s (user-styles script))
    (ffi-buffer-remove-user-style buffer s)))

(defmethod inject-content-script ((buffer buffer) extension (script content-script))
  "Inject scripts/style-sheets of a SCRIPT into where they belong."
  (remove-content-script buffer extension script)
  (dolist (file (files script))
    (if (equal (pathname-type file) "css")
        (pushnew (ffi-buffer-add-user-style
                  buffer (make-instance 'nyxt/mode/user-script:user-style
                                        :base-path (merge-extension-path extension file)
                                        :world-name (extension-name extension)
                                        :include (match-patterns script)))
                 (user-styles script)
                 :test #'string= :key #'nyxt/mode/user-script:code)
        ;; FIXME: Doesn't apply somewhy?
        (pushnew
         (ffi-buffer-add-user-script
          buffer (make-instance 'nyxt/mode/user-script:user-script
                                :code (uiop:read-file-string
                                       (merge-extension-path extension file))
                                :all-frames-p t
                                :world-name (extension-name extension)
                                :run-at :document-end
                                :include (match-patterns script)))
         (user-scripts script)
         :test #'string= :key #'nyxt/mode/user-script:code))))

(defun make-content-script (json)
  "Create a Lisp-friendly content script representation of our WebExtension keys.

MATCHES, JS, and CSS are all keys of the \"content_scripts\" manifest.json keys:
https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Content_scripts"
  (flet ((->list (thing)
           (coerce thing 'list)))
    (make-instance
     'content-script
     :match-patterns (map 'list #'(lambda (m) (if (equal "<all_urls>" m) "*://*/*" m))
                          (gethash "matches" json))
     :files (append (->list (gethash "js" json)) (->list (gethash "css" json))))))

(defun make-data-url (file &optional mime-type)
  "Create a data: URL with contents taken from FILE.
Can have:
- plain text contents (for text/* MIME-TYPE).
- base64 encoded contents (for everything else)."
  (let* ((type (or mime-type (mimes:mime file)))
         (binary-p (not (str:starts-with-p "text" type))))
    (format nil "data:~a~@[;base64~*~],~a"
            type binary-p (if binary-p
                              (base64:usb8-array-to-base64-string
                               (alex:read-file-into-byte-vector file))
                              (quri:url-encode (uiop:read-file-string file))))))

(defun default-browser-action-icon (json optimal-height)
  "Find the best browser action icon using OPTIMAL-HEIGHT of `status-buffer'.

JSON is the parsed extension manifest."
  (handler-case
      (flet ((first-sort-by-height (list)
               (rest (elt (sera:sort-new
                           list (lambda (a b)
                                  (> (abs (- optimal-height a))
                                     (abs (- optimal-height b))))
                           :key (compose #'parse-integer #'first))
                          0))))
        (or (sera:and-let* ((browser-action (gethash "browser_action" json))
                            (default-icon (gethash "default_icon" browser-action)))
              (if (stringp default-icon)
                  default-icon
                  (first-sort-by-height (when (gethash "default_icon" browser-action)
                                          (alex:hash-table-alist (gethash "default_icon" browser-action))))))
            (sera:and-let* ((icons (gethash "icons" json)))
              (first-sort-by-height (alex:hash-table-alist (gethash "icons" json))))))
    (error ()
      (rest (first (and (gethash "icons" json)
                        (alex:hash-table-alist (gethash "icons" json))))))))

(defun encode-browser-action-icon (json extension-directory)
  "Return the proper <img> HTML with the embedded browser action icon.

There's no way to pass local resources into web view, that's why we're
hacking into it with data: URLs and encode icons into base64 there."
  (let* ((status-buffer-height (ffi-height (status-buffer (current-window))))
         (padded-height (- status-buffer-height 10))
         (best-icon
           (default-browser-action-icon json padded-height)))
    (format nil "<img src=\"~a\" alt=\"~a\"
height=~a/>"
            ;; Extension does not exist yet, so we cannot use `merge-extension-path'.
            (make-data-url (uiop:merge-pathnames* best-icon extension-directory))
            (gethash "name" json)
            padded-height)))

(defun make-browser-action (json)
  "A helper function to construct `browser-action' from the manifest JSON.

For info on its structure, see
https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/manifest.json/browser_action"
  (sera:and-let* ((browser-action (gethash "browser_action" json)))
    (j:bind ("theme_icons" (icons) "default_popup" (popup) "default_title" (title))
      browser-action
      (let* ((sorted-icons (unless (uiop:emptyp icons)
			     (sort (coerce icons 'list)
				   #'> :key (curry #'gethash "size"))))
	     (max-icon (first sorted-icons))
	     (default-icon (default-browser-action-icon json 1000)))
	(make-instance 'browser-action
		       :default-popup popup
		       :default-title title
		       :default-icon default-icon
		       :default-dark-icon (when (hash-table-p max-icon)
					    (or (gethash "dark" max-icon)
						(gethash "light" max-icon)))
		       :default-light-icon (when (hash-table-p max-icon)
					     (or (gethash "light" max-icon)
						 (gethash "dark" max-icon))))))))
(define-class browser-action ()
  ((default-popup
    nil
    :type (or null string pathname)
    :documentation "An HTML file for the popup to open when its icon is clicked.")
   (default-title
    nil
    :type (or null string)
    :documentation "The title to label the popup with.")
   (default-icon
    nil
    :type (or null string)
    :documentation "The extension icon to use in status buffer.")
   ;; TODO: Use those.
   (default-light-icon
    nil
    :type (or null string)
    :documentation "The extension icon for use in status buffer in the light theme.")
   (default-dark-icon
    nil
    :type (or null string)
    :documentation "The extension icon for use in status buffer in the dark theme."))
  (:export-class-name-p t)
  (:export-accessor-names-p t))

(define-class extension-storage-file (files:data-file nyxt-file)
  ((files:name "extension-storage")
   (extension-name ""))
  (:export-class-name-p t)
  (:documentation "The `nyxt-file' for the browser.storage API data storage (see
  https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/API/storage
  for API description)."))

(defmethod files:resolve ((profile nyxt-profile) (file extension-storage-file))
  (sera:path-join
   (call-next-method)
   (uiop:ensure-directory-pathname (files:name file))
   (uiop:strcat (extension-name file) ".txt")))

(define-mode extension ()
  "The base mode for any extension to inherit from."
  ((visible-in-status-p t)
   (extension-name (error "Extension should have a name")
                   :type string)
   (version (error "Extension should have a version")
            :type string)
   (manifest nil
             :type (or null string)
             :documentation "Original contents of the manifest.json file.")
   (id nil
       :type (or null string)
       :documentation "A unique ID of the extension.
Is shared between all the instances of the same extension.")
   (background-buffer nil
                      :export nil
                      :documentation "The buffer to host background page of the extension in.
Is shared between all the instances of the same extension.")
   (popup-buffer nil
                 :type (or null panel-buffer)
                 :documentation "The panel-buffer to host extension popup in.
Is only set when popup is active.")
   (description nil
                :type (or null string))
   (homepage-url nil
                 :type (or null quri:uri))
   (extension-directory nil
                        :type (or null pathname)
                        :documentation "The directory that the extension resides in.")
   (extension-files (make-hash-table)
                    :type hash-table
                    :documentation "All the files extension packages.
Key is the relative path to the file.
Value is the loadable URL of that file.")
   (permissions nil
                :type (list-of string)
                :documentation "List of API permissions extension requires.")
   (content-scripts nil
                    :type (list-of content-script)
                    :documentation "A list of `content-script's used by this extension.")
   (browser-action nil
                   :type (or null browser-action)
                   :documentation "Configuration for popup opening on extension icon click.")
   (storage-path nil
                 :type (or null extension-storage-file)
                 :documentation "Path that the storage API stores data in."))
  (:toggler-command-p nil))

(defmethod enable ((mode extension) &key)
  ;; Inject extension APIs (browser etc.)
  (nyxt:ffi-web-extension-send-message
   (buffer mode)
   (webkit:webkit-user-message-new
    "addExtension" (glib:g-variant-new-string (or (manifest mode) "")))
   nil nil)
  (dolist (script (content-scripts mode))
    (inject-content-script (buffer mode) mode script))
  (unless (background-buffer mode)
    ;; Need to set it to something to not trigger this in other instances.
    (setf (background-buffer mode) t)
    (setf (background-buffer mode) (make-background-buffer)))
  (hooks:add-hook (window-set-buffer-hook (current-window))
                  'tabs-on-activated)
  ;; This is to outsmart WebKit resource loading policy by creating data: URLs.
  (setf (extension-files mode)
        (alex:alist-hash-table
         (mapcar (lambda (file)
                   (let ((relative-path
                           (str:replace-all
                            (namestring (extension-directory mode))
                            "" (namestring file))))
                     (cons relative-path
                           (if (equal (mimes:mime file) "text/html")
                               (format nil "file://~a" file)
                               (make-data-url file)))))
                 (nyxt/mode/file-manager:recursive-directory-elements
                  (extension-directory mode))))))

(defmethod disable ((mode extension) &key)
  (dolist (script (content-scripts mode))
    (remove-content-script (buffer mode) mode script))
  (nyxt:ffi-web-extension-send-message
   (nyxt:current-buffer)
   (webkit:webkit-user-message-new
    "removeExtension" (glib:g-variant-new-string (or (manifest mode) "")))
   nil nil))

(defmethod initialize-instance :after ((mode extension) &key)
  (when (eq 'extension (sera:class-name-of mode))
    (error "Cannot initialize `extension', you must subclass it.")))

(export-always 'has-permission-p)
(defmethod has-permission-p ((extension extension) (permission string))
  (str:s-member (permissions extension) permission))

(export-always 'host-permission-holds-p)
(defmethod host-permission-holds-p ((extension extension) (buffer buffer))
  "Test whether any of the host permissions EXTENSION has hold for a given BUFFER.

For more info on host permissions see
https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/manifest.json/permissions#host_permissions"
  (flet ((match-pattern->regexp (match-pattern)
           (str:replace-using '("*." "*"
                                "*" ".*"
                                "?" ".?")
                              match-pattern)))
    (dolist (permission (permissions extension))
      (alex:when-let* ((match-pattern-p (or (search "*" permission)
                                            (search "?" permission)
                                            (valid-url-p permission)))
                       (matches-p (ppcre:all-matches (match-pattern->regexp permission)
                                                     (render-url (url buffer)))))
        (return-from host-permission-holds-p t)))))

(export-always 'tab-apis-enabled-p)
(defmethod tab-apis-enabled-p ((extension extension) (buffer buffer))
  "Test whether the privileged browser.tabs API pieces can be used."
  (or (has-permission-p extension "<all_urls>")
      (has-permission-p extension "tabs")
      (and (has-permission-p extension "activeTab")
           (eq (current-buffer) buffer))
      (host-permission-holds-p extension buffer)))

(export-always 'merge-extension-path)
(defmethod merge-extension-path ((extension extension) path)
  "Create the absolute path to one of EXTENSION sub-files.

PATH should be a relative path, possibly with the leading forward
slash. WebExtensions require this :/"
  (uiop:merge-pathnames* (string-left-trim "/" (namestring path))
                         (extension-directory extension)))

(defmethod nyxt:mode-status ((status status-buffer) (extension extension))
  (spinneret:with-html-string
    (:button :class "button"
             :onclick (ps:ps (nyxt/ps:lisp-eval
                              (:title "toggle-extension-popup"
                               :buffer status)
                              (toggle-extension-popup (sera:class-name-of extension))))
             :title (format nil "Open the browser action of ~a" extension)
             (call-next-method))))

(define-command-global toggle-extension-popup (&optional extension-class (buffer (current-buffer)))
  "Open the popup of the extension of EXTENSION-CLASS.
If the popup already exists, close it."
  (let ((extension-class (or extension-class
                             (prompt1
                              :prompt "Extension to toggle the popup of"
                              :sources (make-instance 'active-mode-source
                                                      :constructor (sera:filter
                                                                    #'nyxt/web-extensions::extension-p
                                                                    (modes buffer)))))))
    (with-current-buffer buffer
      ;;TODO: Send click message to background script if there's no popup.
      (sera:and-let* ((extension (nyxt:find-submode extension-class (nyxt:current-buffer)))
                      (browser-action (browser-action extension))
                      (default-popup (default-popup browser-action)))
        (alex:if-let ((existing-popup
                       (find (default-title (browser-action extension))
                             (nyxt::panel-buffers (current-window))
                             :key #'title
                             :test #'equal)))
          (nyxt::window-delete-panel-buffer (current-window) existing-popup)
          (let ((popup (make-instance 'panel-buffer
                                      :title (default-title (browser-action extension))
                                      :id (nyxt::new-id)
                                      :default-modes (list extension-class))))
            (setf (popup-buffer extension) popup)
            (nyxt::window-add-panel-buffer
             (current-window) popup
             :right)
            (buffer-load (quri.uri.file:make-uri-file :path (merge-extension-path extension default-popup))
                         :buffer popup)))))))

(export-always 'load-web-extension)
(defmacro load-web-extension (lispy-name directory)
  "Make an extension from DIRECTORY accessible as Nyxt mode (under LISPY-NAME).
DIRECTORY should be the one containing manifest.json file for the extension in question."
  (let* ((directory (if (pathnamep directory)
			directory
			(uiop:parse-native-namestring directory)))
         (manifest-text (uiop:read-file-string (uiop:merge-pathnames* "manifest.json" directory)))
         (json (j:decode manifest-text))
         (name (j:get "name" json)))
    `(progn
       (define-mode ,lispy-name (extension)
         ,(j:get "description" json)
         ((extension-name ,name)
          (version ,(j:get "version" json))
	  ;; Necessary to strip off all the comments from the manifest and
	  ;; process it into valid standard JSON.
          (manifest ,(j:encode json))
          ;; This :allocation :class is to ensure that the instances of the same
          ;; extension class have the same ID, background-buffer, popup-buffer,
          ;; and storage-path and can communicate properly.
          (id (or (symbol-name (gensym ,name)))
              :allocation :class)
          (background-buffer nil
                             :export nil
                             :allocation :class)
          (popup-buffer nil
                        :allocation :class)
          (storage-path (make-instance 'extension-storage-file :extension-name ,name)
                        :allocation :class)
          (description ,(j:get "description" json))
          (extension-directory ,directory)
          (homepage-url ,(j:get "homepage_url" json))
          (browser-action ,(make-browser-action json))
          (permissions (quote ,(j:get "permissions" json)))
          (content-scripts (list ,@(map 'list #'make-content-script
                                        (j:get "content_scripts" json)))))))))

(define-internal-scheme "web-extension"
    (lambda (url buffer)
      (let ((data "<h1>Resource not found</h1>")
            (type "text/html;charset=utf8"))
        (with-protect ("Error while processing the web-extension keyscheme: ~a" :condition)
          (sera:and-let* ((url (quri:uri url))
                          (path (quri:uri-path url))
                          (parts (str:split "/" path :limit 2))
                          (extension-id (first parts))
                          (inner-path (second parts))
                          (extension (find extension-id (sera:filter #'extension-p (modes buffer))
                                           :key #'id
                                           :test #'string-equal))
                          (full-path (merge-extension-path extension inner-path)))
            (setf data (alex:read-file-into-byte-vector full-path)
                  type (mimes:mime full-path))))
        (values data type)))
  :cors-enabled-p t)
