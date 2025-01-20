;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/document
  (:shadow #:focus-first-input-field)
  (:documentation "Package for `document-mode', mode to interact with structured documents.

The APIs that it includes/uses internally are:
- Copy-pasting: `ffi-buffer-copy', `ffi-buffer-paste', `ffi-buffer-cut',
  `ffi-buffer-select-all', `ffi-buffer-undo', `ffi-buffer-redo'.
- Heading management: `heading' class, `get-headings', `current-heading',
  `scroll-page-to-heading', and `scroll-page-to-n-headings'.
- `get-url-source' function.

Libraries used are `plump', `clss', `nyxt/dom', `cl-base64', `cl-qrencode',
`analysis'."))
(in-package :nyxt/mode/document)

;; TODO: Remove document-mode from special buffers (e.g. help).
;; This is required because special buffers cannot be part of a history (and it breaks it).
;; Bind C-l to set-url-new-buffer?  Wait: What if we click on a link?  url
;; changes in special buffers should open a new one.
;; Or else we require that all special-buffer-generating commands open a new buffer.

(define-mode document-mode ()
  "Mode to interact with structured documents.
This is typically for HTML pages, but other formats could be supported too.
It does not assume being online.

Important pieces of functionality are:
- Page scrolling and zooming.
- QR code generation.
- view-source: for URLs.
- Buffer content summarization.
- Heading navigation.
- Frame selection."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (keyscheme-map
    (define-keyscheme-map "document-mode" ()
      keyscheme:default
      (list
       "C-M-Z" 'nyxt/mode/passthrough:passthrough-mode
       "M-i" 'focus-first-input-field
       "C-M-c" 'open-inspector
       "C-S-c" 'open-inspector
       "M-{" 'previous-heading
       "M-}" 'next-heading
       "C-p" 'print-buffer
       "C-+" 'zoom-page
       "C-=" 'zoom-page              ; Because + shifted = on QWERTY.
       "C-button4" 'zoom-page
       "C-hyphen" 'unzoom-page
       "C-button5" 'unzoom-page
       "C-0" 'reset-page-zoom)
      keyscheme:cua
      (list
       "C-h" 'jump-to-heading
       "C-M-h" 'jump-to-heading-buffers
       "C-c" 'copy
       "C-v" 'paste
       "M-v" 'paste-from-clipboard-ring
       "C-x" 'cut
       "C-a" 'select-all
       "C-z" 'undo
       "C-Z" 'redo
       "C-down" 'scroll-to-bottom
       "C-up" 'scroll-to-top
       ;; Leave SPACE, END, HOME and arrow keys unbound for the renderer
       "keypadleft" 'scroll-left
       "keypadright" 'scroll-right
       "keypadup" 'scroll-up
       "keypaddown" 'scroll-down
       "keypadhome" 'scroll-to-top
       "keypadend" 'scroll-to-bottom
       "keypadpageup" 'scroll-page-up
       "keypadprior" 'scroll-page-up
       "keypadnext" 'scroll-page-down
       "C-u C-o" 'edit-with-external-editor)
      keyscheme:emacs
      (list
       "C-." 'jump-to-heading
       "C-M-." 'jump-to-heading-buffers
       "C-g" 'nothing              ; Emacs users may hit C-g out of habit.
       "M-w" 'copy
       "C-y" 'paste
       "M-y" 'paste-from-clipboard-ring
       "C-w" 'cut
       "C-x h" 'select-all
       "C-/" 'undo
       "C-?" 'redo ; / shifted on QWERTY
       "C-x C-+" 'zoom-page
       "C-x C-=" 'zoom-page ; Because + shifted = on QWERTY.
       "C-x C-hyphen" 'unzoom-page
       "C-x C-0" 'reset-page-zoom
       "C-p" 'scroll-up
       "C-n" 'scroll-down
       "M-<" 'scroll-to-top
       "M->" 'scroll-to-bottom
       "M-v" 'scroll-page-up
       "C-v" 'scroll-page-down
       "C-u C-x C-f" 'edit-with-external-editor)
      keyscheme:vi-normal
      (list
       "g h" 'jump-to-heading
       "g H" 'jump-to-heading-buffers
       "{" 'previous-heading
       "}" 'next-heading
       "y y" 'copy
       "p" 'paste
       ;; Debatable: means "insert after cursor" in Vi(m).
       "P" 'paste-from-clipboard-ring
       "d d" 'cut
       "u" 'undo
       "C-r" 'redo
       "+" 'zoom-page
       "z i" 'zoom-page
       "hyphen" 'unzoom-page
       "z o" 'unzoom-page
       "0" 'reset-page-zoom
       "z z" 'reset-page-zoom
       "h" 'scroll-left
       "l" 'scroll-right
       "k" 'scroll-up
       "j" 'scroll-down
       "g g" 'scroll-to-top
       "G" 'scroll-to-bottom
       "C-b" 'scroll-page-up
       "shift-space" 'scroll-page-up
       "pageup" 'scroll-page-up
       "C-f" 'scroll-page-down
       "space" 'scroll-page-down
       "pagedown" 'scroll-page-down)))))

(define-configuration document-buffer
  ((default-modes (cons 'document-mode %slot-value%))))

(export-always 'active-element-tag)
(defun active-element-tag (&optional (buffer (current-buffer)))
  "The name of the active element in BUFFER."
  (ps-eval :buffer buffer (ps:@ (nyxt/ps:active-element document) tag-name)))

(export-always 'input-tag-p)
(-> input-tag-p ((or string null)) boolean)
(defun input-tag-p (tag)
  "Whether TAG is inputtable."
  (or (string= tag "INPUT")
      (string= tag "TEXTAREA")))

(define-command paste (&optional (buffer (current-buffer)))
  "Paste from clipboard into active element."
  (ffi-buffer-paste buffer))

(define-class ring-source (prompter:source)
  ((prompter:name "Clipboard ring")
   (ring :initarg :ring :accessor ring :initform nil)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:constructor
    (lambda (source)
      (containers:container->list (ring source))))
   (prompter:actions-on-return (lambda-command paste* (ring-items)
                                 (ffi-buffer-paste (current-buffer) (first ring-items)))))
  (:export-class-name-p t)
  (:metaclass user-class)
  (:documentation "Source for previous clipboard contents.
Only includes the strings that were pasted/copied inside Nyxt."))

(define-command paste-from-clipboard-ring ()
  "Show `*browser*' clipboard ring and paste selected entry."
  (ring-insert-clipboard (clipboard-ring *browser*))
  (prompt :prompt "Paste from ring"
          :sources (make-instance 'ring-source :ring (clipboard-ring *browser*))))

(define-command copy (&optional (buffer (current-buffer)))
  "Copy selected text to clipboard."
  (ffi-buffer-copy buffer))

(define-command cut (&optional (buffer (current-buffer)))
  "Cut the selected text in BUFFER."
  (ffi-buffer-cut buffer))

(define-command undo (&optional (buffer (current-buffer)))
  "Undo the last editing action."
  (ffi-buffer-undo buffer))

(define-command redo (&optional (buffer (current-buffer)))
  "Redo the last editing action."
  (ffi-buffer-redo buffer))

(define-command select-all (&optional (buffer (current-buffer)))
  "Select all the text in the text field."
  (ffi-buffer-select-all buffer))

(define-command focus-first-input-field (&key (buffer (current-buffer)))
  "Move the focus to the first inputtable element of BUFFER."
  ;; There are two basic ways to have an editable widget on a webpage:
  ;; - Using <input>/<textarea>,
  ;; - or marking any other element as contenteditable:
  ;;   https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/contenteditable
  (loop with inputs = (clss:ordered-select "input, textarea, [contenteditable]" (document-model buffer))
        for input across inputs
        when (ps-eval :buffer buffer
               (nyxt/ps:element-editable-p
                (nyxt/ps:qs-nyxt-id document (ps:lisp (nyxt/dom:get-nyxt-id input)))))
          do (nyxt/dom:focus-select-element input)
          and do (return input)))

(defmethod nyxt:on-signal-load-finished ((mode document-mode) url)
  (reset-page-zoom :buffer (buffer mode))
  url)

(define-internal-page show-url-qrcode (&key url)
    (:title "*Buffer URL QR code*")
  "Display the QR code containing URL.
Warning: URL is a string."
  (let* ((url (quri:render-uri (url url)))
         (stream (flex:make-in-memory-output-stream)))
    (cl-qrencode:encode-png-stream url stream)
    (spinneret:with-html-string
      (:p (:u url))
      (:p (:img :src (str:concat "data:image/png;base64,"
                                 (cl-base64:usb8-array-to-base64-string
                                  (flex:get-output-stream-sequence stream)))
                :alt url)))))

(define-command-global show-url-qrcode (&key (buffer (current-buffer)))
  "Display the QR code containing the URL of BUFFER in a new page."
  (buffer-load-internal-page-focus 'show-url-qrcode :url (quri:render-uri (url buffer))))

(export-always 'get-url-source)
(defun get-url-source (url)
  "Get HTML source for URL page, as a string.
If the page is open in one of the buffers, just get the source from there.
Otherwise, create a dummy buffer with URL to get its source."
  (let ((buffer (or (find (url url) (buffer-list) :test #'quri:uri= :key #'url)
                    (make-background-buffer :url (url url)))))
    (unwind-protect
         (let ((dom (if (web-buffer-p buffer)
                        (nyxt/dom:copy (document-model buffer))
                        (plump:parse (ffi-buffer-get-document buffer)))))
           (loop for e across (clss:select "[nyxt-identifier]" dom)
                 do (plump:remove-attribute e "nyxt-identifier"))
           (map nil #'plump:remove-child (reverse (clss:select ".nyxt-hint" dom)))
           (plump:serialize dom nil))
      (when (background-buffer-p buffer)
        (ffi-buffer-delete buffer)))))

(define-internal-scheme "view-source"
    (lambda (url)
      (values (get-url-source (quri:url-decode (quri:uri-path (quri:uri url))))
              "text/plain")))

(define-command-global view-source (&key (url (url (current-buffer))))
  "View source of the URL (by default current page) in a separate buffer."
  (make-buffer-focus :url (quri:make-uri :scheme "view-source"
                                         :path (quri:url-encode (quri:render-uri url)))))

(define-command scroll-to-top ()
  "Scroll to the top of the current page."
  (ps-eval (ps:chain window (scroll-by 0 (- (ps:chain document document-element scroll-height))))))

(define-command scroll-to-bottom ()
  "Scroll to the bottom of the current page."
  (ps-eval (ps:chain window (scroll-by 0 (ps:chain document document-element scroll-height)))))

(define-command scroll-down (&key (scroll-distance (scroll-distance (current-buffer))))
  "Scroll down the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  ;; FIXME: This naive implementation doesn't scroll when there are several
  ;; scrollable areas on the screen.
  (ps-eval (ps:chain window (scroll-by 0 (ps:lisp scroll-distance)))))

(define-command scroll-up (&key (scroll-distance (scroll-distance (current-buffer))))
  "Scroll up the current page.
The amount scrolled is determined by the buffer's `scroll-distance'."
  (ps-eval (ps:chain window (scroll-by 0 (ps:lisp (- scroll-distance))))))

(define-command scroll-left (&key (horizontal-scroll-distance
                                   (horizontal-scroll-distance (current-buffer))))
  "Scroll left the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (ps-eval (ps:chain window (scroll-by (ps:lisp (- horizontal-scroll-distance)) 0))))

(define-command scroll-right (&key (horizontal-scroll-distance
                                    (horizontal-scroll-distance (current-buffer))))
  "Scroll right the current page.
The amount scrolled is determined by the buffer's `horizontal-scroll-distance'."
  (ps-eval (ps:chain window (scroll-by (ps:lisp horizontal-scroll-distance) 0))))

(define-command scroll-page-down ()
  "Scroll down by one page height."
  (ps-eval (ps:chain window (scroll-by 0 (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                          (ps:@ window inner-height))))))

(define-command scroll-page-up ()
  "Scroll up by one page height."
  (ps-eval (ps:chain window (scroll-by 0 (- (* (ps:lisp (page-scroll-ratio (current-buffer)))
                                             (ps:@ window inner-height)))))))

(define-command zoom-page (&key (buffer (current-buffer)))
  "Zoom in the current page BUFFER."
  (incf (current-zoom-ratio buffer) (zoom-ratio-step buffer)))

(define-command unzoom-page (&key (buffer (current-buffer)))
  "Zoom out the current page in BUFFER."
  (decf (current-zoom-ratio buffer) (zoom-ratio-step buffer)))

(define-command reset-page-zoom (&key (buffer (current-buffer))
                                      (ratio (zoom-ratio-default buffer)))
  "Reset the BUFFER zoom to the `zoom-ratio-default' or RATIO."
  (setf (current-zoom-ratio buffer) ratio))

(define-internal-page summarize-buffer (&key (summary-length 5) (id (id (current-buffer))))
    (:title "*Summary*")
  "Summarize the current buffer by creating a new summary buffer.
ID is a buffer `id'."
  (let ((buffer (nyxt::buffers-get id)))
    (let ((contents
            (serapeum:string-join
             (map 'list (lambda (e) (plump:text e))
                  (clss:select "p" (document-model buffer)))
             " ")))
      (spinneret:with-html-string
        (:h1 "Summary for: " (title buffer))
        (:ul
         (loop for point in (analysis:summarize-text contents :summary-length summary-length)
               collect (:li point)))))))

(define-command-global summarize-buffer (&key (summary-length 5) (buffer (current-buffer)))
  "Summarize the current buffer by creating a new summary buffer.
SUMMARY-LENGTH allows to list more/less summary items."
  (buffer-load-internal-page-focus 'summarize-buffer
                                   :summary-length summary-length
                                   :id (id buffer)))

(define-class heading ()
  ((inner-text "" :documentation "The inner text of the heading within the document.")
   (element nil :documentation "The header-representing element of `document-model'.")
   (buffer :documentation "The buffer to which this heading belongs.")
   (keywords :documentation "Keywords associated with this heading.")
   (scroll-position :documentation "The scroll position of the heading."))
  (:documentation "A heading representation with all the attached metadata.
The inner-text must not be modified, so that we can jump to the anchor of the same name."))

(defmethod title ((heading heading))
  (subseq (inner-text heading) 0 (position #\[ (inner-text heading))))

(defun get-headings (&key (buffer (current-buffer)))
  (ps-labels :buffer buffer
    ((heading-scroll-position
      :buffer buffer (element)
      (ps:chain (nyxt/ps:rqs-nyxt-id document (ps:lisp (nyxt/dom:get-nyxt-id element)))
                (get-bounding-client-rect) y)))
    (map 'list
         (lambda (e)
           (make-instance 'heading :inner-text (plump:text e)
                                   :element e
                                   :buffer buffer
                                   :keywords (ignore-errors
                                              (analysis:extract-keywords
                                               (plump:text (plump:next-element e))))
                                   :scroll-position (heading-scroll-position e)))
         (clss:ordered-select "h1, h2, h3, h4, h5, h6" (document-model buffer)))))

(defun current-heading (&optional (buffer (current-buffer)))
  (when-let* ((scroll-position (document-scroll-position buffer))
              (vertical-scroll-position (second scroll-position))
              (headings (get-headings :buffer buffer)))
    (first (sort headings
                 (lambda (h1 h2)
                   (< (abs (- (scroll-position h1) vertical-scroll-position))
                      (abs (- (scroll-position h2) vertical-scroll-position))))))))

;; TODO: Make a method on plump:node? Extract to nyxt/dom?
(defun scroll-page-to-heading (heading)
  (set-current-buffer (buffer heading) :focus nil)
  (nyxt/dom:scroll-to-element (element heading)))

(defun scroll-page-to-n-headings (n &optional (buffer (current-buffer)))
  "Scroll to the N adjacent heading of the BUFFER."
  (and-let* ((headings (get-headings :buffer buffer))
             (new-position (+ n
                              (position (nyxt/dom:body (element (current-heading buffer)))
                                        headings
                                        :key (compose #'nyxt/dom:body #'element)
                                        :test #'equal)))
             (_ (<= 0 new-position (1- (length headings)))))
    (scroll-page-to-heading (elt headings new-position))))

(define-command next-heading (&optional (buffer (current-buffer)))
  "Scroll to the next heading of the BUFFER."
  (scroll-page-to-n-headings 1 buffer))

(define-command previous-heading (&optional (buffer (current-buffer)))
  "Scroll to the previous heading of the BUFFER."
  (scroll-page-to-n-headings -1 buffer))

(define-class heading-source (prompter:source)
  ((prompter:name "Headings")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:filter-preprocessor #'prompter:filter-exact-matches)
   (prompter:actions-on-current-suggestion-enabled-p t)
   (prompter:actions-on-current-suggestion
    (lambda-command scroll-page-to-heading* (heading)
      "Scroll to heading."
      (scroll-page-to-heading heading)))
   (prompter:constructor (lambda (source)
                           (get-headings :buffer (buffer source))))
   (prompter:actions-on-return (lambda-unmapped-command scroll-page-to-heading))))

(defmethod prompter:object-attributes ((heading heading) (source heading-source))
  (declare (ignore source))
  `(("Title" ,(format nil "~a ~a"
                      (make-string (typecase (element heading)
                                     (nyxt/dom:h1-element 1)
                                     (nyxt/dom:h2-element 2)
                                     (nyxt/dom:h3-element 3)
                                     (nyxt/dom:h4-element 4)
                                     (nyxt/dom:h5-element 5)
                                     (nyxt/dom:h6-element 6)
                                     (t 0))
                                   :initial-element #\*)
                      (title heading)))
    ("Keywords" ,(format nil "~:{~a~^ ~}" (keywords heading)))))

(define-command jump-to-heading (&key (buffer (current-buffer)))
  "Jump to a particular heading, of type h1, h2, h3, h4, h5, or h6."
  (prompt :prompt "Jump to heading"
          :sources (make-instance 'heading-source :buffer buffer)))

(define-command jump-to-heading-buffers ()
  "Jump to a heading (H1 to H6) among a set of buffers."
  (let ((buffers (prompt
                  :prompt "Select headings from buffers"
                  :sources (make-instance 'buffer-source
                                          :enable-marks-p t
                                          :actions-on-return #'identity))))
    (prompt
     :prompt "Jump to heading"
     :sources (loop for buffer in buffers
                    collect (make-instance
                             'heading-source
                             :name (format nil "Headings: ~a" (title buffer))
                             :buffer buffer)))))

(export-always 'print-buffer)
(define-command print-buffer ()
  "Print the current buffer."
  (ps-eval (print)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame selection engine:

(define-class html-element ()
  ((body "")))

(define-class link (html-element)
  ((url "")))

(define-class image (html-element)
  ((alt "" :documentation "Alternative text for the image.")
   (url "")))

(defun frame-element-select ()
  "Allow the user to draw a frame around elements to select them."
  (let ((overlay-style (theme:themed-css (theme *browser*)
                         `("#nyxt-overlay"
                           :position "fixed"
                           :top "0"
                           :left "0"
                           :right "0"
                           :bottom "0"
                           :opacity 0.00
                           :background ,theme:on-background-color
                           :z-index #.(1- (expt 2 31)))))
        (selection-rectangle-style (theme:themed-css (theme *browser*)
                                     `("#nyxt-rectangle-selection"
                                       :position "absolute"
                                       :top "0"
                                       :left "0"
                                       :border-style "dotted"
                                       :border-width "4px"
                                       :border-color ,theme:background-color
                                       :background-color ,theme:on-background-color
                                       :opacity 0.10
                                       :z-index ,(1- (expt 2 30))))))
    (ps-labels :async t
      ((add-overlay
        (overlay-style selection-rectangle-style)
        "Add a selectable overlay to the screen."
        (defparameter selection
          (ps:create x1 0 y1 0
                     x2 0 y2 0
                     set1 false
                     set2 false))
        (defun add-stylesheet ()
          (unless (nyxt/ps:qs document "#nyxt-stylesheet")
            (ps:let ((style-element (ps:chain document (create-element "style"))))
              (setf (ps:@ style-element id) "nyxt-stylesheet")
              (ps:chain document head (append-child style-element)))))
        (defun add-style (style)
          (ps:let ((style-element (nyxt/ps:qs document "#nyxt-stylesheet")))
            (ps:chain style-element sheet (insert-rule style 0))))
        (defun add-overlay ()
          (ps:let ((element (ps:chain document (create-element "div"))))
            (add-style (ps:lisp overlay-style))
            (setf (ps:@ element id) "nyxt-overlay")
            (ps:chain document body (append-child element))))
        (defun add-selection-rectangle ()
          (ps:let ((element (ps:chain document (create-element "div"))))
            (add-style (ps:lisp selection-rectangle-style))
            (setf (ps:@ element id) "nyxt-rectangle-selection")
            (ps:chain document body (append-child element))))
        (defun update-selection-rectangle ()
          (ps:let ((element (nyxt/ps:qs document "#nyxt-rectangle-selection")))
            (setf (ps:@ element style left) (+ (ps:chain selection x1) "px"))
            (setf (ps:@ element style top) (+ (ps:chain selection y1) "px"))
            (setf (ps:@ element style width)
                  (+ (- (ps:chain selection x2)
                        (ps:chain selection x1))
                     "px"))
            (setf (ps:@ element style height)
                  (+ (- (ps:chain selection y2)
                        (ps:chain selection y1))
                     "px"))))
        (defun add-listeners ()
          (setf (ps:@ (nyxt/ps:qs document "#nyxt-overlay") onmousemove)
                (lambda (e)
                  (when (and (ps:chain selection set1)
                             (not (ps:chain selection set2)))
                    (setf (ps:chain selection x2) (ps:chain e |pageX|))
                    (setf (ps:chain selection y2) (ps:chain e |pageY|))
                    (update-selection-rectangle))))
          (setf (ps:@ (nyxt/ps:qs document "#nyxt-overlay") onclick)
                (lambda (e)
                  (if (not (ps:chain selection set1))
                      (progn
                        (setf (ps:chain selection x1) (ps:chain e |pageX|))
                        (setf (ps:chain selection y1) (ps:chain e |pageY|))
                        (setf (ps:chain selection set1) true))
                      (progn
                        (setf (ps:chain selection x2) (ps:chain e |pageX|))
                        (setf (ps:chain selection y2) (ps:chain e |pageY|))
                        (setf (ps:chain selection set2) true))))))
        (add-stylesheet)
        (add-overlay)
        (add-selection-rectangle)
        (add-listeners)))
      (add-overlay overlay-style selection-rectangle-style))))

(defun frame-element-get-selection ()
  "Get the selected elements drawn by the user."
  (ps-labels
    ((get-selection
      ()
      (defun element-in-selection-p (selection element)
        "Determine if a element is bounded within a selection."
        (ps:let* ((element-rect (ps:chain element (get-bounding-client-rect)))
                  (offsetX (ps:chain window |pageXOffset|))
                  (offsetY (ps:chain window |pageYOffset|))
                  (element-left (+ (ps:chain element-rect left) offsetX))
                  (element-right (+ (ps:chain element-rect right) offsetX))
                  (element-top (+ (ps:chain element-rect top) offsetY))
                  (element-bottom (+ (ps:chain element-rect bottom) offsetY)))
          (if (and
               (<= element-left (ps:chain selection x2))
               (>= element-right (ps:chain selection x1))
               (<= element-top (ps:chain selection y2))
               (>= element-bottom (ps:chain selection y1)))
              t nil)))
      (defun object-create (element)
        (cond ((equal "A" (ps:@ element tag-name))
               (ps:create "type" "link" "href" (ps:@ element href) "body" (ps:@ element |innerHTML|)))
              ((equal "IMG" (ps:@ element tag-name))
               (ps:create "type" "img" "src" (ps:@ element src) "alt" (ps:@ element alt)))))
      (defun collect-selection (elements selection)
        "Collect elements within a selection"
        (loop for element in elements
              when (element-in-selection-p selection element)
                collect (object-create element)))
      (collect-selection (nyxt/ps:qsa document (list "a")) selection)))
    (loop for element in (get-selection)
          collect (str:string-case (gethash "type" element )
                    ("link"
                     (make-instance 'link
                                    :url (gethash "href" element )
                                    :body (plump:text (plump:parse (gethash "body" element )))))
                    ("img"
                     (make-instance 'image
                                    :url (gethash "src" element )
                                    :alt (gethash "alt" element )))))))

(define-parenscript frame-element-selection-ready ()
  "Check to see if the selection is complete."
  (and (ps:chain selection set1)
       (ps:chain selection set2)))

(defun frame-element-clear ()
  "Clear the selection frame created by the user."
  (ps-eval
    (ps:chain (nyxt/ps:qs document "#nyxt-rectangle-selection") (remove))
    (ps:chain (nyxt/ps:qs document "#nyxt-overlay") (remove))))

(define-class frame-source (prompter:source)
  ((prompter:name "Selection Frame")
   (buffer :accessor buffer :initarg :buffer)
   (prompter:constructor (lambda (source)
                           (with-current-buffer (buffer source)
                             (frame-element-select)
                             (prog1
                                 (loop
                                   do (sleep 0.25)
                                   when (frame-element-selection-ready)
                                   return (frame-source-selection))
                               (toggle-prompt-buffer-focus)))))))

(define-command select-frame-new-buffer (&key (buffer (current-buffer)))
  "Select a frame and open the links in new buffers."
  (prompt :prompt "Open selected links in new buffers"
          :sources (make-instance
                    'frame-source
                    :buffer buffer
                    :enable-marks-p t
                    :actions-on-return (lambda-command open-new-buffers (urls)
                                         (mapcar (lambda (i) (make-buffer :url (quri:uri i)))
                                                 urls)))
          :after-destructor (lambda () (with-current-buffer buffer
                                    (frame-element-clear)))))

(defun frame-source-selection ()
  (remove-duplicates (mapcar #'url (frame-element-get-selection))
                     :test #'equal))

(pushnew 'document-mode nyxt::%default-modes)
