;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-search-buffer-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/search-buffer-mode:search-buffer-mode buffer))
      (assert-true (disable-modes* 'nyxt/search-buffer-mode:search-buffer-mode buffer)))))

(defmacro with-dom (&body body)
  (let ((spinneret:*html-style* :tree))
    `(nyxt/dom:named-html-parse (spinneret:with-html-string (:body ,@body)))))

(defun search-dom (pattern node)
  (nyxt/search-buffer-mode:search-document pattern
                                           :buffer (make-instance 'document-buffer)
                                           :node node
                                           :hint-p nil))

(defvar *query* "match"
  "Default query pattern for search tests.")

(define-test match-in-excluded-nodes ()
  (assert= 1
           (length (search-dom *query* (with-dom (:a *query*) (:comment *query*)
                                         (:style *query*)
                                         (:script *query*) (:noscript *query*))))))

(define-test match-in-simple-node ()
  ;; A simple node is a node that has a single text node child.
  (let* ((text-node "foo match bar match baz")
         (matches (search-dom *query* (with-dom (:a text-node)))))
    (assert= 2 (length matches))
    (loop for match in matches
          do (assert-string= text-node
                             (nyxt/search-buffer-mode::body match))
          do (assert-string= text-node
                             (plump:text (first (nyxt/search-buffer-mode::nodes match)))))))

(define-test match-in-node ()
  (let ((matches (search-dom *query*
                             (with-dom (:a (:a "foo") "match" (:a "bar") (:a "match"))))))
    (assert= 2 (length matches))
    (loop for match in matches
          do (assert-string= *query*
                             (nyxt/search-buffer-mode::body match))
          do (assert-string= *query*
                             (plump:text (first (nyxt/search-buffer-mode::nodes match)))))))

(define-test match-spanning-sibling-nodes ()
  ;; Match
  (let ((matches (search-dom *query*
                          (with-dom (:a "m") (:a "a") (:a "t") (:a "c") (:a "h")
                            (:a "foo") (:a "mat") (:b "ch")))))
    (assert= 2 (length matches))
    (mapcar (lambda (match) (assert-string= *query* (nyxt/search-buffer-mode::body match)))
            matches)
    (assert= 5 (length (nyxt/search-buffer-mode::nodes (first matches))))
    (assert= 2 (length (nyxt/search-buffer-mode::nodes (second matches)))))
  ;; Non-match
  (let ((matches (search-dom *query* (with-dom (:a "m") (:a "foo") (:a "atch")))))
    (assert= 0 (length matches))))

(define-test match-spanning-nested-nodes ()
  ;; Match
  (let ((matches (search-dom *query*
                          (with-dom (:p "ma" (:i "tc" (:b "h")))
                            (:p "m" (:b "atch"))
                            (:p "m" (:i "") (:b "atch"))))))
    (assert= 3 (length matches))
    (mapcar (lambda (match) (assert-string= *query* (nyxt/search-buffer-mode::body match)))
            matches)
    (assert= 3 (length (nyxt/search-buffer-mode::nodes (first matches))))
    (assert= 2 (length (nyxt/search-buffer-mode::nodes (second matches))))
    (assert= 2 (length (nyxt/search-buffer-mode::nodes (third matches)))))
  ;; Non-match
  (let ((matches (search-dom *query* (with-dom (:a "m" (:b "foo") (:b "atch"))))))
    (assert= 0 (length matches))))

(define-test search-all ()
  (let ((search-all (curry 'nyxt/search-buffer-mode::search-all
                           *query*)))
    (assert-false (nyxt/search-buffer-mode::search-all "" *query*))
    (assert-false (funcall search-all "foo"))
    (assert-equal '((0 5) (10 15))
                  (funcall search-all (str:concat *query* " foo " *query*)))))

(defun assert-str-empty (str)
  "Assert whether STR is nil or the empty string."
  (assert-true (str:empty? str)))

(define-test search-contiguous ()
  (let ((search-contiguous (curry 'nyxt/search-buffer-mode::search-contiguous
                                  *query*)))
    (assert-str-empty (nyxt/search-buffer-mode::search-contiguous "" "foo"))
    (assert-str-empty (nyxt/search-buffer-mode::search-contiguous "foo" ""))
    (assert-error 'error (funcall search-contiguous "mat" :found-pattern "h"))
    (assert-equal (values "m" '(4 5))
                  (funcall search-contiguous "foo m"))
    (assert-equal (values "matc" '(4 8))
                  (funcall search-contiguous "foo matc"))
    (assert-str-empty (funcall search-contiguous "foo match"))
    (assert-equal (values "match" '(4 9))
                  (funcall search-contiguous "foo match" :full-match-p t))
    (assert-equal (values "ma" '(0 1))
                  (funcall search-contiguous "a" :found-pattern "m"))
    (assert-str-empty (funcall search-contiguous "foo a" :found-pattern "m"))
    (assert-str-empty (funcall search-contiguous "a foo" :found-pattern "m"))
    (assert-equal (values "mat" '(0 1))
                  (funcall search-contiguous "t" :found-pattern "ma"))
    (assert-str-empty (funcall search-contiguous "foo t" :found-pattern "ma"))
    (assert-str-empty (funcall search-contiguous "t foo" :found-pattern "ma"))
    (assert-equal (values "matc" '(0 1))
                  (funcall search-contiguous "c" :found-pattern "mat"))
    (assert-str-empty (funcall search-contiguous "foo c" :found-pattern "mat"))
    (assert-str-empty (funcall search-contiguous "c foo" :found-pattern "mat"))
    (assert-equal (values *query* '(0 1))
                  (funcall search-contiguous "h foo" :found-pattern "matc"))
    (assert-str-empty (funcall search-contiguous "foo h" :found-pattern "matc"))))
