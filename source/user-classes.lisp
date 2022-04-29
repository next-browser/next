;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defclass user-class (standard-class) ())
(export-always 'user-class)

(defmethod closer-mop:validate-superclass ((class user-class)
                                           (superclass standard-class))
  t)

(defmethod closer-mop:validate-superclass ((superclass standard-class)
                                           (class user-class))
  t)

(defvar *groups* (make-hash-table :test 'equal))

(macrolet ((define-group-predicate (number)
             (flet ((match-name (number)
                      (intern (format nil "MATCH-INTO-GROUP-~d" number))))
               `(defun ,(match-name number) (qualifier)
                  (let* ((group-label (gethash ,number *groups*)))
                    (cond
                      ((and group-label (equal group-label qualifier))
                       t)
                      ((null group-label)
                       (setf (gethash ,number *groups*) qualifier))
                      (t nil))))))
           (defcombination (amount &body body)
             (flet ((match-name (number)
                      (intern (format nil "MATCH-INTO-GROUP-~d" number)))
                    (group-name (number)
                      (intern (format nil "GROUP-~d" number))))
               `(define-method-combination hookable ()
                  ((before (:before))
                   (around (:around))
                   (after (:after))
                   (primary ())
                   ,@(loop for i below amount
                           collect `(,(group-name i) ,(match-name i))))
                  ,@body)))
           (make-body-wrapper (amount)
             (flet ((group-name (number)
                      (intern (format nil "GROUP-~d" number))))
               `(make-body before after around primary
                           ,@(loop for i below amount collect (group-name i)))))
           (def (amount)
             "A macro-hack to inject the literal iteration number into `define-group-predicate'."
             `(progn
                ,@(loop for i below amount
                        collect `(define-group-predicate ,i))
                (defcombination ,amount
                    (make-body-wrapper ,amount)))))
  (flet ((make-body (before after around primary &rest other-methods)
           (flet ((call-methods (methods)
                    (mapcar #'(lambda (method)
                                `(call-method ,method))
                            methods)))
             (clrhash *groups*)
             (let ((form `(prog1
                              (progn
                                ,@(call-methods before)
                                ,@(alex:mappend #'call-methods other-methods)
                                ,@(call-methods primary))
                            ,@(call-methods (reverse after)))))
               (if around
                   `(call-method ,(first around)
                                 (,@(rest around)
                                  (make-method ,form)))
                   form)))))
    ;; FIXME: 300 is a magic number of groups that is significantly less than
    ;; 1000 (compiling 1000 groups exhaust all 4GB of RAM on aartaka's laptop),
    ;; while still big enough to fit most configs:
    ;;
    ;; 300 * 5 lines of average define-configuration = 1500 lines of config
    (def 300)))

(export-always 'customize-instance)
(defgeneric customize-instance (object &key &allow-other-keys)
  (:method-combination hookable)
  (:method ((class t) &key) t)
  (:documentation "Specialize this method to customize the default values and
behaviour of some CLASS instance.

This method is run after the instance has been initialized (in particular, after
the `initialize-instance' :after method).

The standard method is meant to be used by users only.

Don't use it in public code, prefer `initialize-instance :after' instead to
initialize slots, and `customize-instance :after' for code that relies on
finalized slot values."))

(defmethod make-instance :around ((class user-class) &rest initargs &key &allow-other-keys)
  (sera:lret ((initialized-object (call-next-method)))
    (apply #'customize-instance initialized-object initargs)))

(defun user-class-p (class-specifier)
  (let ((metaclass (cond
                     ((symbolp  class-specifier)
                      (find-class class-specifier))
                     ((closer-mop:classp class-specifier)
                      class-specifier)
                     (t (class-of class-specifier)))))
    (typep metaclass 'user-class)))

(defclass mixin-class (standard-class) ())
(export-always 'mixin-class)

(defmethod closer-mop:validate-superclass ((class mixin-class)
                                           (superclass standard-class))
  t)
(defmethod closer-mop:validate-superclass ((superclass standard-class)
                                           (class mixin-class))
  t)
(defmethod closer-mop:validate-superclass ((class mixin-class)
                                           (superclass user-class))
  t)
(defmethod closer-mop:validate-superclass ((superclass user-class)
                                           (class mixin-class))
  t)

(defmethod initialize-instance :after ((class mixin-class) &key)
  (when (closer-mop:class-direct-slots class)
    (error "Mixin class cannot have slots."))
  class)
