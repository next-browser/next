(in-package :nyxt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(nyxt-condition)))

(define-condition nyxt-condition (error)
  ((message :initarg :message :accessor nyxt-condition-message))
  (:report (lambda (c stream)
             (format stream "~a" (slot-value c 'message))))
  (:documentation "An error internal to Nyxt. It should abort the ongoing command, but not the whole process."))
