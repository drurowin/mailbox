(defpackage :mailbox
  (:use :cl)
  (:export #:mailboxp
           #:make-mailbox
           #:post-mail
           #:read-mail))

(in-package :mailbox)

(defclass mailbox ()
  ((mailbox :initform (cons nil nil))
   (tail)
   (lock :initform (bordeaux-threads:make-lock))
   (emptyp :initform t)))

(defmethod initialize-instance :after ((the mailbox) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value the 'tail) (slot-value the 'mailbox)))

(defun make-mailbox ()
  (make-instance 'mailbox))

(defgeneric mailboxp (object)
  (:documentation "Return T if OBJECT is a mailbox.")
  (:method (object) nil)
  (:method ((object mailbox)) t))

(defgeneric post-mail (object mailbox)
  (:documentation "Add OBJECT to MAILBOX.")
  (:method (object (mailbox mailbox))
    (with-slots (lock emptyp mailbox tail)
        mailbox
      (bordeaux-threads:with-lock-held (lock)
        (let ((mail (cons object nil)))
          (if emptyp
              (setf mailbox mail
                    tail mailbox
                    emptyp nil)
              (setf (cdr tail) mail
                    tail mail))
          object)))))

(defgeneric read-mail (mailbox)
  (:documentation "Retrieve oldest object from MAILBOX.

If the mailbox is not empty return multiple values the object and T,
otherwise NIL and NIL.")
  (:method ((mailbox mailbox))
    (with-slots (lock emptyp mailbox tail)
        mailbox
      (bordeaux-threads:with-lock-held (lock)
        (if emptyp
            (values nil nil)
            (values (prog1 (first mailbox)
                      (if (endp (rest mailbox))
                          (let ((empty (cons nil nil)))
                            (setf mailbox empty
                                  tail empty
                                  emptyp t))
                          (setf mailbox (rest mailbox))))
                    t))))))
