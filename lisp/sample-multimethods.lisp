(defclass Wrapped ()
  ((Thing :accessor Thing :initarg :thing :initform nil)))

(defclass WrappedNumber (Wrapped) ())
(defclass WrappedString (Wrapped) ())

(defgeneric add (n1 n2) (:documentation "Add something to something."))
(defmethod add ((n1 Wrapped) n2) (error "Do not know how to these types!"))
(defmethod add ((n1 WrappedNumber) (n2 integer))
  (setf (Thing n1) (+ (Thing n1) n2)))
(defmethod add ((n1 WrappedString) (n2 simple-array))
  (setf (Thing n1) (format nil "~a~a" (Thing n1) n2)))

(defparameter x (make-instance 'WrappedNumber :thing 5))
(add x 3)
(format *standard-output* "~%~a~%" (Thing x)) ;; 8
(add x 3.2)                                   ;; Signal error
(defparameter y (make-instance 'WrappedString :thing "Hello"))
(add y " World")
(format *standard-output* "~%~a~%" (Thing y)) ;; Hello World
(add y 3)                               ; Error
