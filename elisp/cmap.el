;; A clojure like data accessor

(defmacro cmap (&rest elements)
  `(progn
     ,@(let ((keep nil))
         (mapcar (lambda (e)
                   (setq keep (not keep))
                   (if keep
                       `(defun ,e (clist) (cl-getf clist ,e))
                     `(defun lol () nil)))
                 elements))
     (list ,@elements)))

(defun blub ()
  (let ((tree-like (cmap :foo
                         (cmap :bar 3
                               :baz 4))))
    (prn (-> tree-like :foo :baz))))

(defun blub-noisy ()
  (let ((tree-like (list :foo
                         (list :bar 3
                               :baz 4))))
    (cl-flet ((get-foo (lambda (x) (cl-getf x :foo)))
           (get-baz (lambda (x) (cl-getf x :baz))))
      (-> tree-like get-foo get-baz))))

(blub-noisy)

(defun prn (&rest xs)
  (cl-reduce (lambda (x y) (format "%s%s" x y)) xs))

(defun person-info ()
  (let ((person (cmap :age 36
                      :name "Matthew Carter"
                      :job "being awesome")))
    (prn "Hello " (:name person)
         " you are " (:age person) " years old.")))

(defun person-info-noisy ()
  (let ((person (list :age 36
                      :name "Matthew Carter"
                      :job "being awesome")))
    (prn "Hello " (getf person :name)
         " you are " (getf person :age) " years old.")))

(blub)
(person-info)
