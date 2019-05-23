(asdf:defsystem #:cmap
  :serial t
  :description "Clojure Like Maps to reduce Common Lisp verbosity"
  :author "Matthew Carter <m@ahungry.com>"
  :license "GPLv3"
  :depends-on (#:cl-ppcre
               #:parenscript
               #:named-readtables)
  :components ((:file "cmap")))
