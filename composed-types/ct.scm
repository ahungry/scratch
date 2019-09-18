;; Load at REPL via ,use (oop goops
(use-modules (oop goops))

;; Main conversion types...
;; string->number
;; number->string

(define (foo n) n)

(define (fn i o f)
  `((i . ,i)
    (o . ,o)
    (f . ,f)))

;; Ideally we would use a syntax macro to make a form similar to this
;; (defn add-one
;;   "This is the doc for the add-one call."
;;   [number n -> number]
;;   (+ 1 n))

(define add-one (fn 'number 'number (lambda (n) (+ 1 n))))
(define stringer (fn 'number 'string (lambda (n) (number->string n))))

(define mode 'type-check)
(define (type-check) (set! mode 'type-check))
(define (runtime) (set! mode 'runtime))
(define (invoke f x) ((cdr (assoc 'f f)) x))
(define (in-type f) (cdr (assoc 'i f)))
(define (out-type f) (cdr (assoc 'o f)))

;; This equal? comparison could be vastly improved to allow hierarchal types
;; as well as checking multiple types vs singular types.
(define (assert-types f g)
  (if (equal? (in-type f) (out-type g))
      #t
      (error (format #nil "Incompatible types in comp: expected ~a got ~a~%" (in-type f) (out-type g)))))

(define (_comp f g)
  (fn (out-type f) (in-type g)
      (lambda (x) (invoke f (invoke g x)))))

(define (comp f g)
  (if (equal? 'type-check mode)
      (begin (assert-types f g) (_comp f g))
      (_comp f g)))

(define (_range count iter ns)
  (if (>= (length ns) count)
      ns
      (_range count (+ 1 iter) (cons iter ns))))

;; Should we define a collection type? OR always treat a collection element
;; as if it evalled to a single element and flag a col type differently...
(define range (fn 'number 'number (lambda (n) (reverse (_range n 0 #nil)))))

;; Will compose just fine.
(define add-one->string (comp stringer add-one))
(define add-2 (comp add-one add-one))
(invoke add-2 3)                        ; produces 5

;; Even the comp of a comped fn will ensure type safety.
(define add-4 (comp add-2 add-2))
(invoke add-4 10)                       ; produces 14

;;  Will trigger an error.
;; (define add-one-to-string (comp add-one stringer))

;; How about a type safe map facility? Comp a fn f to a data provider g
;; To ultimately generate a 0 arity fn to invoke later.
(define (_compmap f g)
  (fn (out-type g) (in-type g)
      (lambda (x) (map (lambda (n) (invoke f n)) (invoke g x)))))
(define (compmap f g)
  (if (equal? 'type-check mode)
      (begin (assert-types f g) (_compmap f g))
      (_compmap f g)))

(define add-4-to-range (compmap add-4 range))

;; evals to 4 5 6
(invoke add-4-to-range 3)

;; Now, a potential fail case...
(define get-names (fn 'nil 'string (lambda (_) (list "Jon" "Bob"))))

;; Incompatible types in comp: expected number got string
;; (define add-4-to-names (compmap add-4 get-names))
