(require 'list-lib)

(define (min-key k x . xs)
  (car (fold (lambda (y prev-pair)
               (let ((kx (cadr prev-pair))
                     (ky (k y)))
                 (if (< kx ky)
                     prev-pair
                     (list y ky))))
             (list x (k x))
             xs)))

(define *rand* (java.util.Random))

(define (rand-int n)
  (*rand*:nextInt n))

(define (read-string s)
  (read (open-input-string s)))

(define (repeatedly n f)
  (list-tabulate n (lambda (_) (f))))

(define (str . xs)
  (let ((o (open-output-string)))
    (for-each (lambda (x) (display x o)) xs)
    (get-output-string o)))

(define-syntax swap!
  (syntax-rules ()
    ((swap! x f args ...)
     (set! x (f x args ...)))))
