(require 'list-lib)

(define-syntax dolist
  (syntax-rules ()
    ((dolist (x lst) body ...)
     (for-each (lambda (x)
		 body ...)
	       lst))))

(define (min-key k x . xs)
  (first (fold (lambda (y prev-pair)
                 (let ((kx (second prev-pair))
                       (ky (k y)))
                   (if (< kx ky)
                       prev-pair
                       (list y ky))))
               (list x (k x))
               xs)))

(define *rand* ::java.util.Random (java.util.Random))

(define (rand-int n)
  (*rand*:nextInt n))

(define (repeatedly n f)
  (list-tabulate n (lambda (_) (f))))

(define-syntax swap!
  (syntax-rules ()
    ((swap! x f args ...)
     (set! x (f x args ...)))))
