(require 'list-lib)
(require "ui.scm")
(require "util.scm")

(define *rows* 16)
(define *cols* 34)

(define *inputs* '(("Q" (-1 -1)) ("W" (-1  0)) ("E" (-1  1))
                   ("A" ( 0 -1)) ("S" ( 0  0)) ("D" ( 0  1))
		   ("Z" ( 1 -1)) ("X" ( 1  0)) ("C" ( 1  1))))

(define *player* #f)
(define *robots* #f)
(define *game-over* #f)

(define (combine pos offset)
  (let ((pos2 (map + pos offset)))
    (if (and (< -1 (car pos2) *rows*) (< -1 (cadr pos2) *cols*))
        pos2
        pos)))

(define (rand-pos)
  (list (rand-int *rows*) (rand-int *cols*)))

(define (move-player input)
  (let ((input-offset (assoc input *inputs*)))
    (cond (input-offset (swap! *player* combine (cadr input-offset)))
	  ((string=? input "T") (set! *player* (rand-pos))))))

(define (scrap? pos)
  (> (count (lambda (r) (equal? r pos)) *robots*) 1))

(define (manhattan-dist pos1 pos2)
  (apply + (map (lambda (x y) (abs (- x y))) pos1 pos2)))

(define (best-move pos)
  (let* ((offsets (map cadr *inputs*))
	 (moves (map (lambda (o) (combine pos o)) offsets)))
    (apply min-key (lambda (m) (manhattan-dist m *player*)) moves)))

(define (move-robot pos)
  (if (scrap? pos) pos (best-move pos)))

(define (move-robots)
  (set! *robots* (map move-robot *robots*)))

(define (index pos)
  (+ (* (+ (car pos) 1) (+ *cols* 3)) (cadr pos) 1))

(define (draw-map)
  (let* ((row (string-append "|" (make-string *cols* #\space) "|\n"))
         (output (string-append (make-string (+ *cols* 2) #\-) "\n"
                                (apply string-append (repeat *rows* row))
                                (make-string (+ *cols* 2) #\-) "\n")))
    (string-set! output (index *player*) #\O)
    (dolist (r *robots*)
            (cond
             ((equal? r *player*) (string-set! output (index r) #\X))
             ((scrap? r) (string-set! output (index r) #\S))
             (else (string-set! output (index r) #\R))))
    (set-output output)))

(define (end-game msg)
  (append-output msg)
  (set! *game-over* #t))

(define (check-end-game)
  (cond ((every scrap? *robots*) (end-game "Player wins!"))
        ((member *player* *robots*) (end-game "Player loses!"))
        (else (append-output "QWE/ASD/ZXC to move, T to teleport"))))

(define (new-game)
  (set! *player* (list (/ *rows* 2) (/ *cols* 2)))
  (set! *robots* (repeatedly 10 rand-pos))
  (set! *game-over* #f)
  (draw-map)
  (check-end-game))

(define (process-input input)
  (unless *game-over*
    (move-player input)
    (move-robots)
    (draw-map)
    (check-end-game)))
