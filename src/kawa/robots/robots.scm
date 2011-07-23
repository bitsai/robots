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

(define (map-index pos)
  (+ 1 (* (+ 1 (first pos)) (+ *cols* 3)) (second pos)))

(define (draw-map)
  (let* ((border (string-append (make-string (+ *cols* 2) #\-) "\n"))
         (row (string-append "|" (make-string *cols* #\space) "|\n"))
         (rows (apply string-append (make-list *rows* row)))
         (output (string-append border rows border)))
    (string-set! output (map-index *player*) #\@)
    (dolist (r *robots*)
            (let* ((idx (map-index r))
                   (c (string-ref output idx)))
              (case c
                ((#\@) (string-set! output idx #\X))
                ((#\A) (string-set! output idx #\#))
                ((#\space) (string-set! output idx #\A)))))
    output))

(define (combine pos offset)
  (let ((pos2 (map + pos offset)))
    (if (and (< -1 (first pos2) *rows*) (< -1 (second pos2) *cols*))
        pos2
        pos)))

(define (rand-pos)
  (list (rand-int *rows*) (rand-int *cols*)))

(define (move-player input)
  (let ((input-offset (assoc input *inputs*)))
    (cond (input-offset (swap! *player* combine (second input-offset)))
	  ((string=? "T" input) (set! *player* (rand-pos))))))

(define (manhattan-dist pos1 pos2)
  (apply + (map (lambda (x y) (abs (- x y))) pos1 pos2)))

(define (scrap? pos)
  (> (count (lambda (r) (equal? pos r)) *robots*) 1))

(define (move-robot pos)
  (if (scrap? pos)
      pos
      (let* ((offsets (map second *inputs*))
             (moves (map (lambda (o) (combine pos o)) offsets)))
        (apply min-key (lambda (m) (manhattan-dist m *player*)) moves))))

(define (move-robots)
  (set! *robots* (map move-robot *robots*)))

(define (end-game msg)
  (append-output msg)
  (set! *game-over* #t))

(define (check-end-game)
  (cond ((every scrap? *robots*) (end-game "Player wins!"))
        ((member *player* *robots*) (end-game "Player loses!"))
        (else (append-output "QWE/ASD/ZXC to move, (T)eleport"))))

(define (new-game)
  (set! *player* (list (/ *rows* 2) (/ *cols* 2)))
  (set! *robots* (repeatedly 10 rand-pos))
  (set! *game-over* #f)
  (set-output (draw-map))
  (check-end-game))

(define (process-input input)
  (unless *game-over*
    (move-player input)
    (move-robots)
    (set-output (draw-map))
    (check-end-game)))
