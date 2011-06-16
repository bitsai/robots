(require 'list-lib)
(require "ui.scm")
(require "util.scm")

(define *rows* 16)
(define *cols* 32)
(define *inputs* '((Q (-1 -1)) (W (-1  0)) (E (-1  1))
                   (A ( 0 -1)) (S ( 0  0)) (D ( 0  1))
		   (Z ( 1 -1)) (X ( 1  0)) (C ( 1  1))))

(define *player* #f)
(define *robots* #f)
(define *game-over* #f)

(define (rand-pos)
  (list (rand-int *rows*) (rand-int *cols*)))

(define (combine pos offset)
  (let ((pos2 (map + pos offset)))
    (if (and (< -1 (car pos2) *rows*) (< -1 (cadr pos2) *cols*))
        pos2
        pos)))

(define (move-player input)
  (let ((input-offset (assoc input *inputs*)))
    (cond (input-offset (swap! *player* combine (cadr input-offset)))
	  ((equal? input 'T) (set! *player* (rand-pos))))))

(define (manhattan-dist pos1 pos2)
  (apply + (map (lambda (x y) (abs (- x y))) pos1 pos2)))

(define (best-move pos)
  (let* ((offsets (map cadr *inputs*))
	 (moves (map (lambda (offset) (combine pos offset)) offsets)))
    (apply min-key (lambda (move) (manhattan-dist move *player*)) moves)))

(define (dead-robot? pos)
  (> (count (lambda (robot) (equal? pos robot)) *robots*) 1))

(define (move-robot pos)
  (if (dead-robot? pos) pos (best-move pos)))

(define (move-robots)
  (set! *robots* (map move-robot *robots*)))

(define (draw-pos pos)
  (cond ((dead-robot? pos) "#")
        ((member pos *robots*) "A")
        ((equal? pos *player*) "@")
        (else " ")))

(define (draw-row row)
  (let ((draw-col (lambda (col) (draw-pos (list row col)))))
    (str "|" (apply str (map draw-col (iota *cols*))) "|\n")))

(define (draw-map)
  (set-output (make-string (+ *cols* 2) #\-) "\n"
              (apply str (map draw-row (iota *rows*)))
              (make-string (+ *cols* 2) #\-) "\n"
              "QWE/ASD/ZXC to move, T to teleport\n"))

(define (new-game)
  (set! *player* (list (/ *rows* 2) (/ *cols* 2)))
  (set! *robots* (repeatedly 6 rand-pos))
  (set! *game-over* #f)
  (draw-map))

(define (end-game msg)
  (append-output msg)
  (set! *game-over* #t))

(define (process-input input)
  (unless *game-over*
    (move-player input)
    (move-robots)
    (cond ((every dead-robot? *robots*) (end-game "Player wins!"))
          ((member *player* *robots*) (end-game "Player loses!"))
          (else (draw-map)))))
