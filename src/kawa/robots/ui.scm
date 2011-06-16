(require 'android-defs)
(require "engine.scm")
(require "util.scm")

(define *text-view* ::android.widget.TextView #!null)

(activity
 ui
 (on-create
  ((this):setContentView kawa.robots.R$layout:main)
  (set! *text-view* ((this):findViewById kawa.robots.R$id:text_view))
  (new-game))
 ((onClick v ::android.view.View)
  (process-input (read-string (as android.widget.Button v):text))))

(define (append-output . xs)
  (*text-view*:append (apply str xs)))

(define (set-output . xs)
  (*text-view*:setText (as String (apply str xs))))
