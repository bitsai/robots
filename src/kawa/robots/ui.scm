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
  (process-input (as android.widget.Button v):text)))

(define (append-output s)
  (*text-view*:append s))

(define (set-output s ::String)
  (*text-view*:setText s))
