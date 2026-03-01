#lang racket/gui
(require framework)

(define my-styles (new style-list%))
(send my-styles replace-named-style "framework:line-numbers-current-line-number-background" (send my-styles basic-style))


(define frame
  (new frame% [label "Test Frame"]
              [width 900] [height 700]))

(define (show) (send frame show #t))
(define (hide) (send frame show #f))

(send frame create-status-line)
(send frame set-status-text " Initialized")
(define (set-status text)
  (send frame set-status-text text))

(define panel
  (new (panel:horizontal-dragable-mixin 
        (panel:dragable-mixin panel%)) [parent frame]))

(define ed-canv (new editor-canvas% [parent panel]))

(define selected-color (make-object color% 180 180 0))

(define TestEditor%
  (class (editor:standard-style-list-mixin text%)
    (inherit get-start-position
             get-end-position)
    (display "HELLO WORLD !!\n")
    
    (define/augment (after-set-position)
      (let [[start (get-start-position)]
            [end (get-end-position)]]
        (set-status (format " Position ~a -> ~a" start end))))

    (define/augment (after-delete start len)
      (set-status (format " Delete at ~a | length: ~a" start len)))

    (define/augment (after-insert start len)
      (set-status (format " Insert at ~a | length: ~a" start len)))

    (super-new)))

(define TestEditor2%
  (class (racket:set-mode-mixin
          (racket:text-mixin
           (text:autocomplete-mixin
            (mode:host-text-mixin color:text%))))  
    (display "HELLO WORLD !!\n")

    (define/override (get-all-words) '("apple" "banana" "orange"))

    (define/override (get-autocomplete-border-color) "Dark Orange")
    (define/override (get-autocomplete-background-color) "Gold")
    (define/override (get-autocomplete-selected-color) selected-color)
    
    (define (completion-mode-key-event? key-event)
      (cond
        [(and (eq? (send key-event get-key-code) #\space)
              (send key-event get-control-down))
         (or (eq? (system-type) 'macosx)
             (not (preferences:get 'framework:menu-bindings)))]
        [else
         #f]))
    (override completion-mode-key-event?)
    
    (super-new)
    ))

(define (color->snip color)
  (let* [[bitmap (make-object bitmap% 20 20)]
         [dc (send bitmap make-dc)]]
    (send dc set-background color)
    (send dc clear)
    (make-object image-snip% bitmap)))

(define (list-styles)
  (let* [[style-list (send editor get-style-list)]
         [count (send style-list number)]]
       (let loop [[index 0]]
         (when (< index count)
           (let* [[style  (send style-list index-to-style index)]
                  [name (send style get-name)]]
             (when name
               (display (color->snip (send style get-foreground)))
               (display " ")
               ;(display (color->snip (send style get-background)))
               ;(display " ")
               (display (send style get-size))
               (display "  ")
               (display name)
               (newline))
             (loop (+ index 1)))))))

;; TODO How to prevent filtering of autocompletions?

(define editor (new (text:line-numbers-mixin 
                     TestEditor2%)))
(send ed-canv set-editor editor)
(send editor set-line-numbers-color "light gray")
;(send editor set-style-list my-styles)

(let [[keymap (send editor get-keymap)]]
  (send keymap add-function "doobleclick"
        (λ (editor event) (set-status "DOUBLE CLICK")))
  (send keymap map-function "leftbuttondouble" "doobleclick"))

(define mb (new menu-bar% [parent frame]))
(define m-edit (new menu% [label "Edit"] [parent mb]))
(define m-font (new menu% [label "Font"] [parent mb]))
(append-editor-operation-menu-items m-edit #f)
(append-editor-font-menu-items m-font)

(send ed-canv set-canvas-background (send the-color-database find-color "White"))

(show)
(list-styles)
