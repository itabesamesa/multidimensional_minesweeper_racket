#lang racket/gui

(require racket/gui/base)
(require racket/draw)
(require config)

(define is-revealed? #f)
(define is-lost? #f)
(define is-won? #f)

(local-config-file-name)

(define-config-param debug-on? #:default-value 10)
(set! debug-on? (debug-on?))

(define-config-path covered-img #:default-value "./covered.png")
(define-config-path uncovered-img #:default-value "./uncovered.png")
(define-config-path flag-img #:default-value "./flag.png")
(define-config-path bomb-img #:default-value "./bomb.png")
(set!-values (covered-img uncovered-img flag-img bomb-img) (values (covered-img) (uncovered-img) (flag-img) (bomb-img)))

(define-config-param img-width #:default-value 40)
(define-config-param img-height #:default-value 40)

(define-config-param highlight-red #:default-value 255)
(define-config-param highlight-green #:default-value 42)
(define-config-param highlight-blue #:default-value 255)

(define-config-param influenced-saturation #:default-value 0.3)

(define-config-param paused-value #:default-value 0.3)

(define-config-param reveal-on-lose? #:default-value #f)
;(define reveal-on-lose? #t)
(define-config-param hide-on-pause? #:default-value #t)
;(define hide-on-pause? #f)
(define-config-param is-relative? #:default-value #t)
(set! is-relative? (is-relative?))
;(define is-relative? #f)
(define-config-param show-timer? #:default-value #t)
;(define show-timer? #t)

(define-config-param field-dimensions #:default-value '(4 4 4 4))
(set! field-dimensions (field-dimensions))
;(define field-dimensions '(4 4 4 4))
(define-config-param seed #:default-value (current-seconds))
(set! seed (seed))
;(define seed (current-seconds))
(define-config-param amount #:default-value 10)
(set! amount (amount))
;(define amount 10)

(define field-dimensions-new field-dimensions)
(define seed-new seed)
(define amount-new amount)

(define starting-click? #f)
(define start-time 0)
(define pause-times '())
(define time-accumulator 0)

(define largest-value 0)
(define clicked-fields 0)
(define field-size (foldl * 1 field-dimensions))
(define current-pos (make-list (length field-dimensions) 0))

(define lr-dimension 0)
(define du-dimension 1)
(define ad-dimension 2)
(define sw-dimension 3)

(define (pause-game)
  (when starting-click?
    (send timer stop)
    (set! pause-times (append pause-times (list (cons start-time (current-inexact-milliseconds))))))
  (if (hide-on-pause?)
      (apply-to-all field-dimensions (car (send mine-field get-children)) (λ (child) (send child draw-temporary 'paused 'covered)))
      (apply-to-all field-dimensions (car (send mine-field get-children)) (λ (child) (send child draw-temporary 'paused))))
  (send mine-field-container enable #f)
  (send state set-label "Game paused"))

(define (unpause-game)
  (apply-to-all field-dimensions (car (send mine-field get-children)) (λ (child) (send child draw-reset)))
  (send mine-field-container enable #t)
  (send state set-label old-state)
  (when starting-click?
    (set! start-time (current-inexact-milliseconds))
    (send timer start 10)))

(define (toggle-pause)
  (unless (or is-revealed? is-lost? is-won?)
    (if (send mine-field-container is-enabled?)
        (pause-game)
        (unpause-game))))

(define (reveal-game)
  (send timer stop)
  (set! pause-times (append pause-times (list (cons start-time (current-inexact-milliseconds)))))
  (apply-to-all field-dimensions (car (send mine-field get-children)) (λ (child) (send child draw-temporary 'default 'uncovered)))
  (send mine-field-container enable #f)
  (unless is-won?
    (set! is-revealed? #t)))

(define (win-game)
  (send timer stop)
  (set! pause-times (append pause-times (list (cons start-time (current-inexact-milliseconds)))))
  (apply-to-all field-dimensions (car (send mine-field get-children)) (λ (child) (send child draw-temporary 'default)))
  (send mine-field-container enable #f)
  (set! is-won? #t)
  (send state set-label "Game won"))

(define (lose-game)
  (if (reveal-on-lose?)
      (reveal-game)
      (begin
        (send timer stop)
        (set! pause-times (append pause-times (cons start-time (current-inexact-milliseconds))))
        (send mine-field-container enable #f)))
  (set! is-lost? #t)
  (send state set-label "Game lost"))

(define (run-game)
  (set! start-time (current-inexact-milliseconds))
  (send timer start 10)
  (when (show-timer?) (send timer-message show #t))
  (send state set-label "Game running")
  (set! old-state (send state get-label))
  (set! starting-click? #t))

(define (move-up-in-dimension d)
  (if (< d (length field-dimensions))
      (let ([new-pos (add1 (list-ref current-pos d))])
        (if (and (>= new-pos 0) (< new-pos (list-ref field-dimensions d)))
            (begin
              (set! current-pos (list-set current-pos d new-pos))
              #t)
            (if (move-up-in-dimension (+ d 2))
              (begin
                (set! current-pos (list-set current-pos d 0))
                #t)
              #f)))
      #f))

(define (move-down-in-dimension d)
  (if (< d (length field-dimensions))
      (let ([new-pos (sub1 (list-ref current-pos d))])
        (if (and (>= new-pos 0) (< new-pos (list-ref field-dimensions d)))
            (begin
              (set! current-pos (list-set current-pos d new-pos))
              #t)
            (if (move-down-in-dimension (+ d 2))
              (begin
                (set! current-pos (list-set current-pos d (sub1 (list-ref field-dimensions d))))
                #t)
              #f)))
      #f))

(define (apply-at-coord coord [proc (λ (child) #f)] [parent (car (send mine-field get-children))])
  (if (empty? coord)
      (proc parent)
      (apply-at-coord (drop-right coord 1) proc (list-ref (send parent get-children) (last coord)))))

(define (move move-in-dimension d)
  (apply-at-coord current-pos
                  (λ (child) (send child apply-to-neighbours (λ (neighbour) (send neighbour set-style 'default) (send neighbour refresh)))))
  (move-in-dimension d)
  (apply-at-coord current-pos
                  (λ (child) (send* child (apply-to-neighbours (λ (neighbour) (send neighbour set-style 'influenced) (send neighbour refresh)))
                               (set-style 'focused)
                               (refresh)))))

(define frame2%
        (class frame%
          (super-new)
          (inherit get-focus-window)
          (define/override (on-activate v)
            ;(println "hihi! did an override :3")
            ;(println v)
            (unless (or v is-lost? is-revealed? is-won?)
              (pause-game)))
          (define/override (on-traverse-char event)
            (define key-code (send event get-key-code))
            (if (is-a? (get-focus-window) editor-canvas%)
                #f
                (begin
                  (unless (or is-revealed? is-lost? is-won?)
                    (when (equal? key-code 'down) (move move-up-in-dimension du-dimension))
                    (when (equal? key-code 'up) (move move-down-in-dimension du-dimension))
                    (when (equal? key-code 'right) (move move-up-in-dimension lr-dimension))
                    (when (equal? key-code 'left) (move move-down-in-dimension lr-dimension))
                    (when (equal? key-code #\s) (move move-up-in-dimension sw-dimension))
                    (when (equal? key-code #\w) (move move-down-in-dimension sw-dimension))
                    (when (equal? key-code #\d) (move move-up-in-dimension ad-dimension))
                    (when (equal? key-code #\a) (move move-down-in-dimension ad-dimension))
                    (when (equal? key-code #\space)
                      (apply-at-coord current-pos (λ (child) (send child on-event (new mouse-event% [event-type 'left-down]))))
                      (unless starting-click?
                        (run-game)))
                    (when (or (equal? key-code #\e) (equal? key-code #\m))
                      (apply-at-coord current-pos (λ (child) (send child on-event (new mouse-event% [event-type 'right-down]))))
                      (unless starting-click?
                        (run-game))))
                  (if (or (equal? key-code #\p) (equal? key-code #\P))
                      (let ()
                        (toggle-pause)
                        #t)
                      #f))))
          (define/override (on-subwindow-event receiver event)
            (unless starting-click?
              (when (and (send event button-down?) (is-a? receiver mine-button%))
                (run-game)))
            (when (get-focus-window)
              (send (get-focus-window) focus))
            #f)))

(define frame (new frame2% [label "Multidimensional minesweeper"] [style (list 'float)]))

; Make a static text message in the frame
(define msg (new message% [parent (new horizontal-pane% [parent frame] [alignment '(center top)] [stretchable-height #f] [stretchable-width #f])]
                 [label "No events so far..."]
                 [stretchable-width #t]
                 [auto-resize #t]
                 [style (when debug-on? '(deleted))]))

(define (coords->string coords) (~a coords))

(define (generate-warning string parent [pos #f] [warn-color (make-object color% "red")])
  (new canvas% [parent parent]
       [stretchable-width #f]
       [stretchable-height #f]
       [paint-callback (λ (canvas dc)
                         (send canvas set-canvas-background (make-object color% 0 0 0 0))
                         (send dc set-font normal-control-font)
                         (define-values (tw th d v) (send dc get-text-extent string))
                         (send canvas min-width (inexact->exact (ceiling tw)))
                         (send canvas min-height (inexact->exact (ceiling th)))
                         (define safe-color (get-label-foreground-color))
                         (send dc set-text-foreground safe-color)
                         (if pos
                             (let ([end (for/fold ([s 0]
                                                   [o 0]
                                                   #:result o)
                                                  ([i pos])
                                          (let ([safe (substring string s (car i))]
                                                [warn (substring string (car i) (cdr i))])
                                            (define-values (tws ths ds vs) (send dc get-text-extent safe))
                                            (send dc draw-text safe o 0)
                                            (send dc set-text-foreground warn-color)
                                            (define-values (tww thw dw vw) (send dc get-text-extent warn))
                                            (send dc draw-text warn (+ o tws) 0)
                                            (send dc set-text-foreground safe-color)
                                            (values (cdr i) (+ o tws tww))))])
                               (unless (= (string-length string) (cdr (last pos)))
                                 (send dc draw-text (substring string (cdr (last pos))) end 0)))
                             (send dc draw-text string))
                         (send canvas enable #f))]))

(define input-box%
  (class vertical-panel%
    (init-field label)
    (init-field enter)
    (init-field (init-value ""))
    (init-field (alignment '(center top)))
    (field (input-error #f))
    (super-new [alignment alignment]
               [stretchable-width #f]
               [stretchable-height #f])
    (define input-container (new horizontal-panel% [parent this]
                                 [stretchable-width #f]
                                 [stretchable-height #f]))
    (new message% [parent input-container]
         [label label])
    (define input-field (new text%))
    (define editor-canvas (new editor-canvas% [parent input-container]
                               [editor input-field]
                               [style '(hide-hscroll no-vscroll)]
                               [min-width 200]))
    (define-values (tw th d v) (send (send input-field get-dc) get-text-extent "0"))
    (send editor-canvas vertical-inset 2)
    (send editor-canvas horizontal-inset 2)
    (send editor-canvas min-height (+ (exact-truncate th) (* (send editor-canvas vertical-inset) 2)))
    (send editor-canvas set-canvas-background (get-label-background-color))
    (define style-delta (make-object style-delta%))
    (if (> (send (get-label-background-color) red) 127)
        (send style-delta set-delta-foreground "black")
        (send style-delta set-delta-foreground "white"))
    (send input-field change-style style-delta)
    (define/public (set-value v)
      (send* input-field (erase)
        (change-style style-delta)
        (insert v 0)))
    (set-value init-value)
    (define/public (set-input-error v) (set! input-error v))
    (define/public (get-value) (send input-field get-text 0 'eof #t))
    (define/override (on-subwindow-char window key-event)
      ;(println (send event get-key-code))
      (define key-code (send key-event get-key-code))
      (if (or (equal? key-code #\return) (equal? key-code 'numpad-enter))
          (let ()
            (enter this)
            #t)
          #f))
    (define/public (generate-warnings input [pos #f] [label #f] [warn-color (make-object color% "red")])
      (let ([hp (new horizontal-panel% [parent this]
                     [alignment alignment])])
        (when label
          (new message% [parent hp]
               [label label]))
        (generate-warning input hp pos warn-color)))
    (define/public (delete-warnings)
      (for ([i (cdr (send this get-children))])
        (send this delete-child i)))))

(define input-generate-container (new horizontal-panel% [parent frame]
                                      [alignment '(center center)]
                                      [stretchable-width #f]
                                      [stretchable-height #f]))

(define input-container (new vertical-panel% [parent input-generate-container]
                             [alignment '(right center)]))

(define seed-input-box (new input-box% [parent input-container]
                            [label "Seed"]
                            [enter (λ (input-box)
                                     (define input (string-trim (send input-box get-value)))
                                     (send input-box delete-warnings)
                                     (if (regexp-match-exact? #px"\\d+" input)
                                         (let ()
                                           (send input-box set-input-error #f)
                                           (set! seed-new (string->number input)))
                                         (let ([pos (regexp-match-positions* #px"[^\\d]+" input)])
                                           (send input-box set-input-error #t)
                                           (send input-box generate-warnings input pos "Invalid characters: "))))]
                            [init-value (number->string seed)]))

(new input-box% [parent input-container]
     [label "Mines"]
     [enter (λ (input-box)
              (define input (string-trim (send input-box get-value)))
              (send input-box delete-warnings)
              (if (regexp-match-exact? #px"\\d+" input)
                  (let ([mines (string->number input)])
                    (if (> mines field-size)
                        (let ()
                          (send input-box set-input-error #t)
                          (send input-box generate-warnings (~a mines " is too big! Max value is " field-size)))
                        (let ()
                          (send input-box set-input-error #f)
                          (set! amount-new (string->number input)))))
                  (let ([pos (regexp-match-positions* #px"[^\\d]+" input)])
                    (send input-box set-input-error #t)
                    (send input-box generate-warnings input pos "Invalid characters: "))))]
     [init-value (number->string amount)])

(new input-box% [parent input-container]
     [label "Dimensions"]
     [enter (λ (input-box)
                 (define input (send input-box get-value))
                 (define purified
                   (flatten (map (λ (n)
                                   (match n
                                     [n #:when (regexp-match-exact? #px"\\d+" n) (string->number n)]
                                     [n #:when (regexp-match-exact? #px"\\d+\\^\\d+" n)
                                        (define l (string-split n "^"))
                                        (make-list (string->number (cadr l)) (string->number (car l)))]
                                     [n #f]))
                                 (regexp-split #px"\\s*,\\s*|\\s+" input))))
                 (define input-error (get-field input-error input-box))
                 (if (member #f purified)
                     (let ()
                       (if input-error (send input-box delete-warnings) (send input-box set-input-error #t))
                       (define wrong-chars-pos (regexp-match-positions* #px"[^\\d\\^\\d\\s]+" input))
                       (define wrong-syntax-pos (regexp-match-positions* #px"(?!\\d+\\^\\d+)(?<!\\d)\\d*\\^\\d*" input))
                       (unless (empty? wrong-chars-pos)
                         (send input-box generate-warnings input wrong-chars-pos "Invalid characters: "))
                       (unless (empty? wrong-syntax-pos)
                         (send input-box generate-warnings input wrong-syntax-pos "Wrong syntax: ")))
                     (let ()
                       (when input-error
                         (send input-box set-input-error #f)
                         (send input-box delete-warnings))
                       (set! field-dimensions-new purified))))]
     [init-value (let ([string (coords->string field-dimensions)]) (substring string 1 (sub1 (string-length string))))])

(define generate-container (new vertical-panel% [parent input-generate-container]
                                [alignment '(center center)]))

(define random-seed-button (new button% [parent generate-container]
                                [label "Random seed"]
                                [callback (λ (button event)
                                            (random-seed (current-seconds))
                                            (send seed-input-box set-value (number->string (random 4294967087))))]))

(define generate-button (new button% [parent generate-container]
                             [label "Generate!"]
                             [callback (λ (button event)
                                         (when (for/and ([i (send input-container get-children)])
                                                 (send i on-subwindow-char
                                                       (car (send i get-children))
                                                       (new key-event% [key-code #\return]))
                                                 (not (get-field input-error i)))
                                           (begin-busy-cursor)
                                           (send mine-field-container enable #t)
                                           (send mine-field delete-child (car (send mine-field get-children)))
                                           (set!-values (amount seed field-dimensions is-revealed? is-lost? is-won?)
                                                        (values amount-new seed-new field-dimensions-new #f #f #f))
                                           (populate-all)
                                           (println largest-value)
                                           (send state set-label "New game")
                                           (set! old-state (send state get-label))
                                           (set! starting-click? #f)
                                           (set! pause-times '())
                                           (set! time-accumulator 0)
                                           (set! clicked-fields 0)
                                           (set! field-size (foldl * 1 field-dimensions))
                                           (set! current-pos (make-list (length field-dimensions) 0))
                                           (set! lr-dimension 0)
                                           (set! du-dimension 1)
                                           (set! ad-dimension 2)
                                           (set! sw-dimension 3)
                                           (end-busy-cursor)
                                           (println "generated")))]))

(define-values (input-generate-container-width input-generate-container-height) (send input-generate-container get-graphical-min-size))
(define-values (input-container-width input-container-height) (send input-container get-graphical-min-size))
(define-values (generate-container-width generate-container-height) (send generate-container get-graphical-min-size))

(define mode-pause-container (new horizontal-pane% [parent frame]
                                  [alignment '(center center)]
                                  [min-width input-generate-container-width]
                                  [stretchable-width #f]
                                  [stretchable-height #f]))

(new check-box% [parent (new pane% [parent mode-pause-container]
                             [min-width input-container-width])]
     [label "Relative mode"]
     [callback (λ (check-box event)
                 (set! is-relative? (not is-relative?))
                 (if (or is-lost? is-revealed?)
                     (apply-to-all field-dimensions (car (send mine-field get-children)) (λ (child) (send child draw-temporary (cons 'default 'uncovered))))
                     (apply-to-all field-dimensions (car (send mine-field get-children)) (λ (child) (unless (send child is-covered?) (send child update-layer2) (send child refresh))))))]
     [value is-relative?])

(new button% [parent (new pane% [parent mode-pause-container]
                          [min-width generate-container-width])]
     [label "Pause"]
     [callback (λ (button event) (toggle-pause))])

(define state-reveal-container (new horizontal-panel% [parent frame]
                                    [min-width input-generate-container-width]
                                    [stretchable-width #f]
                                    [stretchable-height #f]))

(define state-container (new horizontal-panel% [parent state-reveal-container]
                             [alignment '(left center)]
                             [min-width input-container-width]))

(define state (new message% [parent state-container]
                   [label "New game"]
                   [horiz-margin 0]
                   [min-width (exact-truncate (* input-container-width 2/3))]
                   [stretchable-width #f]))

(define old-state (send state get-label))

(define timer-message (new message% [parent state-container]
                           [label "0"]
                           [horiz-margin 0]))

(send timer-message show #f)

(define timer (new timer% [notify-callback (λ ()
                                             (set! time-accumulator (add1 time-accumulator))
                                             (send timer-message set-label
                                                   (let ()
                                                         (define-values (q t) (quotient/remainder time-accumulator 6000))
                                                         (string-append (number->string q) ":" (number->string (exact->inexact (/ t 100)))))))]
                   [interval #f]))

(new button% [parent (new pane% [parent state-reveal-container]
                          [min-width generate-container-width])]
     [label "Reveal all"]
     [callback (λ (button event) (reveal-game) (send state set-label "Game revealed"))])

(define mine-field-container (new panel% [parent frame]
                                  [style (list 'auto-hscroll 'auto-vscroll)]
                                  [alignment '(center top)]))

(define mine-field (new pane% [parent mine-field-container]
                      [spacing 0]
                      [stretchable-width #f]
                      [stretchable-height #f]))

(define flag (make-object bitmap% flag-img 'unknown/alpha))
(define bomb (make-object bitmap% bomb-img 'unknown/alpha))

(define covered (make-object bitmap% covered-img))
(define-values (covered-width covered-height covered-color-depth) (values (send covered get-width) (send covered get-height) (if (send covered is-color?) 4 1)))
(define covered-size (* covered-width covered-height covered-color-depth))
(define uncovered (make-object bitmap% uncovered-img))
(define-values (uncovered-width uncovered-height uncovered-color-depth) (values (send uncovered get-width) (send uncovered get-height) (if (send uncovered is-color?) 4 1)))
(define uncovered-size (* uncovered-width uncovered-height uncovered-color-depth))
(define default (cons covered uncovered))

(define (scale-from-pixels-to pixels color-len pixel)
  (for* ([i (in-range 0 (bytes-length pixels) color-len)]
         [c (in-range 0 color-len)])
    (define v (+ i c))
    (bytes-set! pixels v (exact-truncate (* (/ (bytes-ref pixels v) 255) (bytes-ref pixel c))))))

(define (mod n m)
  (define fn (floor n))
  (+ (modulo fn m) (- n fn)))

(define my-color%
  (class object%
    (super-new)
    (init-field c0)
    (init-field c1)
    (init-field c2)
    (init-field (alpha 255))
    (init-field (is-hsv? #f))
    (define/private (rgb->hsv R G B)
      (let ([R (/ R 255)]
            [G (/ G 255)]
            [B (/ B 255)])
        (define-values
          (m-max m-min)
          (values (max R G B) (min R G B)))
        (define ∆ (- m-max m-min))
        (values
         (if (= ∆ 0) ;; H
             0
             (match m-max
               [m-max #:when (= m-max R) (mod (/ (- G B) ∆) 6)]
               [m-max #:when (= m-max G) (+ (/ (- B R) ∆) 2)]
               [m-max (+ (/ (- R G) ∆) 4)])) ;; #:when (= m-max B)
         (if (= m-max 0) 0 (/ ∆ m-max)) ;; S
         m-max))) ;; V
    (define/private (hsv->rgb H S V)
      (define (scale n) (* (+ n m) 255))
      (define C (* V S))
      (define X (* C (- 1 (abs (sub1 (mod H 2))))))
      (define m (- V C))
      (let ([C (scale C)]
            [X (scale X)]
            [m (scale 0)])
        (match (modulo (floor H) 6)
          [H #:when (= H 0) (values C X m)]
          [H #:when (= H 1) (values X C m)]
          [H #:when (= H 2) (values m C X)]
          [H #:when (= H 3) (values m X C)]
          [H #:when (= H 4) (values X m C)]
          [H #:when (= H 5) (values C m X)])))
    (define-values
      (R G B H S V)
      (if is-hsv?
          (call-with-values (λ () (hsv->rgb c0 c1 c2)) (λ (r g b) (values r g b c0 c1 c2)))
          (call-with-values (λ () (rgb->hsv c0 c1 c2)) (λ (h s v) (values c0 c1 c2 h s v)))))
    (define/public (my-color->color) (make-object color% (exact-truncate R) (exact-truncate G) (exact-truncate B)  (exact-truncate (/ alpha 255))))
    (define/public (my-color-with-hsv->color h s v)
      (call-with-values (λ () (hsv->rgb (if h h H)
                                        (if s s S)
                                        (if v v V)))
                        (λ (r g b) (make-object color% (exact-truncate r) (exact-truncate g) (exact-truncate b)  (exact-truncate (/ alpha 255))))))
    (define/public (get-rgb) R G B)
    (define/public (get-argb) alpha R G B)
    (define/public (get-hsv) H S V)
    (define/public (get-ahsv) alpha H S V)
    (define/public (get-alpha) alpha)
    (define/public (get-bytes) (bytes (exact-truncate alpha) (exact-truncate R) (exact-truncate G) (exact-truncate B)))
    (define/public (get-bytes-with-hsv h s v)
      (call-with-values (λ () (hsv->rgb (if h h H)
                                        (if s s S)
                                        (if v v V)))
                        (λ (r g b) (bytes (exact-truncate alpha) (exact-truncate r) (exact-truncate g) (exact-truncate b)))))
    (define/public (get-hex) (string-append "#" (apply string-append (map (λ (n) (number->string (exact-truncate n) 16)) (list alpha R G B)))))
    ;(define/public (get-list) (list R G B H S V))
    (define/public (set-rgb r g b)
      (set!-values (R G B H S V)
                   (call-with-values (λ () (rgb->hsv r g b)) (λ (h s v) (values r g b h s v)))))
    (define/public (set-argb a r g b)
      (set! alpha a)
      (set-rgb r g b))
    (define/public (set-r r) (set-rgb r G B))
    (define/public (set-g g) (set-rgb R g B))
    (define/public (set-b b) (set-rgb R G b))
    (define/public (set-hsv h s v)
      (set!-values (R G B H S V)
                   (call-with-values (λ () (hsv->rgb h s v)) (λ (r g b) (values r g b h s v)))))
    (define/public (set-ahsv a h s v)
      (set! alpha a)
      (set-hsv h s v))
    (define/public (set-h h) (set-hsv h S V))
    (define/public (set-s s) (set-hsv H s V))
    (define/public (set-v v) (set-hsv H S v))
    (define/public (set-alpha a) (set! alpha a))))

(println (list highlight-red highlight-green highlight-blue))
(println (list (highlight-red) (highlight-green) (highlight-blue)))
(define hotpink (make-object my-color% (highlight-red) (highlight-green) (highlight-blue)))

(define focused (cons (make-object bitmap% covered-img) (make-object bitmap% uncovered-img)))
(let ([focused-covered-pixels (make-bytes covered-size)])
  (send (car focused) get-argb-pixels 0 0 covered-width covered-height focused-covered-pixels)
  (scale-from-pixels-to focused-covered-pixels covered-color-depth (send hotpink get-bytes))
  (send (car focused) set-argb-pixels 0 0 covered-width covered-height focused-covered-pixels))
(let ([focused-uncovered-pixels (make-bytes uncovered-size)])
  (send (cdr focused) get-argb-pixels 0 0 uncovered-width uncovered-height focused-uncovered-pixels)
  (scale-from-pixels-to focused-uncovered-pixels covered-color-depth (send hotpink get-bytes))
  (send (cdr focused) set-argb-pixels 0 0 uncovered-width uncovered-height focused-uncovered-pixels))
(send hotpink set-s (influenced-saturation))
(define influenced (cons (make-object bitmap% covered-img) (make-object bitmap% uncovered-img)))
(let ([influenced-covered-pixels (make-bytes covered-size)])
  (send (car influenced) get-argb-pixels 0 0 covered-width covered-height influenced-covered-pixels)
  (scale-from-pixels-to influenced-covered-pixels covered-color-depth (send hotpink get-bytes))
  (send (car influenced) set-argb-pixels 0 0 covered-width covered-height influenced-covered-pixels))
(let ([influenced-uncovered-pixels (make-bytes uncovered-size)])
  (send (cdr influenced) get-argb-pixels 0 0 uncovered-width uncovered-height influenced-uncovered-pixels)
  (scale-from-pixels-to influenced-uncovered-pixels covered-color-depth (send hotpink get-bytes))
  (send (cdr influenced) set-argb-pixels 0 0 uncovered-width uncovered-height influenced-uncovered-pixels))
(define grey (make-object my-color% 0 0 (paused-value) 255 #t))
(define paused (cons (make-object bitmap% covered-img) (make-object bitmap% uncovered-img)))
(let ([paused-covered-pixels (make-bytes covered-size)])
  (send (car paused) get-argb-pixels 0 0 covered-width covered-height paused-covered-pixels)
  (scale-from-pixels-to paused-covered-pixels covered-color-depth (send grey get-bytes))
  (send (car paused) set-argb-pixels 0 0 covered-width covered-height paused-covered-pixels))
(let ([paused-uncovered-pixels (make-bytes uncovered-size)])
  (send (cdr paused) get-argb-pixels 0 0 uncovered-width uncovered-height paused-uncovered-pixels)
  (scale-from-pixels-to paused-uncovered-pixels covered-color-depth (send grey get-bytes))
  (send (cdr paused) set-argb-pixels 0 0 uncovered-width uncovered-height paused-uncovered-pixels))

(define (print-icons)
  (print (car default))
  (print (car focused))
  (println (car influenced))
  (print (cdr default))
  (print (cdr focused))
  (println (cdr influenced)))

(print-icons)

(define (apply-to-neighbours-outer coords [dimensions field-dimensions] [parent (car (send mine-field get-children))] [proc (λ (child) #f)])
  (if (empty? dimensions)
      (proc parent)
      (let ([index (- (length coords) (length dimensions))])
        (for ([i (in-range (max (sub1 (list-ref coords index)) 0) (min (+ (list-ref coords index) 2) (last dimensions)))])
          (apply-to-neighbours-outer coords (drop-right dimensions 1) (list-ref (send parent get-children) i) proc)))))

(define (get-from-neighbours-outer coords [dimensions field-dimensions] [parent (car (send mine-field get-children))] [proc (λ (child) #f)])
  (if (empty? dimensions)
      (proc parent)
      (let ([index (- (length coords) (length dimensions))])
        (for/list ([i (in-range (max (sub1 (list-ref coords index)) 0) (min (+ (list-ref coords index) 2) (last dimensions)))])
          (get-from-neighbours-outer coords (drop-right dimensions 1) (list-ref (send parent get-children) i) proc)))))

(define (apply-to-all [dimensions field-dimensions] [parent (car (send mine-field get-children))] [proc (λ (child) #f)])
  (if (empty? dimensions)
      (proc parent)
      (for ([i (send parent get-children)])
        (apply-to-all (drop-right dimensions 1) i proc))))

(define (get-from-all [dimensions field-dimensions] [parent (car (send mine-field get-children))] [proc (λ (child) #f)])
  (if (empty? dimensions)
      (proc parent)
      (for/list ([i (send parent get-children)])
        (get-from-all (drop-right dimensions 1) i proc))))

(define number-color (make-object my-color% 0 1 1 255 #t))

(define/contract mine-button%
  (class/c (field [coords (non-empty-listof exact-positive-integer?)])
           (field [style (or/c ;(cons/c (is-a?/c bitmap%) (is-a?/c bitmap%))
                          ;(one-of/c 'default 'influenced 'focused)
                          (cons/c (cons/c (is-a?/c bitmap%) (is-a?/c bitmap%)) (one-of/c 'covered 'uncovered))
                          (cons/c (one-of/c 'default 'influenced 'focused 'paused) (one-of/c 'covered 'uncovered)))])
           (field [width exact-positive-integer?])
           (field [height exact-positive-integer?])
           (field [left-down (-> (is-a?/c mine-button%) (is-a?/c event%) any)])
           [set-style (case->m (-> (or/c (cons/c (is-a?/c bitmap%) (is-a?/c bitmap%))
                                         (one-of/c 'default 'influenced 'focused 'paused)
                                         (cons/c (cons/c (is-a?/c bitmap%) (is-a?/c bitmap%)) (one-of/c 'covered 'uncovered))
                                         (cons/c (one-of/c 'default 'influenced 'focused 'paused) (one-of/c 'covered 'uncovered)))
                                   void?)
                               (-> (or/c (cons/c (is-a?/c bitmap%) (is-a?/c bitmap%))
                                         (one-of/c 'default 'influenced 'focused 'paused))
                                   (one-of/c 'covered 'uncovered)
                                   void?))]
           [set-mine (->m boolean? void?)])
  (class* canvas% (writable<%>)
    (init-field coords)
    (init-field (mine? #f))
    (init-field (style (cons 'default 'covered)))
    (init-field (width (img-width)))
    (init-field (height (img-height)))
    (init-field (left-down (λ (this event) #f)))
    (init-field (right-down (λ (this event) #f)))
    (init-field (enter (λ (this event) #f)))
    (init-field (leave (λ (this event) #f)))
    (define bitmaps #f)
    (define draw #f)
    (define bitmap-width #f)
    (define bitmap-height #f)
    (define width-scaling #f)
    (define height-scaling #f)
    (define/private (adjust-bitmap-values)
      (set! bitmap-width (send draw get-width))
      (set! bitmap-height (send draw get-height))
      (set! width-scaling (/ width bitmap-width))
      (set! height-scaling (/ height bitmap-height)))
    (define/private (match-style s)
      (set! bitmaps
            (match s
              [(cons (cons a b) c) (set! style (cons 'custom c)) (cons a b)]
              [(cons a b) #:when (equal? a 'default) (set! style s) default]
              [(cons a b) #:when (equal? a 'influenced) (set! style s) influenced]
              [(cons a b) #:when (equal? a 'focused) (set! style s) focused]
              [(cons a b) #:when (equal? a 'paused) (set! style s) paused]))
      (set! draw
            (if (equal? (cdr style) 'covered)
                (car bitmaps)
                (cdr bitmaps))))
    (match-style style)
    (adjust-bitmap-values)
    (define/public set-style
      (case-λ [(a) (match a
                     [(cons x y) (match-style a)
                                 (unless (equal? y (cdr style)) (adjust-bitmap-values))]
                     [x (match-style (cons a (cdr style)))])]
              [(a b) (match-style (cons a b))
                     (unless (equal? b (cdr style)) (adjust-bitmap-values))]))
    (define/public (get-style) style)
    (define/public (get-bitmaps) bitmaps)
    (define layer2 #f)
    (define/public (update-layer2)
      (set! layer2 (new bitmap-dc% [bitmap (make-object bitmap% bitmap-width bitmap-height #f #t)]))
      (unless (is-covered?)
        (if mine?
            (send layer2 draw-bitmap bomb 0 0)
            (unless flag?
              (let ([value (if is-relative? relative-value value)])
                (define-values (tw th d v) (send layer2 get-text-extent (number->string value)))
                (if (zero? value)
                    (when (foldl (λ (a result) (or a result)) #f (flatten (get-from-neighbours (λ (child) (and (send child is-covered?) (not (get-field flag? child)))))))
                      (send layer2 set-text-foreground (send number-color my-color-with-hsv->color 0 0 0))
                      (send layer2 draw-text ;; should probably change the font to something usefull
                            (number->string value)
                            (/ (- bitmap-width tw) 2)
                            (/ (- bitmap-height th) 2)))
                    (begin
                      (send layer2 set-text-foreground
                            (if (positive? value)
                              (send number-color my-color-with-hsv->color (* (/ value largest-value) 2) #f #f)
                              (send number-color my-color-with-hsv->color (+ (* (/ value largest-value) 2) 3) #f #f)))
                      (send layer2 draw-text ;; should probably change the font to something usefull
                            (number->string value)
                            (/ (- bitmap-width tw) 2)
                            (/ (- bitmap-height th) 2))))))))
      (when flag?
        (unless (and (equal? (car style) 'paused) (hide-on-pause?))
          (send layer2 draw-bitmap flag 0 0))))
    (super-new [min-width width]
               [min-height height]
               [stretchable-width #f]
               [stretchable-height #f])
    (define dc (send this get-dc))
    (define/private (update-scale)
      (send dc set-scale width-scaling height-scaling))
    (update-scale)
    (field (string-coords (coords->string coords)))
    (define/public (set-mine m)
      (set! mine? m))
    (field (flag? #f))
    (define/public (toggle-flag)
      (if flag?
          (begin
            (set! flag? #f)
            (apply-to-neighbours (λ (child) (send child inc) (send child refresh))))
          (begin
            (set! flag? #t)
            (apply-to-neighbours (λ (child) (send child dec) (send child refresh)))))
      (update-layer2))
    (define/public (is-covered?) (equal? (cdr style) 'covered))
    (define/public (uncover)
      (when (is-covered?)
        (set! clicked-fields (add1 clicked-fields))
        (set! style (cons (car style) 'uncovered))
        (set! draw (cdr bitmaps))
        (adjust-bitmap-values)
        (update-scale)))
    (define/public (get-state)
      (values (cdr style) (if flag? 'flaged 'unflaged)))
    (field (value 0))
    (define/public (set-value v : Natural)
      (set! value v))
    (field (relative-value 0))
    (define/public (inc-all)
      (set! value (add1 value))
      (set! relative-value (add1 relative-value)))
    (define/public (inc)
      (set! relative-value (add1 relative-value))
      (unless (is-covered?)
        (update-layer2)))
    (define/public (dec-all)
      (set! value (sub1 value))
      (set! relative-value (sub1 relative-value)))
    (define/public (dec)
      (set! relative-value (sub1 relative-value))
      (unless (is-covered?)
        (update-layer2)))
    (define/public (get-value)
      (if mine? #f value))
    (define/public (get-relative-value)
      (if mine? #f relative-value))
    (define/public (get-display-value)
      (if is-relative? (get-relative-value) (get-value)))
    (update-layer2)
    (define/override (on-paint)
      (send dc draw-bitmap draw 0 0)
      (send dc draw-bitmap (send layer2 get-bitmap) 0 0))
    (define/public (draw-temporary . new-style)
      (let ([current-style style])
        (send/apply this set-style new-style)
        (update-layer2)
        (on-paint)
        (set-style current-style)
        (update-layer2)))
    (define/public (draw-reset)
      (adjust-bitmap-values)
      (update-scale)
      (on-paint))
    (define/override (on-event ev)
      (define type (send ev get-event-type))
      ;(println (~a type))
      (when (equal? type 'left-down)
        (left-down this ev))
      (when (equal? type 'right-down)
        (right-down this ev))
      (when (equal? type 'enter)
        (enter this ev))
      (when (equal? type 'leave)
        (leave this ev)))
    (define/public (get-state-string) (call-with-values (λ () (get-state)) (λ (a b) (~a a b #:separator " "))))
    (define/public (get-value-string) (if mine? "mine" (number->string value)))
    (define/public (get-relative-value-string) (if mine? "mine" (number->string relative-value)))
    (define/public (get-info)
      (string-append "state: " (get-state-string)
                     ", value: " (get-value-string)
                     ", relative value: " (get-relative-value-string)))
    (define/public (apply-to-neighbours (proc (λ (child) #f)))
      (apply-to-neighbours-outer coords field-dimensions (car (send mine-field get-children)) proc))
    (define/public (get-from-neighbours (proc (λ (child) #f)))
      (get-from-neighbours-outer coords field-dimensions (car (send mine-field get-children)) proc))
    (define/public (get-area-of-influence)
      (for/list ([i (reverse coords)]
                 [d (reverse field-dimensions)])
        (in-range (max (sub1 i) 0) (min (+ i 2) d))))
    (define/private (layer-all)
      (define bm-dc (new bitmap-dc% [bitmap (make-object bitmap% bitmap-width bitmap-height)]))
      (send bm-dc draw-bitmap draw 0 0)
      (if mine?
          (send bm-dc draw-bitmap bomb 0 0)
          (let ([value (if is-relative? relative-value value)])
            (unless flag?
              (define-values (tw th d v) (send bm-dc get-text-extent (number->string value)))
              (send bm-dc draw-text ;; should probably change the font to something usefull
                    (number->string value)
                    (/ (- bitmap-width tw) 2)
                    (/ (- bitmap-height th) 2)))))
      (when flag?
        (send bm-dc draw-bitmap flag 0 0))
      (send bm-dc get-bitmap))
    (define/public (custom-write port)
      (write (layer-all) port))
    (define/public (custom-display port)
      (display (layer-all) port))))

(define (populate dimensions [parent mine-field] [sm 5] [mine #f] [sp '()])
  (if (> (length dimensions) 1)
      (let ()
        (define b
          (let ()
            (define m (* (quotient (- (length dimensions) 1) 2) sm))
            (if (even? (length dimensions))
                (new vertical-panel% [parent parent]
                     [spacing m]
                     [alignment '(left top)])
                (new horizontal-panel% [parent parent]
                     [spacing m]
                     [alignment '(left top)]))))
        (for ([i (in-range (last dimensions))]) (populate (drop-right dimensions 1) b sm mine (append sp (list i)))))
      (let ()
        (define b (new horizontal-panel% [parent parent]
                       [spacing 0]
                       [alignment '(left top)]))
        (for ([i (in-range (car dimensions))]) (new mine-button% [parent b]
                                                    [coords (append sp (list i))]
                                                    [mine? mine]
                                                    [style (cons 'default 'covered)]
                                                    [vert-margin 0]
                                                    [horiz-margin 0]
                                                    [left-down
                                                     (λ (button event)
                                                       (send msg set-label
                                                             (string-append
                                                              (get-field string-coords button)
                                                              " "
                                                              (send button get-info)))
                                                       (when (and (send button is-covered?) (not (get-field flag? button)))
                                                         (send button uncover)
                                                         (define value (send button get-display-value))
                                                         (if value
                                                           (begin
                                                             (println value)
                                                             (when (zero? value)
                                                               (begin-busy-cursor)
                                                               (send button apply-to-neighbours (λ (child) (send child on-event (new mouse-event% [event-type 'left-down]))))
                                                               (end-busy-cursor))
                                                             (if is-relative?
                                                                 (send button apply-to-neighbours (λ (child) (send child update-layer2) (send child refresh)))
                                                                 (begin
                                                                   (send button update-layer2)
                                                                   (send button refresh)))
                                                             (when (= clicked-fields (- field-size amount))
                                                               (win-game)))
                                                           (lose-game))))]
                                                    [right-down
                                                     (λ (button event)
                                                       (send button toggle-flag)
                                                       (if is-relative?
                                                           (send button apply-to-neighbours (λ (child) (send child update-layer2) (send child refresh)))
                                                           (send button refresh))
                                                       (send msg set-label
                                                             (string-append
                                                              (get-field string-coords button)
                                                              " "
                                                              (send button get-info))))]
                                                    [enter
                                                     (λ (button event)
                                                       (send button apply-to-neighbours (λ (child)
                                                                                          (send child set-style 'influenced)
                                                                                          (send child refresh)))
                                                       (send button set-style 'focused))]
                                                    [leave
                                                     (λ (button event)
                                                       (send button apply-to-neighbours (λ (child)
                                                                                          (send child set-style 'default)
                                                                                          (send child refresh))))])))))

(define (set-mine-at-coord coord
                           [v #t]
                           [mine-setter (λ (parent v)
                                          (send parent apply-to-neighbours (λ (child) (send child inc-all)))
                                          (send parent set-mine v))]
                           [parent (car (send mine-field get-children))])
  (if (empty? coord)
      (if (equal? (get-field mine? parent) (not v))
          (let ()
            (mine-setter parent v)
            #f)
          #t)
      (set-mine-at-coord (drop-right coord 1) v mine-setter (list-ref (send parent get-children) (last coord)))))

(define (populate-field amount seed [v #t] [mine-setter (λ (parent v)
                                                          (send parent apply-to-neighbours (λ (child) (send child inc-all)))
                                                          (send parent set-mine v))]
                        [dimensions field-dimensions])
  (random-seed seed)
  (for ([i (in-range amount)])
    (for/and ([i (in-naturals)])
      (define coord
        (for/list ([d dimensions])
          (random 0 d)))
      (set-mine-at-coord coord v mine-setter))))

(define (populate-all [parent mine-field] [sm 10])
    (println field-size)
    (if (> amount field-size)
        #f
        (let ()
          (if (> amount (/ field-size 2))
              (let ()
                (println "removing mines")
                (populate field-dimensions parent sm #t)
                (println (- field-size amount))
                (populate-field (- field-size amount) seed #f (λ (parent v)
                                                          (send parent set-mine v)
                                                          (send parent apply-to-neighbours (λ (child) (if (get-field mine? child) (send parent inc-all) (send child dec-all))))
                                                          (send parent inc-all))))
              (let ()
                (println "adding mines")
                (populate field-dimensions parent sm)
                (populate-field amount seed)))
          (set! largest-value (apply max (flatten (get-from-all field-dimensions (car (send mine-field get-children)) (λ (child) (if (get-field mine? child) 0 (send child get-value)))))))
          #t)))

(populate-all)
(println largest-value)

(send frame show #t)