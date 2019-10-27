;; another timer demo
(import (prefix sdl2 "sdl2:")
        (prefix sdl2-image "img:")
        (prefix sdl2-ttf "ttf:")
        (chicken condition)
        (format))

(define screen-width 640)
(define screen-height 480)

;; Initialise SDL
(sdl2:set-main-ready!)
(sdl2:init! '(video) '(events))
(ttf:init!)

;; Schedule quit! to be automatically called on normal exit
(on-exit sdl2:quit!)

;; Install a custom exception handler that will call quit! then call the
;; original exception handler. This will insure that quit! is called even
;; if an unhandled exception reaches the top level
(current-exception-handler
 (let ((original-handler (current-exception-handler)))
   (lambda (exception)
     (sdl2:quit!)
     (original-handler exception))))

(define window
  (sdl2:create-window! "SDL Tutorial" 0 0 screen-width screen-height))

(define screen-renderer (sdl2:create-renderer! window -1 '(accelerated present-vsync)))

(define font (ttf:open-font "23_advanced_timers/lazy.ttf" 28))

(define (make-ltexture file)
  (let ((file-surface (img:load file)))
    (set! (sdl2:surface-colour-key file-surface)
          (sdl2:map-rgb (sdl2:surface-format file-surface) #x0 #xFF #xFF))
    (list (sdl2:create-texture-from-surface screen-renderer file-surface)
          (sdl2:surface-w file-surface)
          (sdl2:surface-h file-surface))))

(define (make-ltexture-from-text text colour)
  (let ((text-surface (ttf:render-text-solid font text colour)))
    (list (sdl2:create-texture-from-surface screen-renderer text-surface)
          (sdl2:surface-w text-surface) 
          (sdl2:surface-h text-surface))))

(define (ltexture-w ltexture) (cadr ltexture))

(define (ltexture-h ltexture) (caddr ltexture))

(define (render-ltexture! ltexture x y clip)
  (sdl2:render-copy!
    screen-renderer
    (car ltexture)
    clip
    (sdl2:make-rect x
                    y
                    (if clip (sdl2:rect-w clip) (cadr ltexture))
                    (if clip (sdl2:rect-h clip) (caddr ltexture)))))

(define (make-ltimer started start-ticks paused pause-ticks)
  (list started start-ticks paused pause-ticks))

(define (ltimer-started? ltimer) (car ltimer))

(define (ltimer-start-ticks ltimer) (cadr ltimer))

(define (ltimer-paused? ltimer) (and (car ltimer) (caddr ltimer)))

(define (ltimer-pause-ticks ltimer) (cadddr ltimer))

(define (ltimer-start ltimer)
  (if (not (ltimer-started? ltimer))
      (make-ltimer #t (sdl2:get-ticks) #f 0)
      ltimer))

(define (ltimer-stop) (make-ltimer #f 0 #f 0))

(define (ltimer-pause ltimer)
  (if (and (ltimer-started? ltimer) (not (ltimer-paused? ltimer)))
      (make-ltimer #t
                   (ltimer-start-ticks ltimer)
                   #t
                   (- (sdl2:get-ticks) (ltimer-start-ticks ltimer)))
      ltimer))

(define (ltimer-unpause ltimer)
  (if (and (ltimer-started? ltimer) (ltimer-paused? ltimer))
      (make-ltimer #t
                   (- (sdl2:get-ticks) (ltimer-pause-ticks ltimer))
                   #f
                   0)
      ltimer))

(define (ltimer-ticks ltimer)
  (if (ltimer-started? ltimer)
      (if (ltimer-paused? ltimer)
          (ltimer-pause-ticks ltimer)
          (- (sdl2:get-ticks) (ltimer-start-ticks ltimer)))
      0))

(define text-colour (sdl2:make-colour 0 0 0))

(define start-prompt-texture
  (make-ltexture-from-text "Press S to Start or Stop the Timer" text-colour))

(define pause-prompt-texture
  (make-ltexture-from-text "Press P to Pause or Unpause the Timer" text-colour))

(define (make-state ltimer in-progress) (list ltimer in-progress))

(define (state-ltimer state) (car state))

(define (state-in-progress? state) (cadr state))

(define (process-state old-state)
  (let* ((e (sdl2:poll-event!))
         (key (if (and (sdl2:keyboard-event? e)
                       (eq? (sdl2:event-type e) 'key-down))
                  (sdl2:keyboard-event-sym e)
                  #f)))
    (make-state
      (cond ((eq? key 's)
             (if (ltimer-started? (state-ltimer old-state))
                 (ltimer-stop)
                 (ltimer-start (state-ltimer old-state))))
            ((eq? key 'p)
             (if (ltimer-paused? (state-ltimer old-state))
                 (ltimer-unpause (state-ltimer old-state))
                 (ltimer-pause (state-ltimer old-state))))
            (else (state-ltimer old-state)))
      (not (sdl2:quit-event? e)))))

(define (main state)
  (when (state-in-progress? state)
    (let ((time-text-texture
          (make-ltexture-from-text
            (format "Seconds since start time ~D"
                    (/ (ltimer-ticks (state-ltimer state)) 1000))
                    text-colour)))
      (begin
        (set! (sdl2:render-draw-colour screen-renderer)
            (sdl2:make-colour #xFF #xFF #xFF #xFF))
        (sdl2:render-clear! screen-renderer)
        (render-ltexture! start-prompt-texture 
                          (quotient (- screen-width
                                       (ltexture-w start-prompt-texture))
                                    2)
                          0
                          #f)
        (render-ltexture! pause-prompt-texture
                          (quotient (- screen-width
                                       (ltexture-w pause-prompt-texture))
                                    2)
                          (ltexture-h start-prompt-texture)
                          #f)
        (render-ltexture! time-text-texture
                          (quotient (- screen-width
                                       (ltexture-w time-text-texture)) 
                                    2)
                          (quotient (- screen-height
                                        (ltexture-h time-text-texture))
                                    2)
                          #f)
        (sdl2:render-present! screen-renderer)
        (main (process-state state))))))

(main (make-state (make-ltimer #f 0 'f 0) #t))

