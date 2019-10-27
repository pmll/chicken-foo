;; cap frame rate
(import (prefix sdl2 "sdl2:")
        (prefix sdl2-image "img:")
        (prefix sdl2-ttf "ttf:")
        (chicken condition)
        (format))

(define screen-width 640)
(define screen-height 480)
(define screen-fps 60)
(define screen-tick-per-frame (quotient 1000 screen-fps))

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

(define screen-renderer (sdl2:create-renderer! window -1 '(accelerated)))

(define font (ttf:open-font "25_capping_frame_rate/lazy.ttf" 28))

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

(define (make-state ltimer frame-count in-progress)
  (list ltimer frame-count in-progress))

(define (state-ltimer state) (car state))

(define (state-frame-count state) (cadr state))

(define (state-in-progress? state) (caddr state))

(define (state-avg-fps state)
  (let* ((elapsed (/ (ltimer-ticks (state-ltimer state)) 1000.0))
         (avg (if (> elapsed 0) (/ (state-frame-count state) elapsed) 0)))
    (if (> avg 2000000) 0 avg)))

(define (process-state old-state)
  (let ((e (sdl2:poll-event!)))
    (make-state
      (state-ltimer old-state)
      (+ (state-frame-count old-state) 1)
      (not (sdl2:quit-event? e)))))

(define (main state)
  (when (state-in-progress? state)
    (let ((cap-timer (ltimer-start (make-ltimer #f 0 #f 0)))
          (fps-text-texture
          (make-ltexture-from-text
            (format "Average Frames Per Second (with cap) ~F" (state-avg-fps state))
            text-colour)))
      (begin
        (set! cap-timer cap-timer)
        (set! (sdl2:render-draw-colour screen-renderer)
            (sdl2:make-colour #xFF #xFF #xFF #xFF))
        (sdl2:render-clear! screen-renderer)
        (render-ltexture! fps-text-texture
                          (quotient (- screen-width
                                       (ltexture-w fps-text-texture)) 
                                    2)
                          (quotient (- screen-height
                                        (ltexture-h fps-text-texture))
                                    2)
                          #f)
	(let ((frame-ticks (ltimer-ticks cap-timer)))
	  (when (< frame-ticks screen-tick-per-frame)
	    (sdl2:delay! (- screen-tick-per-frame frame-ticks))))
        (sdl2:render-present! screen-renderer)
        (main (process-state state))))))

(main (make-state (ltimer-start (make-ltimer #f 0 #f 0)) 0 #t))

