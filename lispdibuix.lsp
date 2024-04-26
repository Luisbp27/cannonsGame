(defun inici ()
    "inici del programa (tecles)"
    (putprop 'programa 0 'angle)
    (repeteix))

(defun inici-cli ()
    "inici del programa (CLI)"
    (putprop 'programa 0 'angle)
    (repeteix-cli))

(defun inc-angle ()
    "incrementa l'angle"
    (putprop 'programa (+ (get 'programa 'angle) 1) ' angle))

(defun dec-angle ()
    "decrementa l'angle"
    (putprop 'programa (- (get 'programa 'angle) 1) ' angle))

(defun repeteix ()
    (pinta)
    (princ "Pitja fletxa cap amunt o cap avall o ESC.")
    (terpri)
    (setq tecla (get-key))
    (cond ((equal tecla 328) ; fletxa cap amunt
           (inc-angle) (repeteix)) ; incrementa angle i repeteix
          ((equal tecla 336) ; fletxa cap avall
           (dec-angle) (repeteix)) ; decrementa angle i repeteix
          ((equal tecla 27)  ; ESC
           t)                      ; acaba recursió
          (t                 ; altrament
           (repeteix))))           ; repeteix

(defun repeteix-cli ()
    (pinta)
    (princ "Funcions: inc-angle o dec-angle.")
    (terpri)
    (princ "> ")
    (eval (read))
    (repeteix-cli))

(defun pinta ()
    (cls)
    (color 255 0 0)
    (rectangle 0 0 639 339)
    (color 0 0 0)
    (cercle 320 170 100 12)
    (color 255 0 255)
    (angle 320 170 100 (get 'programa 'angle))
    (color 0 0 0))

(defun angle (x y r angle)
    (move x y)
    (drawr (+ x (* r (cos (radians angle))))
           (+ y (* r (sin (radians angle))))))

(defun rectangle (x y w h)
    (move x y)
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h)))

(defun cercle (x y radi segments)
    (mover (+ x radi) y)
    (cercle2 x y radi (/ 360 segments) 0))

(defun cercle2 (x y radi pas angle)
  (cond ((< angle 360)
         (drawr (+ x (* radi (cos (radians (+ angle pas)))))
                (+ y (* radi (sin (radians (+ angle pas))))))
         (cercle2 x y radi pas (+ angle pas)))
        (t t)))

(defun mover (x y)
  "mou a les coordenades arrodonides"
  (move (round x) 
        (round y)))

(defun drawr (x y)
  "pinta a les coordenades arrodonides"
  (draw (round x) 
        (round y)))

(defun radians (graus)
  (/ (* graus (* 2 pi)) 360))
