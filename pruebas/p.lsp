;; FUNCIONS DE DIBUIX ;;



; Funció que puja el cano
(defun puja-cano (cano graus)
    (let ((angle (get cano 'angle)))
        (putprop cano (+ angle graus) 'angle)
    )
    (pinta)
)

; Funció que baixa el cano
(defun baixa-cano (cano graus)
    (let ((angle (get cano 'angle)))
        (putprop cano (- angle graus) 'angle)
    )
    (pinta)
)



; Funció que dispara el cano
(defun dispara (cano)
    (let ((x (get cano 'x))
          (y (get cano 'y))
          (angle (get cano 'angle))
          (velocitat (get cano 'velocitat)))
        (let ((x2 (+ x (* 10 (cos (to-radians angle)))))
              (y2 (+ y (* 10 (sin (to-radians angle))))))
            (move x y)
            (draw x2 y2)
        )
    )
    (pinta)
)



;; FUNCIONS DE AUXILIARS ;;

(defun to-radians (graus)
    (* graus (/ 3.141592 180))
)