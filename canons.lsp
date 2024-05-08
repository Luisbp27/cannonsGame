;; Pràctca LISP
;; Assignatura: Llenguatges de Programació
;; Grau en Enginyeria Informàtica - Universitat de le Illes Balears
;; Autors: Lluis Barca Pons i Eduardo Bonnín Narváez

;; FUNCIONS PRINCIPALS ;;

(defun inicia()
    ; Inicialitzar l'escenari
    (putprop 'escenari 640 'amplada)
    (putprop 'escenari 340 'altura)
    (putprop 'escenari 0 'explosio)

    ; Inicialitzar el mur
    (putprop 'escenari (+ (random 21) 20) 'amplada-mur)
    (putprop 'escenari (+ (random 51) 100) 'altura-mur)

    ; Inicialitzar els camps
    (let ((amplada-total-camps (- (get 'escenari 'amplada) (get 'escenari 'amplada-mur))))
        (putprop 'escenari (+ (+ 0 (floor amplada-total-camps 2)) (- (random 41) 20)) 'amplada-camp-esquerra)
        (putprop 'escenari (+ (random 31) 15) 'altura-camp-esquerra)
        (putprop 'escenari (- amplada-total-camps (get 'escenari 'amplada-camp-esquerra)) 'amplada-camp-dreta)
        (putprop 'escenari (+ (random 31) 15) 'altura-camp-dreta)
    )

    ; Inicialitzar la bandera
    (putprop 'escenari 20 'altura-bandera)

    ; Inicialitzar cano 1
    (putprop 'cano1 (+ (random (- (get 'escenari 'amplada-camp-esquerra) 95)) 95) 'x)
    (putprop 'cano1 (get 'escenari 'altura-camp-esquerra) 'y)
    (putprop 'cano1 45 'angle)
    (putprop 'cano1 20 'velocitat)

    ; Inicialitzar cano 2
    (putprop 'cano2 (+ (get 'escenari 'amplada-camp-esquerra) (get 'escenari 'amplada-mur) (random (- (get 'escenari 'amplada-camp-dreta) 95)) 95) 'x)
    (putprop 'cano2 (get 'escenari 'altura-camp-dreta) 'y)
    (putprop 'cano2 135 'angle)
    (putprop 'cano2 20 'velocitat)

    (pinta)
    (bucle)
)

(defun bucle()
    (let ((key (get-key)))  ; Obté la tecla premuda per l'usuari
        (cond
            ((equal key 27) (return))  ; Si l'usuari prem ESC, sortir del bucle

            ((equal key 97) (moure-cano 'cano1 'esquerra))  ; a: Moure canó esquerra a l'esquerra
            ((equal key 100) (moure-cano 'cano1 'dreta))  ; d: Moure canó esquerra a la dreta
            ((equal key 119) (puja-cano 'cano1 5))  ; w: Inclinar canó esquerra cap amunt
            ((equal key 115) (baixa-cano 'cano1 5))  ; s: Inclinar canó esquerra cap avall
            ((equal key 113) (disminueix-cano-velocitat 'cano1 10))  ; q: Disminuir velocitat del canó esquerra
            ((equal key 101) (augmenta-cano-velocitat 'cano1 10))  ; e: Augmentar velocitat del canó esquerra
            ((equal key 102) (dispara 'cano1))  ; f: Disparar amb el canó esquerra

            ((equal key 106) (moure-cano 'cano2 'esquerra))  ; j: Moure canó dreta a l'esquerra
            ((equal key 108) (moure-cano 'cano2 'dreta))  ; l: Moure canó dreta a la dreta
            ((equal key 105) (puja-cano 'cano2 5))  ; i: Inclinar canó dreta cap amunt
            ((equal key 107) (baixa-cano 'cano2 5))  ; k: Inclinar canó dreta cap avall
            ((equal key 111) (disminueix-cano-velocitat 'cano2 10))  ; o: Disminuir potència de dispar del canó dreta
            ((equal key 117) (augmenta-cano-velocitat 'cano2 10))  ; u: Augmentar potència de dispar del canó dreta
            ((equal key 104) (dispara 'cano2)) ; h: Disparar amb el canó dreta
        )
    )

    (bucle)  ; Tornar a executar el bucle
)

(defun pinta()
    ; Esborra l'exposio
    (putprop 'escenari 0 'explosio)

    ; Esborra l'escenari
    (cls)
    (move 0 0)

    ; Dibuixa l'escenari
    (color 0 0 0)
    (rectangle 0 0 (get 'escenari 'amplada) (get 'escenari 'altura))

    ; Dibuixa el mur
    (color 0 0 0)
    (rectangle (get 'escenari 'amplada-camp-esquerra) 0 (get 'escenari 'amplada-mur) (get 'escenari 'altura-mur))

    ; Dibuixa la bandera
    (color 0 0 0)
    (rectangle (+ (get 'escenari 'amplada-camp-esquerra) (floor (get 'escenari 'amplada-mur) 2)) (get 'escenari 'altura-mur) 0 (get 'escenari 'altura-bandera))

    ; Dibuixa els camps
    (rectangle 0 0 (get 'escenari 'amplada-camp-esquerra) (get 'escenari 'altura-camp-esquerra)) ; Camp esquerra
    (rectangle (+ (get 'escenari 'amplada-mur) (get 'escenari 'amplada-camp-esquerra)) 0 (get 'escenari 'amplada-camp-dreta) (get 'escenari 'altura-camp-dreta)) ; Camp dreta

    ; Dibuixa els canons
    (color 0 0 0)
    (pinta-cano 'cano1 (get 'cano1 'velocitat))
    (color 0 0 0)
    (pinta-cano 'cano2 (get 'cano2 'velocitat))
)

;; FUNCIONS PRINCIPALS DE DIBUIX ;;

; Dibuixa un rectangle que representa el canó i una línia damunt d'aquest que indica la direcció del tir
(defun pinta-cano (cano velocitat)
    (let ((x (get cano 'x))
          (y (get cano 'y))
          (angle (get cano 'angle)))
        (rectangle (- x 20) y 20 10)
        (angle (- x 10) (+ y 10) velocitat angle)
    )
)

; Funció que mou el cano depenent de la direcció indicada
(defun moure-cano (cano direccio)
    (let* (
        (x (get cano 'x))
        (y (get cano 'y))
        (angle (get cano 'angle))
        (limit-esquerra-cano1 21)
        (limit-dreta-cano1 (- (get 'escenari 'amplada-camp-esquerra) 1))
        (limit-esquerra-cano2 (+ (+ (get 'escenari 'amplada-camp-esquerra) (get 'escenari 'amplada-mur)) 21))
        (limit-dreta-cano2 (- (get 'escenari 'amplada) 1)))

        ; Depenent del cano
        (cond
            ((equal cano 'cano1)
                (cond
                    ; Movem esquerra o dreta, tenint en compte els límits
                    ((equal direccio 'esquerra) (if (> x limit-esquerra-cano1) (putprop cano (- x 5) 'x)))
                    ((equal direccio 'dreta) (if (< x limit-dreta-cano1) (putprop cano (+ x 5) 'x)))
                )
            )

            ((equal cano 'cano2)
                (cond
                    ; Movem esquerra o dreta, tenint en compte els límits
                    ((equal direccio 'esquerra) (if (> x limit-esquerra-cano2) (putprop cano (- x 5) 'x)))
                    ((equal direccio 'dreta) (if (< x limit-dreta-cano2) (putprop cano (+ x 5) 'x)))
                )
            )
        )
    )

    (pinta)
)

; Funció que disminueix la velocitat del cano
(defun disminueix-cano-velocitat (cano valor)
    (let ((velocitat (get cano 'velocitat)))
        (cond
            ((> velocitat 10) (putprop cano (- velocitat valor) 'velocitat))
            ((= velocitat 10) (print "La velocitat no pot ser inferior a 5"))
        )
    )

    (pinta)
)

; Funció que augmenta la velocitat del cano
(defun augmenta-cano-velocitat (cano valor)
    (let ((velocitat (get cano 'velocitat)))
        (cond
            ((< velocitat 80) (putprop cano (+ velocitat valor) 'velocitat))
            ((= velocitat 80) (print "La velocitat no pot ser superior a 80"))
        )
    )

    (pinta)
)

; Funció que puja el cano
(defun puja-cano (cano valor)
    (let ((angle (get cano 'angle)))
        (cond
            ((equal cano 'cano1) (putprop cano (+ angle valor) 'angle))
            ((equal cano 'cano2) (putprop cano (- angle valor) 'angle))
        )
    )
    (pinta)
)

; Funció que baixa el cano
(defun baixa-cano (cano valor)
    (let ((angle (get cano 'angle)))
        (if (equal cano 'cano1)
            (putprop cano (- angle valor) 'angle)
        )
        (if (equal cano 'cano2)
            (putprop cano (+ angle valor) 'angle)
        )
    )
    (pinta)
)

; Funció que dibuixa un projectil a les coordenades indicades
(defun dispara (cano)
    (let* ( (g -9.8)  ; Acceleració de la gravetat en px/s^2
            (dt 0.25) ; Interval de temps en segons per a la simulació
            (vi (get cano 'velocitat)) ; Velocitat inicial
            (angle (get cano 'angle))  ; Angle de dispar
            (x0 (- (get cano 'x) 10))          ; Posició inicial X del canó
            (y0 (+ (get cano 'y) 10))          ; Posició inicial Y del canó
            (vx (* vi (cos (radians angle))))  ; Component X de la velocitat
            (vy (* vi (sin (radians angle))))  ; Component Y de la velocitat
            (target (if (equal cano 'cano1) 'cano2 'cano1)) ; Determinar l'objectiu
            (mur-x (get 'escenari 'amplada-camp-esquerra))
            (mur-amplada (get 'escenari 'amplada-mur))
            (mur-altura (get 'escenari 'altura-mur)))
        (dispara-recursiu x0 y0 vx vy dt g cano target mur-x mur-amplada mur-altura)
    )

    (print "Explosió!")
    (pinta)
)

; Funció recursiva que simula el moviment del projectil
(defun dispara-recursiu (x y vx vy dt g cano target mur-x mur-amplada mur-altura)
    (when (zerop (get 'escenari 'explosio)) ; Comprova si s'ha produït una explosió
        (when (and (> y 0) (> x 0) (< x (get 'escenari 'amplada)))  ; Continua mentre estigui dins de l'àrea
            ; Actualitza les propietats del projectil
            (putprop cano (+ x (* vx dt)) 'x0) ; x = x + vx * dt
            (putprop cano (+ y (* vy dt) (* 0.5 g (* dt dt))) 'y0) ; y = y + vy * dt + 0.5 * g * dt^2
            (putprop cano (+ vy (* g dt)) 'vy) ; vy = vy + g * dt

            ; Dibuixa el projectil
            (color 0 0 0)
            (cercle x y 0.5 20)

            ; Espera un temps per a la següent iteració
            (sleep 0.015)

            ; Comprova si hi ha col·lisió amb el terra d'un camp o l'altra
            (when (or
                    ; Camp esquerra
                    (and (>= x (- (get 'escenari 'amplada) (get 'escenari 'amplada-camp-dreta))) (<= y (get 'escenari 'altura-camp-dreta)))
                    ; Camp dreta
                    (or (<= y (get 'escenari 'altura-camp-esquerra)) (<= y (get 'escenari 'altura-camp-dreta)))
                )
                (print "Col·lisió amb el terra dreta")
                (explosio x y)
                (putprop 'escenari 1 'explosio)
            )

            ; Comprova si hi ha col·lisió amb el mur
            (when (and (>= x mur-x) (<= x (+ mur-x mur-amplada)) (<= y mur-altura))
                (print "Col·lisió amb el mur")
                (explosio x y)
                (putprop 'escenari 1 'explosio)
            )

            ;(print "hola")

            (let ((distancia-obj (calcular_distancia x y (get target 'x) (get target 'y))))
                ; Comprova si hi ha col·lisió amb l'objectiu
                (when (< distancia-obj 15)
                    (print distancia-obj)
                    (print "Col·lisió amb l'objectiu")
                    (explosio x y)
                    (putprop 'escenari 1 'explosio)
                )
            )

            ; Si no hi ha explosió, segueix amb la següent iteració (ho comprovam per si s'ha produït una explosió, no continuar amb la simulació)
            (when (zerop (get 'escenari 'explosio))
                (dispara-recursiu (get cano 'x0) (get cano 'y0) vx (get cano 'vy) dt g cano target mur-x mur-amplada mur-altura)
            )
        )
    )
)

;; FUNCIONS AUXILIARS ;;

(defun calcular_distancia (x1 y1 x2 y2)
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))
)

(defun explosio(x y)
    (color 255 0 0)
    (cercle x y 5 10)
    (sleep 5)
)

(defun rectangle (x y w h)
    (move x y)
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h))
)

; Dibuixa una línia a partir de les coordenades actuals
(defun angle (x y r angle)
    (move x y)
    (drawr (+ x (* r (cos (radians angle))))
           (+ y (* r (sin (radians angle)))))
)

(defun drawr (x y)
    "pinta a les coordenades arrodonides"
    (draw (round x) (round y))
)

(defun moure (x y)
  "mou a les coordenades arrodonides"
  (move (round x)
        (round y)))

(defun cercle (x y radi segments)
    (moure (+ x radi) y)
    (cercle2 x y radi (/ 360 segments) 0))

(defun cercle2 (x y radi pas angle)
  (cond ((< angle 360)
         (drawr (+ x (* radi (cos (radians (+ angle pas)))))
                (+ y (* radi (sin (radians (+ angle pas))))))
         (cercle2 x y radi pas (+ angle pas)))
        (t t)
    )
)

(defun radians (graus)
  (/ (* graus (* 2 pi)) 360))

(defun sleep (seconds)
"Espera la quantitat indicada de segons"
    (do ((endtime (+ (get-internal-real-time)
                    (* seconds internal-time-units-per-second))))
        ((> (get-internal-real-time) endtime))
    )
)