;; Pràctca LISP
;; Assignatura: Llenguatges de Programació
;; Grau en Enginyeria Informàtica - Universitat de le Illes Balears
;; Autors: Lluis Barca Pons i Eduardo Bonnín Narváez

;; FUNCIONS PRINCIPALS ;;

(defun inicia()
    "Inicialitza l'escenari, els canons i inicia el bucle principal del joc"
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
    (putprop 'cano1 10 'resistencia)

    ; Inicialitzar cano 2
    (putprop 'cano2 (+ (get 'escenari 'amplada-camp-esquerra) (get 'escenari 'amplada-mur) (random (- (get 'escenari 'amplada-camp-dreta) 95)) 95) 'x)
    (putprop 'cano2 (get 'escenari 'altura-camp-dreta) 'y)
    (putprop 'cano2 135 'angle)
    (putprop 'cano2 20 'velocitat)
    (putprop 'cano2 10 'resistencia)

    (pinta)
    (bucle)
)

(defun bucle()
    "Bucle principal del joc"
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
    "Dibuixa l'escenari, els canons i els projectils"
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

    ; Dibuixa la resistència dels canons
    (dibuixar-resistencia 'cano1)
    (dibuixar-resistencia 'cano2)
)

;; FUNCIONS PRINCIPALS DE DIBUIX ;;

(defun pinta-cano (cano velocitat)
    "Dibuixa el canó i la línia que indica la direcció del tir"
    (let ((x (get cano 'x))
          (y (get cano 'y))
          (angle (get cano 'angle)))
        (rectangle (- x 20) y 20 10) ; Dibuixar el canó
        (angle (- x 10) (+ y 10) velocitat angle) ; Dibuixar la línia de direcció del tir
    )
)

(defun dibuixar-resistencia (cano)
    "Dibuixa la resistència del canó"
    (let ((resistencia (get cano 'resistencia)))
        (cond
            ((equal cano 'cano1)
                (dibuixar-cors 20 (- (get 'escenari 'altura) 20) 5 10 resistencia)
            )

            ((equal cano 'cano2)
                (dibuixar-cors (- (get 'escenari 'amplada) 110) (- (get 'escenari 'altura) 20) 5 10 resistencia)
            )
        )
    )
)

(defun dibuixar-cors (x y radius spacing resistencia)
    "Funció recursiva per a dibuixar es cors segons la resistencia."
    (when (> resistencia 0)
        (color 255 0 0)  ; Color vermell
        (cercle x y radius spacing)  ; Dibuixar un cor
        (dibuixar-cors (+ x spacing) y radius spacing (1- resistencia))
    )
)


(defun moure-cano (cano direccio)
    "Mou el cano a l'esquerra o a la dreta, depenent de la direcció indicada"
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

(defun disminueix-cano-velocitat (cano valor)
    "Disminueix la velocitat del cano en la quantitat indicada, sempre que no sigui inferior a 10"
    (let ((velocitat (get cano 'velocitat)))
        (cond
            ((> velocitat 10) (putprop cano (- velocitat valor) 'velocitat))
            ((= velocitat 10) (print "La velocitat no pot ser inferior a 10"))
        )
    )

    (pinta)
)

(defun augmenta-cano-velocitat (cano valor)
    "Augmenta la velocitat del cano en la quantitat indicada, sempre que no sigui superior a 80"
    (let ((velocitat (get cano 'velocitat)))
        (cond
            ((< velocitat 80) (putprop cano (+ velocitat valor) 'velocitat))
            ((= velocitat 80) (print "La velocitat no pot ser superior a 80"))
        )
    )

    (pinta)
)

(defun puja-cano (cano valor)
    "Puja el cano en la quantitat indicada, depenent del cano indicat"
    (let ((angle (get cano 'angle)))
        (cond
            ((equal cano 'cano1) (putprop cano (+ angle valor) 'angle))
            ((equal cano 'cano2) (putprop cano (- angle valor) 'angle))
        )
    )

    (pinta)
)

(defun baixa-cano (cano valor)
    "Baixa el cano en la quantitat indicada, depenent del cano indicat"
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

(defun disminueix-resistencia (cano)
    "Disminueix la resistència del canó en 1"
    (let ((resistencia (get cano 'resistencia)))
        (cond
            ((> resistencia 1) (putprop cano (- resistencia 1) 'resistencia))
            ((= resistencia 1) (print "El canó ja no té més resistència"))
        )
    )
)

(defun dispara (cano)
    "Dispara un projectil amb les característiques del cano indicat"
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

    (pinta)
)

(defun dispara-recursiu (x y vx vy dt g cano target mur-x mur-amplada mur-altura)
    "Simula el moviment del projectil fins a la col·lisió amb un objectiu o una paret"
    (when (zerop (get 'escenari 'explosio)) ; Comprova si s'ha produït una explosió
        (let*
            ((new-x (+ x (* vx dt))) ; Nova posició X
            (new-y (+ y (* vy dt) (* 0.5 g (expt dt 2)))) ; Nova posició Y
            (new-vy (+ vy (* g dt))) ; Nova velocitat Y
            (distancia-obj (calcular_distancia new-x new-y (get target 'x) (get target 'y)))) ; Distància entre el projectil i l'objectiu

            ; Dibuixa el projectil
            (color 0 0 0)
            (cercle new-x new-y 2 10)

            ; Espera breu
            (sleep 0.015)

            ; Evaluem col·lisions
            (cond
                ((or (>= new-y (get 'escenari 'altura)) ; Col·lisió amb les parets o el sostre
                    (>= new-x (get 'escenari 'amplada))
                    (<= new-x 0))

                    (explosio new-x new-y)
                    ; (print "Col·lisió amb les parets o el sostre")
                    ; (sleep 1)
                    (putprop 'escenari 1 'explosio))

                ((or (<= new-y (get 'escenari 'altura-camp-esquerra)) ; Col·lisió amb el camp esquerra o dreta
                    (<= new-y (get 'escenari 'altura-camp-dreta)))

                    (explosio new-x new-y)
                    ; (print "Col·lisió amb el camp")
                    ; (sleep 1)
                    (putprop 'escenari 1 'explosio)
                )

                ((and (>= new-x mur-x) (<= new-x (+ mur-x mur-amplada)) (<= new-y mur-altura)) ; Col·lisió amb el mur
                    (explosio new-x new-y)
                    ; (print "Col·lisió amb el mur")
                    ; (sleep 1)
                    (putprop 'escenari 1 'explosio))

                ((< distancia-obj 20) ; Col·lisió amb l'objectiu
                    (explosio new-x new-y)
                    (disminueix-resistencia target)
                    ; (print "Col·lisió amb l'objectiu")
                    ; (sleep 1)
                    (putprop 'escenari 1 'explosio))

                (t  ; Si no hi ha col·lisions, següeix amb la següent iteració la simulación
                    (dispara-recursiu new-x new-y vx new-vy dt g cano target mur-x mur-amplada mur-altura))
            )
        )
    )
)

;; FUNCIONS AUXILIARS ;;

(defun calcular_distancia (x1 y1 x2 y2)
    "Calcula la distància entre dos punts"
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))) ; Teorema de Pitàgores on h = sqrt(a^2 + b^2)
)

(defun explosio(x y)
    "Dibuixa una explosió a les coordenades indicades"
    (color 255 0 0) ; Color vermell
    (cercle x y 5 10)
    (sleep 1)
)

(defun rectangle (x y w h)
    "Dibuixa un rectangle a les coordenades indicades"
    (move x y)
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h))
)

(defun angle (x y r angle)
    "Dibuixa una línia a partir de les coordenades actuals amb un angle i una longitud determinats"
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
    (move (round x) (round y))
)

(defun cercle (x y radi segments)
    "Dibuixa un cercle a les coordenades indicades"
    (moure (+ x radi) y)
    (cercle2 x y radi (/ 360 segments) 0)
)

(defun cercle2 (x y radi pas angle)
    "Dibuixa un cercle a les coordenades indicades"
    (cond ((< angle 360)
        (drawr (+ x (* radi (cos (radians (+ angle pas)))))
                (+ y (* radi (sin (radians (+ angle pas))))))
        (cercle2 x y radi pas (+ angle pas)))
        (t t)
    )
)

(defun radians (graus)
    "Converteix graus a radians"
    (/ (* graus (* 2 pi)) 360)
)

(defun sleep (seconds)
    "Espera la quantitat indicada de segons"
    (do ((endtime (+ (get-internal-real-time)
                    (* seconds internal-time-units-per-second))))
        ((> (get-internal-real-time) endtime))
    )
)