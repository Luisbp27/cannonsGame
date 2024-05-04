
;; FUNCIONS PRINCIPALS ;;

(defun inicia()
    ; Inicialitzar l'escenari
    (putprop 'escenari 640 'amplada)
    (putprop 'escenari 340 'altura)

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
            ((equal key 113) (disminueix-cano-velocitat 'cano1 1))  ; q: Disminuir velocitat del canó esquerra
            ((equal key 101) (augmenta-cano-velocitat 'cano1 1))  ; e: Augmentar velocitat del canó esquerra
            ((equal key 102) (dispara 'cano1))  ; f: Disparar amb el canó esquerra

            ((equal key 106) (moure-cano 'cano2 'esquerra))  ; j: Moure canó dreta a l'esquerra
            ((equal key 108) (moure-cano 'cano2 'dreta))  ; l: Moure canó dreta a la dreta
            ((equal key 105) (puja-cano 'cano2 5))  ; i: Inclinar canó dreta cap amunt
            ((equal key 107) (baixa-cano 'cano2 5))  ; k: Inclinar canó dreta cap avall
            ((equal key 111) (disminueix-cano-velocitat 'cano2 1))  ; o: Disminuir potència de dispar del canó dreta
            ((equal key 117) (augmenta-cano-velocitat 'cano2 1))  ; u: Augmentar potència de dispar del canó dreta
            ((equal key 104) (dispara 'cano2)) ; h: Disparar amb el canó dreta
        )
    )

    (bucle)  ; Tornar a executar el bucle
)

(defun pinta()
    ; Esborra l'escenari
    (cls)
    (moure 0 0)

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
    (pinta-cano 'cano1)
    (color 0 0 0)
    (pinta-cano 'cano2)
)

;; FUNCIONS AUXILIARS DE DIBUIX ;;

(defun rectangle (x y w h)
    (move x y)
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h))
)

(defun pinta-cano (cano)
    ; Dibuixa un rectangle que representa el canó i una línia damunt d'aquest que indica la direcció del tir
    (let ((x (get cano 'x))
          (y (get cano 'y))
          (angle (get cano 'angle)))
        (rectangle (- x 20) y 20 10)
        (moure x y)
        ;(drawrel (* 20 (cos angle)) (* 20 (sin angle)))
    )
)

; Funció que mou el cano depenent de la direcció indicada
(defun moure-cano (cano direccio)
    (let ((x (get cano 'x))
          (y (get cano 'y))
          (angle (get cano 'angle))
          (velocitat (get cano 'velocitat)))
        (cond
            ((equal direccio 'esquerra) (putprop cano (- x velocitat) 'x))
            ((equal direccio 'dreta) (putprop cano (+ x velocitat) 'x))
        )
    )
    (pinta)
)

; Funció que disminueix la velocitat del cano
(defun disminueix-cano-velocitat (cano valor)
    (let ((velocitat (get cano 'velocitat)))
        (putprop cano (- velocitat valor) 'velocitat)
    )
    (pinta)
)

; Funció que augmenta la velocitat del cano
(defun augmenta-cano-velocitat (cano valor)
    (let ((velocitat (get cano 'velocitat)))
        (putprop cano (+ velocitat valor) 'velocitat)
    )
    (pinta)
)

;; FUNCIONS AUXILIARS ;;

(defun moure (x y)
    "mou a les coordenades arrodonides"
    (move (round x) (round y))
)

(defun radians (graus)
  (/ (* graus (* 2 pi)) 360))