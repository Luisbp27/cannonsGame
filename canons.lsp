
(defun inicia()
    (set (get 'escenari 'amplada) 640)
    (set (get 'escenari 'altura) 340)

    ; Generar aleatòriament les dimensions del mur i els camps
    (let*(
        (amplada-mur (+ (random 21) 20))  ; amplada del mur entre 20 i 40 px
        (altura-mur (+ (random 51) 100))  ; altura del mur entre 100 i 150 px
        (amplada-total-camps (- 640 amplada-mur))
        (amplada-camp-esquerra (+ (floor amplada-total-camps 2) (- (random 41) 20)))
        (amplada-camp-dreta (- amplada-total-camps amplada-camp-esquerra))
    ))
    (set (get 'escenari 'amplada-mur) amplada-mur)
    (set (get 'escenari 'altura-mur) altura-mur)
    (set (get 'escenari 'amplada-camp-esquerra) amplada-camp-esquerra)
    (set (get 'escenari 'amplada-camp-dreta) amplada-camp-dreta)

    ; Inicialitzar canons
    (set (get 'cano1 'x) (+ (random (- amplada-camp-esquerra 95)) 95))
    (set (get 'cano1 'y) (+ (random 31) 15))
    (set (get 'cano1 'angle) 45)
    (set (get 'cano1 'velocitat) 20)

    (set (get 'cano2 'x) (+ amplada-camp-esquerra amplada-mur (random (- amplada-camp-dreta 95)) 95))
    (set (get 'cano2 'y) (+ (random 31) 15))
    (set (get 'cano2 'angle) 135)
    (set (get 'cano2 'velocitat) 20)

    (bucle)
)

(defun bucle()
    (let ((key (get-key)))  ; Obté la tecla premuda per l'usuari
        (cond
            ((equal key 'ESC) (return))  ; Si l'usuari prem ESC, sortir del bucle
            ((equal key 'A) (moure-cano 'cano1 'esquerra))  ; Moure canó esquerra a l'esquerra
            ((equal key 'D) (moure-cano 'cano1 'dreta))  ; Moure canó esquerra a la dreta
            ((equal key 'W) (puja-cano 'cano1 5))  ; Inclinar canó esquerra cap amunt
            ((equal key 'S) (baixa-cano 'cano1 5))  ; Inclinar canó esquerra cap avall
            ((equal key 'Q) (disminueix-cano-velocitat 'cano1 1))  ; Disminuir potència de dispar del canó esquerra
            ((equal key 'E) (augmenta-cano-velocitat 'cano1 1))  ; Augmentar potència de dispar del canó esquerra
            ((equal key 'F) (dispara 'cano1))  ; Disparar amb el canó esquerra
            ((equal key 'J) (moure-cano 'cano2 'esquerra))  ; Moure canó dreta a l'esquerra
            ((equal key 'L) (moure-cano 'cano2 'dreta))  ; Moure canó dreta a la dreta
            ((equal key 'I) (puja-cano 'cano2 5))  ; Inclinar canó dreta cap amunt
            ((equal key 'K) (baixa-cano 'cano2 5))  ; Inclinar canó dreta cap avall
            ((equal key 'O) (disminueix-cano-velocitat 'cano2 1))  ; Disminuir potència de dispar del canó dreta
            ((equal key 'U) (augmenta-cano-velocitat 'cano2 1))  ; Augmentar potència de dispar del canó dreta
            ((equal key 'H) (dispara 'cano2)) ; Disparar amb el canó dreta
        )
    )

    (pinta)  ; Redibuixa l'escenari i els canons amb les seves posicions actualitzades
    (bucle)  ; Tornar a executar el bucle
)

(defun rectangle (x y w h)
    (move x y)
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h)))

(defun pinta-cano (cano)
    (let ((x (get cano 'x)) ; Obtenir les coordenades x del canó
            (y (get cano 'y)) ; Obtenir les coordenades y del canó
            (angle (get cano 'angle)) ; Obtenir l'angle del canó
            (velocitat (get cano 'velocitat))) ; Obtenir la velocitat del canó
        (moure x y) ; Moure el cursor a la posició del canó
        ; Dibuixar el canó
        (drawrel (* 10 (cos angle)) (* 10 (sin angle)))
    )
)

(defun pinta()
    ; Esborra l'escenari
    (clear)

    ; Dibuixa el mur
    (rectangle 0 0 (get 'escenari 'amplada-mur) (get 'escenari 'altura-mur))

    ; Dibuixa els camps
    (rectangle (get 'escenari 'amplada-mur) 0 (get 'escenari 'amplada-camp-esquerra) (get 'escenari 'altura))
    (rectangle (+ (get 'escenari 'amplada-mur) (get 'escenari 'amplada-camp-esquerra)) 0 (get 'escenari 'amplada-camp-dreta) (get 'escenari 'altura))

    ; Dibuixa els canons
    (pinta-cano 'cano1)
    (pinta-cano 'cano2)

    ; Actualitza la finestra gràfica
    (refresh)
)

