
;; FUNCIONS BÀSIQUES ;;

(defun inicia()
    ; Inicialitzar l'escenari
    (putprop 'escenari 640 'amplada)
    (putprop 'escenari 340 'altura)

    ; Inicialitzar el mur
    (putprop 'escenari (+ (random 21) 20) 'amplada-mur)
    (putprop 'escenari (+ (random 51) 100) 'altura-mur)

    ; Inicialitzar els camps
    (putprop 'escenari (- 640 amplada-mur) 'amplada-total-camps)
    (putprop 'escenari (+ (+ 0 (floor amplada-total-camps 2)) (- (random 41) 20)) 'amplada-camp-esquerra)
    (putprop 'escenari (- amplada-total-camps amplada-camp-esquerra) 'amplada-camp-dreta)

    ; Inicialitzar cano 1
    (putprop 'cano1 (+ (random (- amplada-camp-esquerra 95)) 95) 'x)
    (putprop 'cano1 (+ (random 31) 15) 'y)
    (putprop 'cano1 45 'angle)
    (putprop 'cano1 20 'velocitat)

    ; Inicialitzar cano 2
    (putprop 'cano2 (+ amplada-camp-esquerra amplada-mur (random (- amplada-camp-dreta 95)) 95) 'x)
    (putprop 'cano2 (+ (random 31) 15) 'y)
    (putprop 'cano2 135 'angle)
    (putprop 'cano2 20 'velocitat)

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

(defun pinta()
    ; Esborra l'escenari
    (clear)

    ; Dibuixa el mur
    (rectangle 0 0 (get 'escenari 'amplada-mur) (get 'escenari 'let_-mur))

    ; Dibuixa els camps
    (rectangle (get 'escenari 'amplada-mur) 0 (get 'escenari 'amplada-camp-esquerra) (get 'escenari 'altura))
    (rectangle (+ (get 'escenari 'amplada-mur) (get 'escenari 'amplada-camp-esquerra)) 0 (get 'escenari 'amplada-camp-dreta) (get 'escenari 'altura))

    ; Dibuixa els canons
    (pinta-cano 'cano1)
    (pinta-cano 'cano2)

    ; Actualitza la finestra gràfica
    (refresh)
)

;; FUNCIONS AUXILIARS ;;

(defun rectangle (x y w h)
    (move x y)
    (drawrel w 0)
    (drawrel 0 h)
    (drawrel (- w) 0)
    (drawrel 0 (- h))
)

