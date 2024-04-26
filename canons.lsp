
(defun inicia()
    (setf (get 'escenari 'amplada) 640)
    (setf (get 'escenari 'altura) 340)

    ;; Generar aleatòriament les dimensions del mur i els camps
    (let* ((amplada-mur (random 21) + 20)  ;; amplada del mur entre 20 i 40 px
            (altura-mur (random 51) + 100)  ;; altura del mur entre 100 i 150 px
            (amplada-total-camps (- 640 amplada-mur))
            (amplada-camp-esquerra (+ (/ amplada-total-camps 2) (random 41) - 20))
            (amplada-camp-dreta (- amplada-total-camps amplada-camp-esquerra)))
        (setf (get 'escenari 'amplada-mur) amplada-mur)
        (setf (get 'escenari 'altura-mur) altura-mur)
        (setf (get 'escenari 'amplada-camp-esquerra) amplada-camp-esquerra)
        (setf (get 'escenari 'amplada-camp-dreta) amplada-camp-dreta)
    )

    ;; Inicialitzar canons
    (setf (get 'cano1 'x) (+ (random (- amplada-camp-esquerra 95)) 95))
    (setf (get 'cano1 'y) (random 31) + 15)
    (setf (get 'cano1 'angle) 45)
    (setf (get 'cano1 'velocitat) 20)

    (setf (get 'cano2 'x) (+ amplada-camp-esquerra amplada-mur (random (- amplada-camp-dreta 95)) 95))
    (setf (get 'cano2 'y) (random 31) + 15)
    (setf (get 'cano2 'angle) 135)
    (setf (get 'cano2 'velocitat) 20)

    ;; Bucle principal del joc
    (bucle)
)

(defun bucle()
    (let ((key (get-key)))  ; Obté la tecla premuda per l'usuari
        (cond
            ((equal key 'ESC) (return))  ; Si l'usuari prem ESC, sortir del bucle
            ((equal key 'A) (mover-canon 'cano1 'esquerra))  ; Moure canó esquerra a l'esquerra
            ((equal key 'D) (mover-canon 'cano1 'dreta))  ; Moure canó esquerra a la dreta
            ((equal key 'W) (puja-cano 'cano1 5))  ; Inclinar canó esquerra cap amunt
            ((equal key 'S) (baixa-cano 'cano1 5))  ; Inclinar canó esquerra cap avall
            ((equal key 'Q) (disminueix-cano-velocitat 'cano1 1))  ; Disminuir potència de dispar del canó esquerra
            ((equal key 'E) (augmenta-cano-velocitat 'cano1 1))  ; Augmentar potència de dispar del canó esquerra
            ((equal key 'F) (dispara 'cano1))  ; Disparar amb el canó esquerra
            ((equal key 'J) (mover-canon 'cano2 'esquerra))  ; Moure canó dreta a l'esquerra
            ((equal key 'L) (mover-canon 'cano2 'dreta))  ; Moure canó dreta a la dreta
            ((equal key 'I) (puja-cano 'cano2 5))  ; Inclinar canó dreta cap amunt
            ((equal key 'K) (baixa-cano 'cano2 5))  ; Inclinar canó dreta cap avall
            ((equal key 'O) (disminueix-cano-velocitat 'cano2 1))  ; Disminuir potència de dispar del canó dreta
            ((equal key 'U) (augmenta-cano-velocitat 'cano2 1))  ; Augmentar potència de dispar del canó dreta
            ((equal key 'H) (dispara 'cano2))
        )
    )  ; Disparar amb el canó dreta
    (pinta)  ; Redibuixa l'escenari i els canons amb les seves posicions actualitzades
    (bucle)  ; Tornar a executar el bucle
)

(defun pinta()

)

(defun puja_cano_graus()

)

(defun baixa_cano_graus

)

(defun augmenta_cano_velocitat()

)

(defun disminueix_cano_velocitat()

)

(defun dispara_cano()

)

