;;;======================================================
;;;     Cal executar CLIPS des del directori del projecte
;;;     i carregar-lo:
;;;
;;;     (load "habitatges.pont")
;;;     (load "main.clp")
;;;     (reset)
;;;     (load-instances "habitatges.pins")
;;;     (run)
;;;======================================================

;;****************
;;*    MODULS    *
;;****************

(defmodule MAIN (export ?ALL))

(defmodule preguntes
    (import MAIN ?ALL)
    (export ?ALL)
)

(defmodule abstraccio
    (import MAIN ?ALL)
    (import preguntes ?ALL)
    (export ?ALL)
)

(defmodule construccio
    (import MAIN ?ALL)
    (export ?ALL)
)

(defmodule presentacio
    (import MAIN ?ALL)
    (export ?ALL)
)

;;**********************
;;*    DEFTEMPLATES    *
;;**********************

; Restriccions del sol·licitant
(deftemplate MAIN::restriccions
    (slot superficie-habitable-maxima (type FLOAT) (default 0.0))
)

; Preferencies del sol·licitant
(deftemplate MAIN::preferencies
)

(deftemplate MAIN::llista-recomanacions
    (multislot recomanacions (type INSTANCE))
)

(deftemplate MAIN::problema-abstracte
    (slot mida-habitatge (type SYMBOL) (allowed-values Petit Mitja Gran))
)

;;*******************
;;*    DEFCLASSES   *
;;*******************

(defclass OfertaAbstracta (is-a USER) (role concrete)
	(slot oferta (type INSTANCE) (create-accessor read-write))
	(slot mida-habitatge (type SYMBOL) (allowed-values Petit Mitja Gran) (create-accessor read-write))
	(slot puntuacio (type INTEGER) (create-accessor read-write) (default 0))
	(multislot justificacio-puntuacio (type STRING) (create-accessor read-write))
)

;;****************************
;;* DEFFUNCTIONS - PREGUNTES *
;;****************************

(deffunction preguntar (?pregunta $?valors-permesos)
    (progn$
        (?var ?valors-permesos)
        (lowcase ?var))
    (format t "%s (%s) " ?pregunta (implode$ ?valors-permesos))
    (bind ?resposta (read))
    (while (not (member (lowcase ?resposta) ?valors-permesos)) do
        (format t "%s (%s) " ?pregunta (implode$ ?valors-permesos))
        (bind ?resposta (read))
    )
    ?resposta
)

(deffunction preguntar-si-o-no (?pregunta)
   (bind ?resposta (preguntar ?pregunta si no s n))
   (if (or (eq ?resposta si) (eq ?resposta s))
       then TRUE
       else FALSE)
)

(deffunction preguntar-nombre (?pregunta ?cota-inferior ?cota-superior)
    (format t "%s [%d, %d] " ?pregunta ?cota-inferior ?cota-superior)
    (bind ?resposta (read))
    (while (not (and (>= ?resposta ?cota-inferior) (<= ?resposta ?cota-superior))) do
        (format t "%s [%d, %d] " ?pregunta ?cota-inferior ?cota-superior)
        (bind ?resposta (read))
    )
    ?resposta
)

;;*****************************
;;* DEFFUNCTIONS - ABSTRACCIO *
;;*****************************

(deffunction calcular-ofertes-abstractes ()
	(bind ?llista-ofertes (find-all-instances ((?inst Oferta)) TRUE))
	(loop-for-count (?i 1 (length$ ?llista-ofertes)) do
		(bind ?oferta (nth$ ?i ?llista-ofertes))
		(bind ?ofertaAbstracta (make-instance (sym-cat ofertaAbstracta- (gensym)) of OfertaAbstracta))
		(send ?ofertaAbstracta put-oferta ?oferta)
		(send ?ofertaAbstracta calcula-mida-habitatge)
	)
)

;;*****************
;;*      MAIN     *
;;*****************

(defrule MAIN::initial_rule "Missatge inicial"
    =>
    (calcular-ofertes-abstractes)
    (printout t crlf crlf)
    (printout t "-----------------------------------------------------------" crlf)
    (printout t "---------------Sistema expert d'habitatges ----------------" crlf)
    (printout t "-----------------------------------------------------------" crlf)
    (printout t crlf crlf)
    (focus preguntes)
)

;;************************
;;*  MODUL DE PREGUNTES  *
;;************************

(deffacts dades
    (restriccions)
    (superficie-habitable-maxima preguntar)
)

(defrule preguntes::preguntar-superficie-habitable-maxima 
    (declare (salience 10))
    ?fet <- (superficie-habitable-maxima preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?superficie-habitable-maxima (preguntar-nombre "Quina superficie habitable maxima vols (m2)?" 0 1000))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (superficie-habitable-maxima ?superficie-habitable-maxima))
)

(defrule preguntes::passar-a-seleccio "Passa al modul de seleccio"
    (declare (salience -10))
    (not (superficie-habitable-maxima preguntar))
    =>
    (printout t "Abstraient problema..." crlf)
    (focus abstraccio)
)

;;************************
;;*  MODUL D'ABSTRACCIO  *
;;************************

(deffacts abstraccio
    (mida-habitatge abstreure)
)

(defrule abstraccio::abstreure-mida-habitatge
    ?fet <- (mida-habitatge abstreure)
    (restriccions (superficie-habitable-maxima ?superficie-habitable-maxima))
    =>
    (if  (< ?superficie-habitable-maxima 70)
        then (assert (problema-abstracte (mida-habitatge Petit)))
            else (
                if (< ?superficie-habitable-maxima 150)
                    then (assert (problema-abstracte (mida-habitatge Mitja)))
                else (assert (problema-abstracte (mida-habitatge Gran)))
            )
    )
    (retract ?fet)
)

(defrule abstraccio::passar-a-construccio
    (declare (salience -10))
    (not (mida-habitatge abstreure))
    =>
    (printout t "Generant resultats..." crlf)
    (focus construccio)
)

;;**************************
;;*  MODUL DE CONSTRUCCIO  *
;;**************************

(defrule construccio::calcular-puntuacions
    (problema-abstracte (mida-habitatge ?mida-habitatge))
	=>
	(bind ?llista-ofertes-abstractes (find-all-instances ((?inst OfertaAbstracta)) TRUE))
	(loop-for-count (?i 1 (length$ ?llista-ofertes-abstractes)) do
		(bind ?oferta-abstracta (nth$ ?i ?llista-ofertes-abstractes))
		(send ?oferta-abstracta calcula-puntuacio-mida-habitatge ?mida-habitatge)
	)
)

(defrule construccio::passar-a-presentacio
    (declare (salience -10))
    =>
    (printout t "Presentant resultats..." crlf)
    (focus presentacio)
)

;;**************************
;;*  MODUL DE PRESENTACIO  *
;;**************************

(defrule presentacio::mostrar-recomanacions
    (not (final))
    (restriccions (superficie-habitable-maxima ?superficie-habitable-maxima))
    =>
    (printout t crlf)
    (printout t "Et recomano aquestes ofertes:" crlf)
    (bind ?llista-ofertes-abstractes (find-all-instances ((?inst OfertaAbstracta)) TRUE))
    (loop-for-count (?i 1 (length$ ?llista-ofertes-abstractes)) do
        (bind ?oferta-abstracta (nth$ ?i ?llista-ofertes-abstractes))
        (if (> (send ?oferta-abstracta get-puntuacio) 0)
            then
        (printout t " Oferta amb puntuacio: " (send ?oferta-abstracta get-puntuacio) crlf)
        )
        (send (send ?oferta-abstracta get-oferta) imprimir)
    )
    (assert (final))
)

;;**********************
;;*  MESSAGE HANDLERS  *
;;**********************

;;*****************************
;;*  OfertaAbstractaHandlers  *
;;*****************************

(defmessage-handler MAIN::OfertaAbstracta calcula-puntuacio-mida-habitatge (?mida-habitatge-solicitant)

	(bind ?mida-habitatge (send ?self get-mida-habitatge))
	(bind ?puntuacio 0)
	(bind ?justificacio "No te cap bonificacio per la mida de l'habitatge")

	(if (eq (send ?self get-mida-habitatge) ?mida-habitatge-solicitant)
		then
			(bind ?puntuacio 5)
			(bind ?justificacio "La mida de l'habitatge s'ajusta amb la mida d'habitatge del sol·licitant")
	)

	(send ?self put-puntuacio (+ ?puntuacio (send ?self get-puntuacio)))
	(bind ?justificacio (str-cat "+" (str-cat ?justificacio (str-cat " --> " ?justificacio))))
	(slot-insert$ ?self justificacio-puntuacio (+ 1 (length$ ?self:justificacio-puntuacio)) ?justificacio)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-mida-habitatge ()
    (bind ?hab_actual (send ?self:oferta get-ofereix_a))
    (bind ?superficie_habitable (send ?hab_actual get-superficie_habitable))
    (if  (< ?superficie_habitable 70)
        then (send ?self put-mida-habitatge Petit)
            else (
                if (< ?superficie_habitable 150)
                    then (send ?self put-mida-habitatge Mitja)
                else (send ?self put-mida-habitatge Gran)
            )
    )
)

;;***********************
;;*  HabitatgeHandlers  *
;;***********************

(defmessage-handler MAIN::Habitatge imprimir ()
    ;;;(printout t "Esta situat a " ?self::latitud " " ?self::longitud crlf)
    (printout t "Te " ?self:nombre_de_dormitoris " dormitors." crlf)
    (printout t "Te " ?self:nombre_de_banys " banys." crlf)
    (printout t "Te " ?self:superficie_habitable " m2." crlf)
)
;;********************
;;*  OfertaHandlers  *
;;********************

(defmessage-handler MAIN::Oferta imprimir ()
    (printout t "--- Habitatge  ---" crlf)
    (send ?self:ofereix_a imprimir)
    (printout t "Costa " ?self:numero_de_places_de_garatge " euros." crlf)
    (if (eq ?self:admet_mascotes TRUE)
    then
    (printout t "Admet mascotes." crlf)
    else
    (printout t "No admet mascotes." crlf)
    )
    (if (eq ?self:es_per_compartir TRUE)
    then
    (printout t "Es per compartir." crlf)
    else
    (printout t "No es per compartir." crlf)
    )
    (if (eq ?self:inclou_electrodomestics TRUE)
    then
    (printout t "Inclou electrodomestics." crlf)
    else
    (printout t "No inclou electrodomestics." crlf)
    )
    (if (eq ?self:inclou_mobles TRUE)
    then
    (printout t "Inclou mobles." crlf)
    else
    (printout t "No inclou mobles." crlf)
    )
    (printout t "Te " ?self:numero_de_places_de_garatge " places de garatge." crlf)
    (printout t "--------------------------" crlf)
)
