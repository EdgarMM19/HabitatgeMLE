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
    (slot presupost (type FLOAT) (default 0.0))
)

; Preferencies del sol·licitant
(deftemplate MAIN::preferencies
)

(deftemplate MAIN::llista-recomanacions
    (multislot recomanacions (type INSTANCE))
)

(deftemplate MAIN::problema-abstracte
    (slot mida-habitatge (type SYMBOL) (allowed-values Petit Mitja Gran NA) (default NA))
    (slot presupost (type SYMBOL) (allowed-values Barat Mitja Car NA) (default NA))
)

;;*******************
;;*    DEFCLASSES   *
;;*******************

(defclass OfertaAbstracta (is-a USER) (role concrete)
	(slot oferta (type INSTANCE) (create-accessor read-write))
	(slot mida-habitatge (type SYMBOL) (allowed-values Petit Mitja Gran) (create-accessor read-write))
    (slot preu (type SYMBOL) (allowed-values Barat Mitja Car) (create-accessor read-write))
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
        (send ?ofertaAbstracta calcula-rang-preu)
	)
)

;;******************************
;;* DEFFUNCTIONS - PRESENTACIO *
;;******************************

(deffunction comparar-ofertes (?oferta1 ?oferta2)
    (< (send ?oferta1 get-puntuacio) (send ?oferta2 get-puntuacio))
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
    (presupost preguntar)
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

(defrule preguntes::preguntar-presupost
    (declare (salience 10))
    ?fet <- (presupost preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?presupost (preguntar-nombre "Quin presupost tens?" 0 5000))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (presupost ?presupost))
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
    (presupost abstreure)
)

(defrule abstraccio::abstreure-mida-habitatge
    ?fet <- (mida-habitatge abstreure)
    (restriccions (superficie-habitable-maxima ?superficie-habitable-maxima))
    (not (problema-abstracte))
    =>
    (if  (< ?superficie-habitable-maxima 70)
        then (assert (problema-abstracte (mida-habitatge Petit)))
            else (
                if (< ?superficie-habitable-maxima 150)
                    then (assert (problema-abstracte (mida-habitatge Mitja)))
                else
                    (assert (problema-abstracte (mida-habitatge Gran)))
            )
    )
    (retract ?fet)
)

(defrule abstraccio::abstreure-presupost
    ?fet <- (presupost abstreure)
    (restriccions (presupost ?presupost))
    ?e <- (problema-abstracte (presupost ?presupost-abs))
    (test (eq ?presupost-abs NA))
    =>
    (if  (< ?presupost 1000)
        then ( modify ?e (presupost Barat))
            else (
                if (< ?presupost 2000)
                    then (modify ?e (presupost Mitja))
                else (modify ?e (presupost Car))
            )
    )
    (retract ?fet)
)

(defrule abstraccio::passar-a-construccio
    (declare (salience -10))
    (not (mida-habitatge abstreure))
    (not (presupost abstreure))
    =>
    (printout t "Generant resultats..." crlf)
    (focus construccio)
)

;;**************************
;;*  MODUL DE CONSTRUCCIO  *
;;**************************

(defrule construccio::calcular-puntuacions
    (declare (salience 10))
    (problema-abstracte (mida-habitatge ?mida-habitatge))
    (problema-abstracte (presupost ?presupost))
	=>
	(bind ?llista-ofertes-abstractes (find-all-instances ((?inst OfertaAbstracta)) TRUE))
	(loop-for-count (?i 1 (length$ ?llista-ofertes-abstractes)) do
		(bind ?oferta-abstracta (nth$ ?i ?llista-ofertes-abstractes))
        (send ?oferta-abstracta calcula-puntuacio-mida-habitatge ?mida-habitatge)
		(send ?oferta-abstracta calcula-puntuacio-preu ?presupost)
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
    (bind ?llista-ordenada (sort comparar-ofertes ?llista-ofertes-abstractes))
    (loop-for-count (?i 1 (length$ ?llista-ordenada)) do
        (bind ?oferta-abstracta (nth$ ?i ?llista-ordenada))
        (if (> (send ?oferta-abstracta get-puntuacio) 0)
            then
        (printout t "Oferta amb puntuacio: " (send ?oferta-abstracta get-puntuacio) crlf (send ?oferta-abstracta get-justificacio-puntuacio) crlf)
        (send (send ?oferta-abstracta get-oferta) imprimir)
        )
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

	(if (eq ?mida-habitatge ?mida-habitatge-solicitant)
		then
			(bind ?puntuacio 5)
			(bind ?justificacio "La mida de l'habitatge s'ajusta amb la mida d'habitatge del sol·licitant")
	)
	(send ?self put-puntuacio (+ ?puntuacio (send ?self get-puntuacio)))
	(bind ?justificacio (str-cat "+" (str-cat "Mida Habitatge" (str-cat " --> " ?justificacio))))
	(slot-insert$ ?self justificacio-puntuacio (+ 1 (length$ ?self:justificacio-puntuacio)) ?justificacio)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-puntuacio-preu (?presupost-solicitant)

    (bind ?preu-habitatge (send ?self get-mida-habitatge))
    (bind ?puntuacio 0)
    (bind ?justificacio "No te cap bonificacio pel preu de l'habitatge")

    (if (eq ?presupost-solicitant Car)
        then (
            if (eq ?preu-habitatge Car)
            then
                (bind ?puntuacio 5)
                (bind ?justificacio "El presupost es adient")
            else
                (bind ?puntuacio 3)
                (bind ?justificacio "El preu es menor al presupost")
        )
    )
    (if (eq ?presupost-solicitant Mitja)
        then (if (eq ?preu-habitatge Mitja)
            then
                (bind ?puntuacio 5)
                (bind ?justificacio "El presupost es adient")
            else
                (if (eq ?preu-habitatge Barat)
                then
                    (bind ?puntuacio 3)
                    (bind ?justificacio "El preu es menor al presupost")
                )
        )
    )
    
    (if (eq ?presupost-solicitant Barat)
        then (if (eq ?preu-habitatge Barat)
            then
                (bind ?puntuacio 5)
                (bind ?justificacio "El presupost es adient")
        )
    )
    (send ?self put-puntuacio (+ ?puntuacio (send ?self get-puntuacio)))
    (bind ?justificacio (str-cat "+" (str-cat "Preu habitatge" (str-cat " --> " ?justificacio))))
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

(defmessage-handler MAIN::OfertaAbstracta calcula-rang-preu ()
    (bind ?preu (send ?self:oferta get-preu))
    (if  (< ?preu 1000)
        then (send ?self put-preu Barat)
            else (
                if (< ?preu 2000)
                    then (send ?self put-preu Mitja)
                else (send ?self put-preu Car)
            )
    )
)
;;***********************
;;*  HabitatgeHandlers  *
;;***********************

(defmessage-handler MAIN::Habitatge imprimir ()
    (printout t "Esta situat a " (send ?self get-latitud) " " (send ?self get-longitud) crlf)
    (printout t "Te " ?self:nombre_de_dormitoris " dormitors." crlf)
    (printout t "Te " ?self:nombre_de_banys " banys." crlf)
    (printout t "Te " ?self:superficie_habitable " m2." crlf)
)
;;********************
;;*  OfertaHandlers  *
;;********************

(defmessage-handler MAIN::Oferta imprimir ()
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
