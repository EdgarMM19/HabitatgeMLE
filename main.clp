;;;======================================================
;;;     Cal executar CLIPS des del directori del projecte
;;;     i carregar-lo:
;;;
;;;     (load "instancies.clp")
;;;     (load "main.clp")
;;;     (reset)
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

(defmodule construccio-abstracta
    (import MAIN ?ALL)
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
    (slot superficie-habitable-minima (type FLOAT) (default 0.0))
    (slot presupost (type FLOAT) (default 0.0))
    (slot presupost-minim (type FLOAT) (default 0.0))
    (slot mobilitat-reduida (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
    (slot nombre-habitants (type INTEGER) (range 1 10) (default 1))
    (slot nombre-parelles (type INTEGER) (range 0 5) (default 0))
    (slot menors (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
    (slot edat (type INTEGER) (default 40))
)

; Preferencies del sol·licitant
(deftemplate MAIN::preferencies
    (slot places-garatge (type INTEGER) (default 0))
    (slot jardi (type SYMBOL) (allowed-values TRUE FALSE) (default TRUE))
    (slot piscina (type SYMBOL) (allowed-values TRUE FALSE) (default TRUE))
    (slot aire-acondicionat (type SYMBOL) (allowed-values TRUE FALSE) (default TRUE))
    (slot calefaccio (type SYMBOL) (allowed-values TRUE FALSE) (default TRUE))
    (slot terrassa (type SYMBOL) (allowed-values TRUE FALSE) (default TRUE))
    (slot traster (type SYMBOL) (allowed-values TRUE FALSE) (default TRUE))
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

(deffunction imprimir-justificacions (?oferta ?superficie-habitable-maxima ?pressupost ?jardi)
    (printout t "**************************" crlf)
    (bind ?num-restriccions 2)
    (bind ?num-restriccions-satisfetes 0)
    (bind ?justificacions (create$))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (<= (send ?habitatge get-superficie_habitable) ?superficie-habitable-maxima)
        then (bind ?num-restriccions-satisfetes (+ ?num-restriccions-satisfetes 1))
        else (bind ?justificacions (insert$ ?justificacions (+ (length$ ?justificacions) 1) "La superfície habitable de l'oferta és superior a la màxima")))
    (if (<= (send ?oferta get-preu) ?pressupost)
        then (bind ?num-restriccions-satisfetes (+ ?num-restriccions-satisfetes 1))
        else (bind ?justificacions (insert$ ?justificacions (+ (length$ ?justificacions) 1) "El preu de l'oferta és superior al pressupost")))
    (if (and (eq (send ?habitatge get-te_jardi) "true") (eq ?jardi TRUE))
    then
    (bind ?justificacions (insert$ ?justificacions (+ (length$ ?justificacions) 1) "I te jardi tal com desitjes!"))
    )
    (if (eq ?num-restriccions-satisfetes ?num-restriccions)
        then (printout t "L'oferta és adequada" crlf)
        else (if (<= (- ?num-restriccions ?num-restriccions-satisfetes) 2)
        then (printout t "L'oferta és parcialment adequada" crlf)
        else (printout t "L'oferta no és adequada" crlf)))
    (loop-for-count (?i 1 (length$ ?justificacions)) do
        (bind ?justificacio (nth$ ?i ?justificacions))
            (printout t ?justificacio crlf)
    )
    (printout t "**************************" crlf)
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
    (preferencies)
    (superficie-habitable-maxima preguntar)
    (superficie-habitable-minima preguntar)
    (presupost preguntar)
    (presupost-minim preguntar)
    (jardi preguntar)
    (piscina preguntar)
    (aire preguntar)
    (calefaccio preguntar)
    (garatge preguntar)
    (traster preguntar)
    (mobilitat-reduida preguntar)
    (habitants preguntar)
    (parelles preguntar)
    (terrassa preguntar)
    (menors preguntar)
    (edat preguntar)
)

(defrule preguntes::preguntar-superficie-habitable-maxima 
    (declare (salience 9))
    ?fet <- (superficie-habitable-maxima preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?superficie-habitable-maxima (preguntar-nombre "Quina superficie habitable maxima vols (m2)?" 0 1000))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (superficie-habitable-maxima ?superficie-habitable-maxima))
)
(defrule preguntes::preguntar-superficie-habitable-minima 
    (declare (salience 10))
    ?fet <- (superficie-habitable-minima preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?superficie-habitable-minima (preguntar-nombre "Quina superficie habitable minima vols (m2)?" 0 1000))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (superficie-habitable-minima ?superficie-habitable-minima))
)
(defrule preguntes::preguntar-presupost
    (declare (salience 50))
    ?fet <- (presupost preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?presupost (preguntar-nombre "Quin presupost tens?" 0 5000))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (presupost ?presupost))
)

(defrule preguntes::preguntar-presupost-minim
    (declare (salience 49))
    ?fet <- (presupost-minim preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?presupost (preguntar-nombre "Indica el preu minim que t'interessa?" 0 5000))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (presupost-minim ?presupost))
)

(defrule preguntes::preguntar-jardi
    (declare (salience 5))
    ?fet <- (jardi preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?jardi (preguntar-si-o-no "Voldries jardi?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (jardi ?jardi))
)

(defrule preguntes::preguntar-piscina
    (declare (salience 5))
    ?fet <- (piscina preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?piscina (preguntar-si-o-no "Voldries piscina?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (piscina ?piscina))
)

(defrule preguntes::preguntar-aire
    (declare (salience 5))
    ?fet <- (aire preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?aire (preguntar-si-o-no "Voldries aire acondicionat?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (aire-acondicionat ?aire))
)

(defrule preguntes::preguntar-calefaccio
    (declare (salience 5))
    ?fet <- (calefaccio preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?calefaccio (preguntar-si-o-no "Voldries calefaccio?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (calefaccio ?calefaccio))
)

(defrule preguntes::preguntar-garatge
    (declare (salience 4))
    ?fet <- (garatge preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?places (preguntar-nombre "Indica quantes places de garatge necessitaries?" 0 10))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (places-garatge ?places))
)

(defrule preguntes::preguntar-traster
    (declare (salience 5))
    ?fet <- (traster preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?traster (preguntar-si-o-no "T'interessa tenir traster?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (traster ?traster))
)

(defrule preguntes::preguntar-mobilitat
    (declare (salience 5))
    ?fet <- (mobilitat-reduida preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?mobilitat (preguntar-si-o-no "Algun habitant tindra mobilitat reduida?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (mobilitat-reduida ?mobilitat))
)

(defrule preguntes::preguntar-terrassa
    (declare (salience 5))
    ?fet <- (terrassa preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?terrassa (preguntar-si-o-no "Voldries terrassa?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (terrassa ?terrassa))
)

(defrule preguntes::preguntar-habitants
    (declare (salience 15))
    ?fet <- (habitants preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?habitants (preguntar-nombre "Quants habitants hi haura?" 1 10))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (nombre-habitants ?habitants))
)

(defrule preguntes::preguntar-parelles
    (declare (salience 14))
    ?fet <- (parelles preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?parelles (preguntar-nombre "Quantes parelles hi haura?" 0 5))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (nombre-parelles ?parelles))
)
(defrule preguntes::preguntar-edat
    (declare (salience 13))
    ?fet <- (edat preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?edat (preguntar-nombre "Quina es la teva edat?" 18 99))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (edat ?edat))
)

(defrule preguntes::preguntar-menors
    (declare (salience 13))
    ?fet <- (menors preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?menors (preguntar-si-o-no "Hi haura menors?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (menors ?menors))
)

(defrule preguntes::passar-a-seleccio "Passa al modul de seleccio"
    (declare (salience -10))
    (not (superficie-habitable-maxima preguntar))
    (not (superficie-habitable-minima preguntar))
    (not (presupost preguntar))
    (not (presupost-minim preguntar))
    (not (jardi preguntar))
    (not (piscina preguntar))
    (not (aire preguntar))
    (not (calefaccio preguntar))
    (not (garatge preguntar))
    (not (traster preguntar))
    (not (mobilitat-reduida preguntar))
    (not (habitants preguntar))
    (not (parelles preguntar))
    (not (terrassa preguntar))
    (not (menors preguntar))
    (not (edat preguntar))
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

(defrule abstraccio::abstreure-mida-habitatge-gran
    ?fet <- (mida-habitatge abstreure)
    (restriccions (superficie-habitable-maxima ?superficie-habitable-maxima))
    (not (problema-abstracte))
    (test (> ?superficie-habitable-maxima 150))
    =>
    (assert (problema-abstracte (mida-habitatge Gran)))
    (retract ?fet)
)
(defrule abstraccio::abstreure-mida-habitatge-mitja
    ?fet <- (mida-habitatge abstreure)
    (restriccions (superficie-habitable-maxima ?superficie-habitable-maxima))
    (not (problema-abstracte))
    (test (> ?superficie-habitable-maxima 69))
    (test (< ?superficie-habitable-maxima 151))
    =>
    (assert (problema-abstracte (mida-habitatge Mitja)))
    (retract ?fet)
)
(defrule abstraccio::abstreure-mida-habitatge-petit
    ?fet <- (mida-habitatge abstreure)
    (restriccions (superficie-habitable-maxima ?superficie-habitable-maxima))
    (not (problema-abstracte))
    (test (< ?superficie-habitable-maxima 70))
    =>
    (assert (problema-abstracte (mida-habitatge Petit)))
    (retract ?fet)
)


(defrule abstraccio::abstreure-presupost-car
    ?fet <- (presupost abstreure)
    (restriccions (presupost ?presupost))
    ?e <- (problema-abstracte (presupost ?presupost-abs))
    (test (eq ?presupost-abs NA))
    (test (> ?presupost 2000))
    =>
    (modify ?e (presupost Car)) 
    (retract ?fet)
)
(defrule abstraccio::abstreure-presupost-mitja
    ?fet <- (presupost abstreure)
    (restriccions (presupost ?presupost))
    ?e <- (problema-abstracte (presupost ?presupost-abs))
    (test (eq ?presupost-abs NA))
    (test (< ?presupost 2001))
    (test (> ?presupost 1000))
    =>
    (modify ?e (presupost Mitja)) 
    (retract ?fet)
)
(defrule abstraccio::abstreure-presupost-barat
    ?fet <- (presupost abstreure)
    (restriccions (presupost ?presupost))
    ?e <- (problema-abstracte (presupost ?presupost-abs))
    (test (eq ?presupost-abs NA))
    (test (< ?presupost 1001))
    =>
    (modify ?e (presupost Barat))
    (retract ?fet)
)

(defrule abstraccio::passar-a-construccio-abstracta
    (declare (salience -10))
    (not (mida-habitatge abstreure))
    (not (presupost abstreure))
    =>
    (printout t "Generant resultats abstractes..." crlf)
    (focus construccio-abstracta)
)

;;**************************
;;*  MODUL DE ABSTRACTA  *
;;**************************

(deffacts construccio-abstracta
    
)
(defrule construccio-abstracta::passar-a-construccio
    (declare (salience -10))
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
    (restriccions (presupost ?pressupost))
    (preferencies (jardi ?jardi))
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
        (imprimir-justificacions (send ?oferta-abstracta get-oferta) ?superficie-habitable-maxima ?pressupost ?jardi)
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
    (printout t "Costa " ?self:preu " euros." crlf)
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

