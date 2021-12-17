;;; TODOS:
;;; Vector de edats?
;;; banys
;;; abstreure a full
;;; llocs
;;; 

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

;; Maria
(defmodule preguntes
    (import MAIN ?ALL)
    (export ?ALL)
)

;; Edgar
(defmodule abstraccio
    (import MAIN ?ALL)
    (import preguntes ?ALL)
    (export ?ALL)
)

;; En el futuro
(defmodule construccio-solucio-abstracta
    (import MAIN ?ALL)
    (export ?ALL)
)

;; Edgar
(defmodule construccio
    (import MAIN ?ALL)
    (export ?ALL)
)

;; Maria
(defmodule presentacio
    (import MAIN ?ALL)
    (export ?ALL)
)

;;**********************
;;*    DEFTEMPLATES    *
;;**********************

; Coordenades d'un lloc
(deftemplate MAIN::coordenades
    (slot latitud (type FLOAT) (default 0.0))
    (slot longitud (type FLOAT) (default 0.0))
)

; Informació del sol·licitant
(deftemplate MAIN::informacio
    (slot preu-maxim-estricte (type SYMBOL) (allowed-values TRUE FALSE) (default TRUE))
    (slot hi-ha-infants (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
    (slot hi-ha-adolescents (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
    (slot hi-ha-joves (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
    (slot hi-ha-adults (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
    (slot hi-ha-ancians (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
)

; Restriccions del sol·licitant
(deftemplate MAIN::restriccions
    (slot estat-obra-minim (type SYMBOL) (allowed-values NOVA BON-ESTAT PER-REFORMAR) (default BON-ESTAT))
    (slot nombre-banys-minim (type INTEGER) (range 1 10) (default 1))
    (slot nombre-habitants (type INTEGER) (range 1 10) (default 1))
    (slot nombre-parelles (type INTEGER) (range 0 5) (default 0))
    (slot preu-maxim (type INTEGER) (default 0))
    (slot preu-minim (type INTEGER) (default 0))
    (slot superficie-habitable-maxima (type INTEGER) (default 0))
    (slot superficie-habitable-minima (type INTEGER) (default 0))
    (slot te-mascotes (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
    (slot te-mobilitat-reduida (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
)

; Preferencies del sol·licitant
(deftemplate MAIN::preferencies
    (slot vol-aprop-centres-salut (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-aprop-hipermercats (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (multislot vol-aprop-localitzacions (type INSTANCE))
    (slot vol-aprop-oci-nocturn (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-aprop-supermercats (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-aprop-transport-public (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-aprop-zones-comercials (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-aprop-zones-esportives (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-aprop-zones-verdes (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))

    (slot vol-aire-acondicionat (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-ascensor (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-balco (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-calefaccio (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-electrodomestics (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-jardi (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-mobles (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-piscina (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-places-garatge (type INTEGER) (default 0))
    (slot vol-terrassa (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-traster (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
)

(deftemplate MAIN::llista-recomanacions-abstractes
    (multislot recomanacions (type INSTANCE))
)

(deftemplate MAIN::llista-recomanacions
    (multislot recomanacions (type INSTANCE))
)

(deftemplate MAIN::problema-abstracte
    (slot mida-habitatge (type SYMBOL) (allowed-values Petit Mitja Gran NA) (default NA))
    (slot pressupost (type SYMBOL) (allowed-values Barat Mitja Car NA) (default NA))
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

; TODO (maria): fer-ho be sense tants parametres
(deffunction imprimir-justificacions (?oferta ?superficie-habitable-maxima ?preu-minim ?vol-jardi)
    (printout t "**************************" crlf)
    (bind ?num-restriccions 2)
    (bind ?num-restriccions-satisfetes 0)
    (bind ?justificacions (create$))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    ;(if (<= (send ?habitatge get-superficie_habitable) ?superficie-habitable-maxima)
    ;    then (bind ?num-restriccions-satisfetes (+ ?num-restriccions-satisfetes 1))
    ;    else (bind ?justificacions (insert$ ?justificacions (+ (length$ ?justificacions) 1) "La superfície habitable de l'oferta és superior a la màxima")))
    ;(if (<= (send ?oferta get-preu) ?pressupost)
    ;    then (bind ?num-restriccions-satisfetes (+ ?num-restriccions-satisfetes 1))
    ;    else (bind ?justificacions (insert$ ?justificacions (+ (length$ ?justificacions) 1) "El preu de l'oferta és superior al pressupost")))
    ;(if (and (eq (send ?habitatge get-te_jardi) "true") (eq ?jardi TRUE))
    ;then
    ;(bind ?justificacions (insert$ ?justificacions (+ (length$ ?justificacions) 1) "I te jardi tal com desitjes!"))
    ;)
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
    (preu-maxim-estricte preguntar)
    (hi-ha-infants preguntar)
    (hi-ha-adolescents preguntar)
    (hi-ha-joves preguntar)
    (hi-ha-joves preguntar)
    (hi-ha-joves preguntar)
    (estat-obra-minim preguntar)
    (nombre-banys-minim preguntar)
    (nombre-habitants preguntar)
    (nombre-parelles preguntar)
    (preu-maxim preguntar)
    (preu-minim preguntar)
    (superficie-habitable-maxima preguntar)
    (superficie-habitable-minima preguntar)
    (te-mascotes preguntar)
    (te-mobilitat-reduida preguntar)
    (vol-aire-acondicionat preguntar)
    (vol-ascensor preguntar)
    (vol-balco preguntar)
    (vol-calefaccio preguntar)
    (vol-electrodomestics preguntar)
    (vol-jardi preguntar)
    (vol-mobles preguntar)
    (vol-piscina preguntar)
    (vol-places-garatge preguntar)
    (vol-terrassa preguntar)
    (vol-traster preguntar)
    (vol-aprop-punts-interes preguntar)
    (vol-aprop-localitzacions preguntar)
)

(defrule preguntes::passar-a-seleccio "Passa al modul de seleccio"
    (declare (salience -10))
    (not (preu-maxim-estricte preguntar))
    (not (hi-ha-infants preguntar))
    (not (hi-ha-adolescents preguntar))
    (not (hi-ha-joves preguntar))
    (not (hi-ha-joves preguntar))
    (not (hi-ha-joves preguntar))
    (not (estat-obra-minim preguntar))
    (not (nombre-banys-minim preguntar))
    (not (nombre-habitants preguntar))
    (not (nombre-parelles preguntar))
    (not (preu-maxim preguntar))
    (not (preu-minim preguntar))
    (not (superficie-habitable-maxima preguntar))
    (not (superficie-habitable-minima preguntar))
    (not (te-mascotes preguntar))
    (not (te-mobilitat-reduida preguntar))
    (not (vol-aire-acondicionat preguntar))
    (not (vol-ascensor preguntar))
    (not (vol-balco preguntar))
    (not (vol-calefaccio preguntar))
    (not (vol-electrodomestics preguntar))
    (not (vol-jardi preguntar))
    (not (vol-mobles preguntar))
    (not (vol-piscina preguntar))
    (not (vol-places-garatge preguntar))
    (not (vol-terrassa preguntar))
    (not (vol-traster preguntar))
    (not (vol-aprop-punts-interes preguntar))
    (not (vol-aprop-localitzacions preguntar))
    =>
    (printout t "Abstraient problema..." crlf)
    (focus abstraccio)
)

;;************************
;;*  MODUL D'ABSTRACCIO  *
;;************************

(deffacts abstraccio
    (mida-habitatge abstreure)
    (pressupost abstreure)
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

(defrule abstraccio::abstreure-pressupost-car
    ?fet <- (pressupost abstreure)
    ; TODO (edgar): pensar si volem considerar com a pressupost el preu minim o el maxim
    (restriccions (preu-maxim ?pressupost))
    ?e <- (problema-abstracte (pressupost ?pressupost-abstracte))
    (test (eq ?pressupost-abstracte NA))
    (test (> ?pressupost 2000))
    =>
    (modify ?e (pressupost Car)) 
    (retract ?fet)
)
(defrule abstraccio::abstreure-pressupost-mitja
    ?fet <- (pressupost abstreure)
    ; TODO (edgar): pensar si volem considerar com a pressupost el preu minim o el maxim
    (restriccions (preu-maxim ?pressupost))
    ?e <- (problema-abstracte (pressupost ?pressupost-abstracte))
    (test (eq ?pressupost-abstracte NA))
    (test (< ?pressupost 2001))
    (test (> ?pressupost 1000))
    =>
    (modify ?e (pressupost Mitja)) 
    (retract ?fet)
)
(defrule abstraccio::abstreure-pressupost-barat
    ?fet <- (pressupost abstreure)
    ; TODO (edgar): pensar si volem considerar com a pressupost el preu minim o el maxim
    (restriccions (preu-maxim ?pressupost))
    ?e <- (problema-abstracte (pressupost ?pressupost-abstracte))
    (test (eq ?pressupost-abstracte NA))
    (test (< ?pressupost 1001))
    =>
    (modify ?e (pressupost Barat))
    (retract ?fet)
)

(defrule abstraccio::passar-a-construccio-abstracta
    (declare (salience -10))
    (not (mida-habitatge abstreure))
    (not (pressupost abstreure))
    =>
    (printout t "Generant resultats abstractes..." crlf)
    (focus construccio-solucio-abstracta)
)

;;**************************************************
;;*  MODUL DE CONSTRUCCIO DE LA SOLUCIO ABSTRACTA  *
;;**************************************************

(deffacts construccio-solucio-abstracta
)

(defrule construccio-solucio-abstracta::calcular-puntuacions
    (declare (salience 10))
    (problema-abstracte (mida-habitatge ?mida-habitatge))
    (problema-abstracte (pressupost ?pressupost))
    =>
    (bind ?llista-ofertes-abstractes (find-all-instances ((?inst OfertaAbstracta)) TRUE))
    (loop-for-count (?i 1 (length$ ?llista-ofertes-abstractes)) do
        (bind ?oferta-abstracta (nth$ ?i ?llista-ofertes-abstractes))
        (send ?oferta-abstracta calcula-puntuacio-mida-habitatge ?mida-habitatge)
        (send ?oferta-abstracta calcula-puntuacio-preu ?pressupost)
    )
)
(defrule construccio-solucio-abstracta::passar-a-construccio
    (declare (salience -10))
    =>
    (printout t "Generant resultats..." crlf)
    (focus construccio)
)
;;**************************
;;*  MODUL DE CONSTRUCCIO  *
;;**************************

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
    (restriccions (preu-minim ?preu-minim))
    (preferencies (vol-jardi ?vol-jardi))
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
        (imprimir-justificacions (send ?oferta-abstracta get-oferta) ?superficie-habitable-maxima ?preu-minim ?vol-jardi)
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

(defmessage-handler MAIN::OfertaAbstracta calcula-puntuacio-preu (?pressupost-solicitant)

    (bind ?preu-habitatge (send ?self get-mida-habitatge))
    (bind ?puntuacio 0)
    (bind ?justificacio "No te cap bonificacio pel preu de l'habitatge")

    (if (eq ?pressupost-solicitant Car)
        then (
            if (eq ?preu-habitatge Car)
            then
                (bind ?puntuacio 5)
                (bind ?justificacio "El pressupost es adient")
            else
                (bind ?puntuacio 3)
                (bind ?justificacio "El preu es menor al pressupost")
        )
    )
    (if (eq ?pressupost-solicitant Mitja)
        then (if (eq ?preu-habitatge Mitja)
            then
                (bind ?puntuacio 5)
                (bind ?justificacio "El pressupost es adient")
            else
                (if (eq ?preu-habitatge Barat)
                then
                    (bind ?puntuacio 3)
                    (bind ?justificacio "El preu es menor al pressupost")
                )
        )
    )
    
    (if (eq ?pressupost-solicitant Barat)
        then (if (eq ?preu-habitatge Barat)
            then
                (bind ?puntuacio 5)
                (bind ?justificacio "El pressupost es adient")
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

(defmessage-handler MAIN::Habitatge apte-mobilitat-reduida ()
    (bind ?apte FALSE)
    (if (eq ?self:te_ascensor "true")
        then (bind ?apte TRUE) 
    )
    (if (eq (class ?self) HabitatgeUnifamiliar) 
        then (bind ?apte TRUE)
        else (if (< (send ?self get-planta) 2)
            then (bind ?apte TRUE))
    )
    ?apte
)

;;********************
;;*  OfertaHandlers  *
;;********************

;;;"Retorna 0 si no adequat, 1 si parcialment adequat 2 si adequat" 
(defmessage-handler MAIN::Oferta adecuacio (?sup-min ?sup-max ?pres-min ?pres-max ?mob-red ?num-hab ?num-parelles)
    (bind ?faltes-acceptables 0)
    (bind ?faltes-inacceptables 0)
    (bind ?habitatge ?self:ofereix_a)

    (bind ?sup (send ?habitatge get-superficie_habitable))
    (if (> (- ?sup-min 20) ?sup)
        then (bind ?faltes-inacceptables 1)
        else (if (> ?sup-min ?sup) then 
            (bind ?faltes-acceptables (+ ?faltes-acceptables 1))
        )
    )
    (if (< ?sup-max ?sup) then
        (bind ?faltes-acceptables (+ ?faltes-acceptables 1))
    )

    (bind ?preu ?self:preu)
    (if (< (+ ?pres-max 150) ?preu)
        then (bind ?faltes-inacceptables 1)
        else (if (< ?pres-max ?preu) then 
            (bind ?faltes-acceptables (+ ?faltes-acceptables 1))
        )
    )
    (if (> ?pres-min ?preu) then
        (bind ?faltes-acceptables (+ ?faltes-acceptables 1))
    )

    (bind ?capacitat (send ?habitatge get-nombre_d_habitants_maxim))
    (if (< ?capacitat ?num-hab)
        then (bind ?faltes-inacceptables 1) )

    (bind ?capacitat-parelles (send ?habitatge get-nombre_de_dormitoris_dobles))
    (if (< ?capacitat-parelles ?num-parelles)
        then (bind ?faltes-inacceptables 1) )


    (if (and (eq ?mob-red TRUE) (eq (send ?habitatge apte-mobilitat-reduida) FALSE))
        then (bind ?faltes-inacceptables 1)
    )

    (bind ?resposta 0)
    (if (not (eq ?faltes-inacceptables 0))
        then (bind ?resposta 0)
        else 
        (if (eq ?faltes-acceptables 0)
            then (bind ?resposta 2)
            else (if (< ?faltes-acceptables 3)
                then (bind ?resposta 1)
                else (bind ?resposta 0)
            )
        )
    )
    ?resposta
)
(defmessage-handler MAIN::Oferta imprimir ()
    (send ?self:ofereix_a imprimir)
    (printout t "Costa " ?self:preu " euros." crlf)
    (if (eq ?self:admet_mascotes TRUE)
    then
    (printout t "Admet mascotes." crlf)
    else
    (printout t "No admet mascotes." crlf)
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

