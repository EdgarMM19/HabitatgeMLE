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
    (slot nombre-recomanacions (type INTEGER) (range 1 10) (default 5))
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
    (slot nombre-dormitoris-dobles (type INTEGER) (range 0 5) (default 0))
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

    (slot vol-aire-condicionat (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
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
    (slot familia (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
    (slot parella-sense-fills (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
    (slot joves (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
    (slot ancians (type SYMBOL) (allowed-values TRUE FALSE) (default FALSE))
)

;;*******************
;;*    DEFCLASSES   *
;;*******************

(defclass OfertaAbstracta (is-a USER) (role concrete)
	(slot oferta (type INSTANCE) (create-accessor read-write))
	(slot mida-habitatge (type SYMBOL) (allowed-values Petit Mitja Gran) (create-accessor read-write))
    (slot preu (type SYMBOL) (allowed-values Barat Mitja Car) (create-accessor read-write))

    (slot aprop-transport (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot aprop-zona-comercial (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot aprop-supermercat (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot aprop-hipermercat (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot aprop-centre-educatiu (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot aprop-centre-salut (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot aprop-zona-verda (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot aprop-esport (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot aprop-oci-nocturn (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))

    (slot adequat-familia (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot adequat-ancians (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot adequat-joves (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot adequat-parelles (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
	(slot puntuacio (type INTEGER) (create-accessor read-write) (default 0))
	(multislot justificacio-puntuacio (type STRING) (create-accessor read-write))
)

;;****************************
;;* DEFFUNCTIONS - PREGUNTES *
;;****************************

(deffunction preguntar (?pregunta $?valors-permesos)
    (format t "%s (%s) " ?pregunta (implode$ ?valors-permesos))
    (bind ?resposta (read))
    (while (not (member ?resposta ?valors-permesos)) do
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

(deffunction preguntar-si-o-no-na (?pregunta)
    (bind ?resposta (preguntar ?pregunta si no indiferent s n i))
    (if (or (eq ?resposta si) (eq ?resposta s))
        then TRUE
        else (if (or (eq ?resposta no) (eq ?resposta n))
            then FALSE
            else NA
        )
    )
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

        (send ?ofertaAbstracta calcula-aprop-oci-nocturn)
        (send ?ofertaAbstracta calcula-aprop-esport)
        (send ?ofertaAbstracta calcula-aprop-zona-verda)
        (send ?ofertaAbstracta calcula-aprop-centre-salut)
        (send ?ofertaAbstracta calcula-aprop-centre-educatiu)
        (send ?ofertaAbstracta calcula-aprop-hipermercat)
        (send ?ofertaAbstracta calcula-aprop-supermercat)
        (send ?ofertaAbstracta calcula-aprop-zona-comercial)
        (send ?ofertaAbstracta calcula-aprop-transport)

        (send ?ofertaAbstracta calcula-adecuacio-familia)
        (send ?ofertaAbstracta calcula-adecuacio-ancians)
        (send ?ofertaAbstracta calcula-adecuacio-joves)
        (send ?ofertaAbstracta calcula-adecuacio-parelles)
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
    (printout t "-                                                         -" crlf)
    (printout t "-                          Hola!                          -" crlf)
    (printout t "-      Respon les següents preguntes i et recomanaré      -" crlf)
    (printout t "-            ofertes d'habitatges a Barcelona.            -" crlf)
    (printout t "-                                                         -" crlf)
    (printout t "-----------------------------------------------------------" crlf)
    (printout t crlf crlf)
    (focus preguntes)
)

;;************************
;;*  MODUL DE PREGUNTES  *
;;************************

(deffacts dades
    (informacio)
    (restriccions)
    (preferencies)

    (nombre-recomanacions preguntar)
    (nombre-habitants preguntar)
    (nombre-dormitoris-dobles preguntar)
    (hi-ha-infants preguntar)
    (hi-ha-adolescents preguntar)
    (hi-ha-joves preguntar)
    (hi-ha-adults preguntar)
    (hi-ha-ancians preguntar)
    
    (preu-maxim preguntar)
    (preu-maxim-estricte preguntar)
    (preu-minim preguntar)
    (nombre-banys-minim preguntar)
    (superficie-habitable-maxima preguntar)
    (superficie-habitable-minima preguntar)
    (estat-obra-minim preguntar)
    
    (te-mascotes preguntar)
    (te-mobilitat-reduida preguntar)

    (vol-ascensor preguntar)
    (vol-balco preguntar)
    (vol-jardi preguntar)
    (vol-terrassa preguntar)
    (vol-piscina preguntar)

    (vol-aire-condicionat preguntar)
    (vol-calefaccio preguntar)
    (vol-electrodomestics preguntar)
    (vol-mobles preguntar)

    (vol-places-garatge preguntar)
    (vol-traster preguntar)

    (vol-aprop-punts-interes preguntar)
    (vol-aprop-localitzacions preguntar)
)

(defrule preguntes::preguntar-nombre-recomanacions
     (declare (salience 51))
     ?fet <- (nombre-recomanacions preguntar)
     ?informacio <- (informacio)
     =>
     (bind ?nombre-recomanacions (preguntar-nombre "Quantes ofertes vols que et recomani?" 1 10))
     (printout t crlf)
     (retract ?fet)
     (modify ?informacio (nombre-recomanacions ?nombre-recomanacions))
)

(defrule preguntes::preguntar-nombre-habitants
     (declare (salience 50))
     ?fet <- (nombre-habitants preguntar)
     ?restriccions <- (restriccions)
     =>
     (bind ?nombre-habitants (preguntar-nombre "Quants habitants viuran a l'habitatge?" 1 10))
     (printout t crlf)
     (retract ?fet)
     (modify ?restriccions (nombre-habitants ?nombre-habitants))
)

(defrule preguntes::preguntar-nombre-dormitoris-dobles
     (declare (salience 49))
     ?fet <- (nombre-dormitoris-dobles preguntar)
     ?restriccions <- (restriccions)
     =>
     (bind ?nombre-dormitoris-dobles (preguntar-nombre "Quants dormitoris dobles vols que tingui l'habitatge?" 0 5))
     (printout t crlf)
     (retract ?fet)
     (modify ?restriccions (nombre-dormitoris-dobles ?nombre-dormitoris-dobles))
)

(defrule preguntes::preguntar-hi-ha-infants
     (declare (salience 48))
     ?fet <- (hi-ha-infants preguntar)
     ?informacio <- (informacio)
     =>
     (bind ?hi-ha-infants (preguntar-si-o-no "Viuran infants a l'habitatge?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?informacio (hi-ha-infants ?hi-ha-infants))
)

(defrule preguntes::preguntar-hi-ha-adolescents
     (declare (salience 47))
     ?fet <- (hi-ha-adolescents preguntar)
     ?informacio <- (informacio)
     =>
     (bind ?hi-ha-adolescents (preguntar-si-o-no "Viuran adolescents a l'habitatge?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?informacio (hi-ha-adolescents ?hi-ha-adolescents ))
)

(defrule preguntes::preguntar-hi-ha-joves
     (declare (salience 46))
     ?fet <- (hi-ha-joves preguntar)
     ?informacio <- (informacio)
     =>
     (bind ?hi-ha-joves (preguntar-si-o-no "Viuran joves a l'habitatge?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?informacio (hi-ha-joves ?hi-ha-joves ))
)

(defrule preguntes::preguntar-hi-ha-adults
     (declare (salience 45))
     ?fet <- (hi-ha-adults preguntar)
     ?informacio <- (informacio)
     =>
     (bind ?hi-ha-adults (preguntar-si-o-no "Viuran adults a l'habitatge?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?informacio (hi-ha-adults ?hi-ha-adults ))
)

(defrule preguntes::preguntar-hi-ha-ancians
     (declare (salience 44))
     ?fet <- (hi-ha-ancians preguntar)
     ?informacio <- (informacio)
     =>
     (bind ?hi-ha-ancians (preguntar-si-o-no "Viuran ancians a l'habitatge?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?informacio (hi-ha-ancians ?hi-ha-ancians ))
)

(defrule preguntes::preguntar-preu-maxim
     (declare (salience 43))
     ?fet <- (preu-maxim preguntar)
     ?restriccions <- (restriccions)
     =>
     (bind ?preu-maxim (preguntar-nombre "Quin preu mensual màxim estàs disposat a pagar?" 0 5000))
     (printout t crlf)
     (retract ?fet)
     (modify ?restriccions (preu-maxim ?preu-maxim ))
)

(defrule preguntes::preguntar-preu-maxim-estricte
     (declare (salience 42))
     ?fet <- (preu-maxim-estricte preguntar)
     ?informacio <- (informacio)
     =>
     (bind ?preu-maxim-estricte (preguntar-si-o-no "Estàs disposat a pagar més si és per una oferta molt bona?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?informacio (preu-maxim-estricte ?preu-maxim-estricte ))
)

(defrule preguntes::preguntar-preu-minim
     (declare (salience 41))
     ?fet <- (preu-minim preguntar)
     ?restriccions <- (restriccions)
     =>
     (bind ?preu-minim (preguntar-nombre "Quin preu mensual mínim estàs disposat a pagar?" 0 5000))
     (printout t crlf)
     (retract ?fet)
     (modify ?restriccions (preu-minim ?preu-minim ))
)

(defrule preguntes::preguntar-nombre-banys-minim
     (declare (salience 40))
     ?fet <- (nombre-banys-minim preguntar)
     ?restriccions <- (restriccions)
     =>
     (bind ?nombre-banys-minim (preguntar-nombre "Quin és el mínim nombre de banys que vols a l'habitatge?" 1 10))
     (printout t crlf)
     (retract ?fet)
     (modify ?restriccions (nombre-banys-minim ?nombre-banys-minim ))
)

(defrule preguntes::preguntar-superficie-habitable-maxima
     (declare (salience 39))
     ?fet <- (superficie-habitable-maxima preguntar)
     ?restriccions <- (restriccions)
     =>
     (bind ?superficie-habitable-maxima (preguntar-nombre "Quina és la superfície habitable màxima que vols que tingui l'habitatge?" 0 1000))
     (printout t crlf)
     (retract ?fet)
     (modify ?restriccions (superficie-habitable-maxima ?superficie-habitable-maxima ))
)

(defrule preguntes::preguntar-superficie-habitable-minima
     (declare (salience 38))
     ?fet <- (superficie-habitable-minima preguntar)
     ?restriccions <- (restriccions)
     =>
     (bind ?superficie-habitable-minima (preguntar-nombre "Quina és la superfície habitable mínima que vols que tingui l'habitatge?" 0 1000))
     (printout t crlf)
     (retract ?fet)
     (modify ?restriccions (superficie-habitable-minima ?superficie-habitable-minima ))
)

(defrule preguntes::preguntar-estat-obra-minim
     (declare (salience 37))
     ?fet <- (estat-obra-minim preguntar)
     ?restriccions <- (restriccions)
     =>
     (bind ?estat-obra-minim (preguntar "Quin és l'estat d'obra mínim que estàs disposat a acceptar?" PER-REFORMAR BON-ESTAT NOVA))
     (printout t crlf)
     (retract ?fet)
     (modify ?restriccions (estat-obra-minim ?estat-obra-minim ))
)

(defrule preguntes::preguntar-te-mascotes
     (declare (salience 36))
     ?fet <- (te-mascotes preguntar)
     ?restriccions <- (restriccions)
     =>
     (bind ?te-mascotes (preguntar-si-o-no "Viuran mascotes a l'habitatge?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?restriccions (te-mascotes ?te-mascotes ))
)

(defrule preguntes::preguntar-te-mobilitat-reduida
     (declare (salience 35))
     ?fet <- (te-mobilitat-reduida preguntar)
     ?restriccions <- (restriccions)
     =>
     (bind ?te-mobilitat-reduida (preguntar-si-o-no "Algun habitant té mobilitat reduïda?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?restriccions (te-mobilitat-reduida ?te-mobilitat-reduida ))
)

(defrule preguntes::preguntar-vol-ascensor
     (declare (salience 34))
     ?fet <- (vol-ascensor preguntar)
     ?preferencies <- (preferencies)
     =>
     (bind ?vol-ascensor (preguntar-si-o-no-na "Vols ascensor?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?preferencies (vol-ascensor ?vol-ascensor ))
)

(defrule preguntes::preguntar-vol-balco
     (declare (salience 33))
     ?fet <- (vol-balco preguntar)
     ?preferencies <- (preferencies)
     =>
     (bind ?vol-balco (preguntar-si-o-no-na "Vols balcó?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?preferencies (vol-balco ?vol-balco ))
)

(defrule preguntes::preguntar-vol-jardi
     (declare (salience 32))
     ?fet <- (vol-jardi preguntar)
     ?preferencies <- (preferencies)
     =>
     (bind ?vol-jardi (preguntar-si-o-no-na "Vols jardí?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?preferencies (vol-jardi ?vol-jardi ))
)

(defrule preguntes::preguntar-vol-terrassa
     (declare (salience 31))
     ?fet <- (vol-terrassa preguntar)
     ?preferencies <- (preferencies)
     =>
     (bind ?vol-terrassa (preguntar-si-o-no-na "Vols terrassa?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?preferencies (vol-terrassa ?vol-terrassa ))
)

(defrule preguntes::preguntar-vol-piscina
     (declare (salience 30))
     ?fet <- (vol-piscina preguntar)
     ?preferencies <- (preferencies)
     =>
     (bind ?vol-piscina (preguntar-si-o-no-na "Vols piscina?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?preferencies (vol-piscina ?vol-piscina ))
)

(defrule preguntes::preguntar-vol-aire-condicionat
     (declare (salience 29))
     ?fet <- (vol-aire-condicionat preguntar)
     ?preferencies <- (preferencies)
     =>
     (bind ?vol-aire-condicionat (preguntar-si-o-no-na "Vols aire condicionat?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?preferencies (vol-aire-condicionat ?vol-aire-condicionat ))
)

(defrule preguntes::preguntar-vol-calefaccio
     (declare (salience 28))
     ?fet <- (vol-calefaccio preguntar)
     ?preferencies <- (preferencies)
     =>
     (bind ?vol-calefaccio (preguntar-si-o-no-na "Vols calefacció?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?preferencies (vol-calefaccio ?vol-calefaccio ))
)

(defrule preguntes::preguntar-vol-electrodomestics
     (declare (salience 27))
     ?fet <- (vol-electrodomestics preguntar)
     ?preferencies <- (preferencies)
     =>
     (bind ?vol-electrodomestics (preguntar-si-o-no-na "Vols electrodomèstics?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?preferencies (vol-electrodomestics ?vol-electrodomestics ))
)

(defrule preguntes::preguntar-vol-mobles
     (declare (salience 26))
     ?fet <- (vol-mobles preguntar)
     ?preferencies <- (preferencies)
     =>
     (bind ?vol-mobles (preguntar-si-o-no-na "Vols mobles?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?preferencies (vol-mobles ?vol-mobles ))
)

(defrule preguntes::preguntar-vol-places-garatge
     (declare (salience 25))
     ?fet <- (vol-places-garatge preguntar)
     ?preferencies <- (preferencies)
     =>
     (bind ?vol-places-garatge (preguntar-nombre "Quin és el nombre mínim de places de garatge que vols que tingui l'habitatge?" 0 5))
     (printout t crlf)
     (retract ?fet)
     (modify ?preferencies (vol-places-garatge ?vol-places-garatge ))
)

(defrule preguntes::preguntar-vol-traster
     (declare (salience 24))
     ?fet <- (vol-traster preguntar)
     ?preferencies <- (preferencies)
     =>
     (bind ?vol-traster (preguntar-si-o-no-na "Vols traster?"))
     (printout t crlf)
     (retract ?fet)
     (modify ?preferencies (vol-traster ?vol-traster ))
)

(defrule preguntes::preguntar-vol-aprop-punts-interes
     (declare (salience 23))
     ?fet <- (vol-aprop-punts-interes preguntar)
     ?preferencies <- (preferencies)
     =>
     (printout t crlf)
     (retract ?fet)
)

(defrule preguntes::preguntar-vol-aprop-localitzacions
     (declare (salience 22))
     ?fet <- (vol-aprop-localitzacions preguntar)
     ?preferencies <- (preferencies)
     =>
     (printout t crlf)
     (retract ?fet)
)

(defrule preguntes::passar-a-seleccio "Passa al modul de selecció"
    (declare (salience -10))
    (not (nombre-recomanacions preguntar))
    (not (nombre-habitants preguntar))
    (not (nombre-dormitoris-dobles preguntar))
    (not (hi-ha-infants preguntar))
    (not (hi-ha-adolescents preguntar))
    (not (hi-ha-joves preguntar))
    (not (hi-ha-adults preguntar))
    (not (hi-ha-ancians preguntar))
    (not (preu-maxim preguntar))
    (not (preu-maxim-estricte preguntar))
    (not (preu-minim preguntar))
    (not (nombre-banys-minim preguntar))
    (not (superficie-habitable-maxima preguntar))
    (not (superficie-habitable-minima preguntar))
    (not (estat-obra-minim preguntar))
    (not (te-mascotes preguntar))
    (not (te-mobilitat-reduida preguntar))
    (not (vol-ascensor preguntar))
    (not (vol-balco preguntar))
    (not (vol-jardi preguntar))
    (not (vol-terrassa preguntar))
    (not (vol-piscina preguntar))
    (not (vol-aire-condicionat preguntar))
    (not (vol-calefaccio preguntar))
    (not (vol-electrodomestics preguntar))
    (not (vol-mobles preguntar))
    (not (vol-places-garatge preguntar))
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
    (informacio (nombre-recomanacions ?nombre-recomanacions))
    (restriccions (superficie-habitable-maxima ?superficie-habitable-maxima))
    (restriccions (preu-minim ?preu-minim))
    (preferencies (vol-jardi ?vol-jardi))
    =>
    (printout t crlf)
    (bind ?llista-ofertes-abstractes (find-all-instances ((?inst OfertaAbstracta)) TRUE))
    (bind ?llista-ordenada (sort comparar-ofertes ?llista-ofertes-abstractes))
    (if (< (length$ ?llista-ordenada) ?nombre-recomanacions)
        then (bind ?nombre-recomanacions (length$ ?llista-ordenada))
    )
    (loop-for-count (?i 1 ?nombre-recomanacions) do
        (bind ?oferta-abstracta (nth$ ?i ?llista-ordenada))
        (if (> (send ?oferta-abstracta get-puntuacio) 0)
            then 
                ; TODO: esborrar després de debugar
                (printout t "DEBUG: Oferta amb puntuacio: " (send ?oferta-abstracta get-puntuacio) crlf (send ?oferta-abstracta get-justificacio-puntuacio) crlf)
                (send (send ?oferta-abstracta get-oferta) imprimir ?i)
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

(defmessage-handler MAIN::OfertaAbstracta esta-a-prop (?element-localitzable)
    (bind ?latitud (send ?element-localitzable get-latitud))
    (bind ?longitud (send ?element-localitzable get-longitud))
    (bind ?oferta ?self:oferta)
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (bind ?distancia (send ?habitatge distancia ?latitud ?longitud))
    (bind ?resposta FALSE)
    (if (< ?distancia 500) then    
     (bind ?resposta TRUE))
    ?resposta
)

(defmessage-handler MAIN::OfertaAbstracta calcula-aprop-transport ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Parada de transport public")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) then
                (bind ?resposta TRUE)
                )
        )
    )
    (send ?self put-aprop-transport ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-aprop-zona-comercial  ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Zona comercial")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) then
                (bind ?resposta TRUE)
                )
        )
    )
    (send ?self put-aprop-zona-comercial ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-aprop-supermercat ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Supermercat")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) then
                (bind ?resposta TRUE)
                )
        )
    )
    (send ?self put-aprop-supermercat ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-aprop-hipermercat ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Hipermercat")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) then
                (bind ?resposta TRUE)
                )
        )
    )
    (send ?self put-aprop-hipermercat ?resposta)
)
    
(defmessage-handler MAIN::OfertaAbstracta calcula-aprop-centre-educatiu ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Centre educatiu")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) then
                (bind ?resposta TRUE)
                )
        )
    )
    (send ?self put-aprop-centre-educatiu ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-aprop-centre-salut ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Centre de salut")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) then
                (bind ?resposta TRUE)
                )
        )
    )
    (send ?self put-aprop-centre-salut ?resposta)
)

   
(defmessage-handler MAIN::OfertaAbstracta calcula-aprop-zona-verda ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Zona verda")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) then
                (bind ?resposta TRUE)
                )
        )
    )
    (send ?self put-aprop-zona-verda ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-aprop-esport ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Zona esportiva")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) then
                (bind ?resposta TRUE)
                )
        )
    )
    (send ?self put-aprop-esport ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-aprop-oci-nocturn ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Zona d'oci nocturn")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) then
                (bind ?resposta TRUE)
                )
        )
    )
    (send ?self put-aprop-oci-nocturn ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-aprop-transport ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Parada de transport public")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) then
                (bind ?resposta TRUE)
                )
        )
    )
    (send ?self put-aprop-transport ?resposta)
)


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

(defmessage-handler MAIN::OfertaAbstracta calcula-adecuacio-familia ()
    (bind ?oferta ?self:oferta)
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (bind ?punts 0)
    (bind ?no-adequat 0)
    (if (< (send ?habitatge get-nombre_de_banys) 2) then
        (bind ?no-adecuat 1)
    )
    (bind ?punts (+ ?punts (send ?habitatge get-nombre_de_banys)))

    (if (< (send ?habitatge get-nombre_de_dormitoris_dobles) 1) then
        (bind ?no-adecuat 1)
    )

    (if (< (send ?habitatge get-nombre_de_dormitoris_simples) 2) then
        (bind ?no-adecuat 1)
    )
    (bind ?punts (+ ?punts (send ?habitatge get-nombre_de_dormitoris_simples)))

    (if (< (send ?habitatge get-nombre_d_habitants_maxim) 5) then
        (bind ?no-adecuat 1)
    )

    (if (eq (send ?habitatge get-te_jardi) "true") then
        (bind ?punts (+ ?punts 4))
    )
    (if (eq (send ?habitatge get-piscina) "true") then
        (bind ?punts (+ ?punts 4))
    )
    (if (eq ?self:aprop-zona-verda TRUE) then
            (bind ?punts (+ ?punts 4))
    )
    (if (eq ?self:aprop-zona-comercial TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (or (not (eq ?no-adequat 0))
            (< ?punts 10))
        then (send ?self put-adequat-familia FALSE)
        else (send ?self put-adequat-familia TRUE)
    )
    (printout t "DEBUG adecuat familia " (send ?self get-adequat-familia) " " ?punts crlf)
)


(defmessage-handler MAIN::OfertaAbstracta calcula-adecuacio-ancians ()
        (bind ?oferta ?self:oferta)
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (bind ?punts 0)

    (if (eq (class ?habitatge) HabitatgeUnifamiliar) 
        then (bind ?punts (+ ?punts 4))
        else (if (< (send ?habitatge get-planta) 2)
            then (bind ?punts (+ ?punts 4))
        )
    )
    (if (eq (send ?habitatge get-te_ascensor) "true")
        then (bind ?punts (+ ?punts 4)) )

    (if (eq (send ?habitatge get-te_calefaccio) "true")
        then (bind ?punts (+ ?punts 2)) )

    (if (eq (send ?habitatge get-te_aire_condicionat) "true")
        then (bind ?punts (+ ?punts 2)) )

    (if (eq (send ?habitatge get-sol) "tot el dia")
        then (bind ?punts (+ ?punts 2)) )

    (if (eq ?self:aprop-zona-verda TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (eq ?self:aprop-centre-salut TRUE) then
            (bind ?punts (+ ?punts 3))
    )
    (if (eq ?self:aprop-supermercat TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (eq ?self:aprop-oci-nocturn FALSE) then
            (bind ?punts (+ ?punts 1))
    )
    (if (< ?punts 10)
        then (send ?self put-adequat-ancians FALSE)
        else (send ?self put-adequat-ancians TRUE)
    )

    (printout t "DEBUG adecuat ancians " (send ?self get-adequat-ancians) " " ?punts crlf)
)


(defmessage-handler MAIN::OfertaAbstracta calcula-adecuacio-joves ()
    (bind ?oferta ?self:oferta)
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (bind ?punts 0)
    (bind ?no-adequat 0)
    (if (> (send ?habitatge get-nombre_de_banys) 1) then
        (bind ?punts (+ ?punts 4))
    )

    (if (not (eq ?self:preu Barat)) then
         (bind ?no-adecuat 1)
    )

    (bind ?punts (+ ?punts (* 2 (send ?habitatge get-nombre_de_dormitoris_simples))))
    (bind ?punts (+ ?punts (send ?habitatge get-nombre_d_habitants_maxim)))

    (if (eq (send ?oferta get-inclou_mobles) "true") then
        (bind ?punts (+ ?punts 4))
    )

    (if (eq ?self:aprop-oci-nocturn TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (eq ?self:aprop-esport TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (eq ?self:aprop-transport TRUE) then
            (bind ?punts (+ ?punts 3))
    )
    (if (or (not (eq ?no-adequat 0))
            (< ?punts 15))
        then (send ?self put-adequat-joves FALSE)
        else (send ?self put-adequat-joves TRUE)
    )
    (printout t "DEBUG adecuat joves " (send ?self get-adequat-joves) " " ?punts crlf)
)


(defmessage-handler MAIN::OfertaAbstracta calcula-adecuacio-parelles ()
    (bind ?oferta ?self:oferta)
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (bind ?punts 0)
    (bind ?no-adequat 0)

    (if (eq (send ?habitatge get-nombre_de_dormitoris_dobles) 0) then
         (bind ?no-adecuat 1)
    )
    (if (not (eq ?self:preu Barat)) then
        (bind ?punts (+ ?punts 2))
    )

    (if (eq (send ?habitatge get-piscina) "true") then
        (bind ?punts (+ ?punts 2))
    )
    (if (eq (send ?habitatge get-te_aire_condicionat) "true") then
        (bind ?punts (+ ?punts 2))
    )
    
    (if (eq (send ?habitatge get-te_balco) "true") then
        (bind ?punts (+ ?punts 1))
    )
    (if (eq (send ?habitatge get-te_calefaccio) "true") then
        (bind ?punts (+ ?punts 1))
    )
    (if (eq (send ?habitatge get-te_terrassa) "true") then
        (bind ?punts (+ ?punts 3))
    )

    (if (eq ?self:aprop-oci-nocturn TRUE) then
            (bind ?punts (+ ?punts 1))
    )
    (if (eq ?self:aprop-esport TRUE) then
            (bind ?punts (+ ?punts 3))
    )
    (if (eq ?self:aprop-zona-comercial TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (eq ?self:aprop-transport TRUE) then
            (bind ?punts (+ ?punts 2))
    )

    (if (or (not (eq ?no-adequat 0))
            (< ?punts 15))
        then (send ?self put-adequat-parelles FALSE)
        else (send ?self put-adequat-parelles TRUE)
    )
    (printout t "DEBUG adecuat parelles " (send ?self get-adequat-parelles) " " ?punts crlf)
    
)

;;***********************
;;*  HabitatgeHandlers  *
;;***********************

(defmessage-handler MAIN::HabitatgeUnifamiliar imprimir-planta ()
    (printout t "És unifamiliar" crlf)
)

(defmessage-handler MAIN::HabitatgeCol·lectiu imprimir-planta () 
    (printout t "Està a la planta " ?self:planta crlf)
)

(defmessage-handler MAIN::Habitatge imprimir ()
    (printout t "Està situat a " (send ?self get-latitud) " N " (send ?self get-longitud) " E" crlf)
    (send ?self imprimir-planta)
    (printout t "Té " ?self:superficie_habitable " m2" crlf)
    (printout t "Pot tenir " ?self:nombre_d_habitants_maxim " habitant(s)" crlf)
    (printout t "Té " ?self:nombre_de_dormitoris " dormitori(s): " ?self:nombre_de_dormitoris_simples " de simple(s) i " ?self:nombre_de_dormitoris_dobles " de doble(s)" crlf)
    (printout t "Té " ?self:nombre_de_banys " bany(s)" crlf)
    (if (eq ?self:estat_de_l_obra "NOVA")
        then (printout t "És obra nova" crlf)
        else (if (eq ?self:estat_de_l_obra "BON-ESTAT")
            then (printout t "L'obra està en bon estat" crlf)
            else (printout t "Cal reformar l'obra" crlf)
        )
    )
    (if (eq ?self:sol "mati")
        then (printout t "Li toca el sol al matí" crlf)
        else (if (eq ?self:sol "tarda")
            then (printout t "Li toca el sol a la tarda" crlf)
            else (if (eq ?self:sol "mai")
                then (printout t "No li toca mai el sol" crlf)
                else (if (eq ?self:sol "tot el dia")
                    then (printout t "Li toca el sol tot el dia" crlf)
                )
            )
        )
    )
    (if (eq ?self:te_bones_vistes TRUE)
        then (printout t "Té bones vistes" crlf)
        else (printout t "No té bones vistes" crlf)
    )
    (if (eq ?self:te_ascensor TRUE)
        then (printout t "Té ascensor" crlf)
        else (printout t "No té ascensor" crlf)
    )
    (if (eq ?self:te_aire_condicionat TRUE)
        then (printout t "Té aire condicionat" crlf)
        else (printout t "No té aire condicionat" crlf)
    )
    (if (eq ?self:te_calefaccio TRUE)
        then (printout t "Té calefacció" crlf)
        else (printout t "No té calefacció" crlf)
    )
    (if (eq ?self:te_traster TRUE)
        then (printout t "Té traster" crlf)
        else (printout t "No té traster" crlf)
    )
    (if (eq ?self:piscina TRUE)
        then (printout t "Té piscina" crlf)
        else (printout t "No té piscina" crlf)
    )
    (if (eq ?self:te_balco TRUE)
        then (printout t "Té balcó" crlf)
        else (printout t "No té balcó" crlf)
    )
    (if (eq ?self:te_terrassa TRUE)
        then (printout t "Té terrassa" crlf)
        else (printout t "No té terrassa" crlf)
    )
    (if (eq ?self:te_jardi TRUE)
        then (printout t "Té jardí" crlf)
        else (printout t "No té jardí" crlf)
    )
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
(defmessage-handler MAIN::Oferta imprimir (?n)
    (printout t crlf)
    (printout t "- OFERTA " ?n " ------------------------------------------------" crlf)
    (printout t crlf)
    (printout t ?self:descripcio crlf)
    (printout t crlf)
    (printout t "=============================== Informació sobre l'oferta =" crlf)
    (printout t "Costa " ?self:preu "€ al mes" crlf)
    (if (eq ?self:inclou_mobles TRUE)
        then (printout t "Està moblat" crlf)
        else (printout t "No està moblat" crlf)
    )
    (if (eq ?self:inclou_electrodomestics TRUE)
        then (printout t "Inclou electrodomèstics" crlf)
        else (printout t "No inclou electrodomèstics" crlf)
    )
    (if (eq ?self:admet_mascotes TRUE)
        then (printout t "Admet mascotes" crlf)
        else (printout t "No admet mascotes" crlf)
    )
    (if (eq ?self:numero_de_places_de_garatge 0)
        then (printout t "No té places de garatge" crlf)
        else (if (eq ?self:numero_de_places_de_garatge 1)
            then (printout t "Té 1 plaça de garatge" crlf) 
            else (printout t "Té " ?self:numero_de_places_de_garatge " places de garatge" crlf)
        )
    )
    (printout t crlf)
    (printout t "============================ Informació sobre l'habitatge =" crlf)
    (send ?self:ofereix_a imprimir)
    (printout t "-----------------------------------------------------------" crlf)
    (printout t "=================================== Adequació de l'oferta =" crlf)
    ; imprimir justificacions
    (printout t "-----------------------------------------------------------" crlf)
    (printout t crlf)
)

;;*************************
;;*  ElementLocalitzable  *
;;*************************

(defmessage-handler MAIN::ElementLocalitzable distancia (?latitud ?longitud)
    0
)