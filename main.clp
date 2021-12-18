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

(deffunction preguntar-nombres (?cota-inferior ?cota-superior)
    (bind ?acabat FALSE)
    (bind ?llista nil)
    (while (eq ?acabat FALSE)
        (bind ?acabat TRUE)
        (format t "Introdueix els nombres separats per un espai: ")
        (bind ?resposta (readline))
        (bind ?nombres (str-explode ?resposta))
        (bind ?llista (create$))
        (progn$ (?var ?nombres)
            (if (and (integerp ?var) (and (>= ?var ?cota-inferior) (<= ?var ?cota-superior)))
                then
                    (if (not (member$ ?var ?llista))
                        then (bind ?llista (insert$ ?llista (+ (length$ ?llista) 1) ?var))
                    )
                else
                    (bind ?acabat FALSE)
            )
        )
    )
    ?llista
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
    (preferencies)
    (restriccions)

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
    (vol-traster preguntar)

    (vol-places-garatge preguntar)

    (vol-aprop-punts-interes preguntar)
    (vol-lluny-punts-interes preguntar)
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
    (modify ?informacio (hi-ha-adolescents ?hi-ha-adolescents))
)

(defrule preguntes::preguntar-hi-ha-joves
    (declare (salience 46))
    ?fet <- (hi-ha-joves preguntar)
    ?informacio <- (informacio)
    =>
    (bind ?hi-ha-joves (preguntar-si-o-no "Viuran joves a l'habitatge?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?informacio (hi-ha-joves ?hi-ha-joves))
)

(defrule preguntes::preguntar-hi-ha-adults
    (declare (salience 45))
    ?fet <- (hi-ha-adults preguntar)
    ?informacio <- (informacio)
    =>
    (bind ?hi-ha-adults (preguntar-si-o-no "Viuran adults a l'habitatge?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?informacio (hi-ha-adults ?hi-ha-adults))
)

(defrule preguntes::preguntar-hi-ha-ancians
    (declare (salience 44))
    ?fet <- (hi-ha-ancians preguntar)
    ?informacio <- (informacio)
    =>
    (bind ?hi-ha-ancians (preguntar-si-o-no "Viuran ancians a l'habitatge?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?informacio (hi-ha-ancians ?hi-ha-ancians))
)

(defrule preguntes::preguntar-preu-maxim
    (declare (salience 43))
    ?fet <- (preu-maxim preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?preu-maxim (preguntar-nombre "Quin preu mensual màxim estàs disposat a pagar?" 0 5000))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (preu-maxim ?preu-maxim))
)

(defrule preguntes::preguntar-preu-maxim-estricte
    (declare (salience 42))
    ?fet <- (preu-maxim-estricte preguntar)
    ?informacio <- (informacio)
    =>
    (bind ?preu-maxim-estricte (preguntar-si-o-no "Estàs disposat a pagar més si és per una oferta molt bona?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?informacio (preu-maxim-estricte ?preu-maxim-estricte))
)

(defrule preguntes::preguntar-preu-minim
    (declare (salience 41))
    ?fet <- (preu-minim preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?preu-minim (preguntar-nombre "Quin preu mensual mínim estàs disposat a pagar?" 0 5000))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (preu-minim ?preu-minim))
)

(defrule preguntes::preguntar-nombre-banys-minim
    (declare (salience 40))
    ?fet <- (nombre-banys-minim preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?nombre-banys-minim (preguntar-nombre "Quin és el mínim nombre de banys que vols a l'habitatge?" 1 10))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (nombre-banys-minim ?nombre-banys-minim))
)

(defrule preguntes::preguntar-superficie-habitable-maxima
    (declare (salience 39))
    ?fet <- (superficie-habitable-maxima preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?superficie-habitable-maxima (preguntar-nombre "Quina és la superfície habitable màxima que vols que tingui l'habitatge?" 0 1000))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (superficie-habitable-maxima ?superficie-habitable-maxima))
)

(defrule preguntes::preguntar-superficie-habitable-minima
    (declare (salience 38))
    ?fet <- (superficie-habitable-minima preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?superficie-habitable-minima (preguntar-nombre "Quina és la superfície habitable mínima que vols que tingui l'habitatge?" 0 1000))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (superficie-habitable-minima ?superficie-habitable-minima))
)

(defrule preguntes::preguntar-estat-obra-minim
    (declare (salience 37))
    ?fet <- (estat-obra-minim preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?estat-obra-minim (preguntar "Quin és l'estat d'obra mínim que estàs disposat a acceptar?" PER-REFORMAR BON-ESTAT NOVA))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (estat-obra-minim ?estat-obra-minim))
)

(defrule preguntes::preguntar-te-mascotes
    (declare (salience 36))
    ?fet <- (te-mascotes preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?te-mascotes (preguntar-si-o-no "Viuran mascotes a l'habitatge?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (te-mascotes ?te-mascotes))
)

(defrule preguntes::preguntar-te-mobilitat-reduida
    (declare (salience 35))
    ?fet <- (te-mobilitat-reduida preguntar)
    ?restriccions <- (restriccions)
    =>
    (bind ?te-mobilitat-reduida (preguntar-si-o-no "Algun habitant té mobilitat reduïda?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?restriccions (te-mobilitat-reduida ?te-mobilitat-reduida))
)

(defrule preguntes::preguntar-vol-ascensor
    (declare (salience 34))
    ?fet <- (vol-ascensor preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?vol-ascensor (preguntar-si-o-no-na "Vols ascensor?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (vol-ascensor ?vol-ascensor))
)

(defrule preguntes::preguntar-vol-balco
    (declare (salience 33))
    ?fet <- (vol-balco preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?vol-balco (preguntar-si-o-no-na "Vols balcó?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (vol-balco ?vol-balco))
)

(defrule preguntes::preguntar-vol-jardi
    (declare (salience 32))
    ?fet <- (vol-jardi preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?vol-jardi (preguntar-si-o-no-na "Vols jardí?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (vol-jardi ?vol-jardi))
)

(defrule preguntes::preguntar-vol-terrassa
    (declare (salience 31))
    ?fet <- (vol-terrassa preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?vol-terrassa (preguntar-si-o-no-na "Vols terrassa?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (vol-terrassa ?vol-terrassa))
)

(defrule preguntes::preguntar-vol-piscina
    (declare (salience 30))
    ?fet <- (vol-piscina preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?vol-piscina (preguntar-si-o-no-na "Vols piscina?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (vol-piscina ?vol-piscina))
)

(defrule preguntes::preguntar-vol-aire-condicionat
    (declare (salience 29))
    ?fet <- (vol-aire-condicionat preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?vol-aire-condicionat (preguntar-si-o-no-na "Vols aire condicionat?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (vol-aire-condicionat ?vol-aire-condicionat))
)

(defrule preguntes::preguntar-vol-calefaccio
    (declare (salience 28))
    ?fet <- (vol-calefaccio preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?vol-calefaccio (preguntar-si-o-no-na "Vols calefacció?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (vol-calefaccio ?vol-calefaccio))
)

(defrule preguntes::preguntar-vol-electrodomestics
    (declare (salience 27))
    ?fet <- (vol-electrodomestics preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?vol-electrodomestics (preguntar-si-o-no-na "Vols electrodomèstics?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (vol-electrodomestics ?vol-electrodomestics))
)

(defrule preguntes::preguntar-vol-mobles
    (declare (salience 26))
    ?fet <- (vol-mobles preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?vol-mobles (preguntar-si-o-no-na "Vols mobles?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (vol-mobles ?vol-mobles))
)

(defrule preguntes::preguntar-vol-traster
    (declare (salience 25))
    ?fet <- (vol-traster preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?vol-traster (preguntar-si-o-no-na "Vols traster?"))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (vol-traster ?vol-traster))
)

(defrule preguntes::preguntar-vol-places-garatge
    (declare (salience 24))
    ?fet <- (vol-places-garatge preguntar)
    ?preferencies <- (preferencies)
    =>
    (bind ?vol-places-garatge (preguntar-nombre "Quin és el nombre mínim de places de garatge que vols que tingui l'habitatge?" 0 5))
    (printout t crlf)
    (retract ?fet)
    (modify ?preferencies (vol-places-garatge ?vol-places-garatge))
)

(defrule preguntes::preguntar-vol-aprop-punts-interes
    (declare (salience 100))
    ?fet <- (vol-aprop-punts-interes preguntar)
    ?preferencies <- (preferencies)
    =>
    (printout t "Quins d'aquests punts d'interès vols tenir aprop?" crlf)
    (printout t "1 - Centres de salut" crlf)
    (printout t "2 - Hipermercats" crlf)
    (printout t "3 - Zones d'oci nocturn" crlf)
    (printout t "4 - Supermercats" crlf)
    (printout t "5 - Transport públic" crlf)
    (printout t "6 - Zones comercials" crlf)
    (printout t "7 - Zones esportives" crlf)
    (printout t "8 - Zones verdes" crlf)
    (bind ?punts-interes (preguntar-nombres 1 8))
    (loop-for-count (?i 1 (length$ ?punts-interes)) do
        (switch ?i
            (case 1 then (modify ?preferencies (vol-aprop-centres-salut TRUE)))
            (case 2 then (modify ?preferencies (vol-aprop-hipermercats TRUE)))
            (case 3 then (modify ?preferencies (vol-aprop-oci-nocturn TRUE)))
            (case 4 then (modify ?preferencies (vol-aprop-supermercats TRUE)))
            (case 5 then (modify ?preferencies (vol-aprop-transport-public TRUE)))
            (case 6 then (modify ?preferencies (vol-aprop-zones-comercials TRUE)))
            (case 7 then (modify ?preferencies (vol-aprop-zones-esportives TRUE)))
            (case 8 then (modify ?preferencies (vol-aprop-zones-verdes TRUE)))
        )
    )
    (printout t crlf)
    (retract ?fet)
)

(defrule preguntes::preguntar-vol-lluny-punts-interes
    (declare (salience 99))
    ?fet <- (vol-lluny-punts-interes preguntar)
    ?preferencies <- (preferencies)
    =>
    (retract ?fet)
    (printout t "Quins d'aquests punts d'interès no vols tenir aprop?" crlf)
    (printout t "1 - Centres de salut" crlf)
    (printout t "2 - Hipermercats" crlf)
    (printout t "3 - Zones d'oci nocturn" crlf)
    (printout t "4 - Supermercats" crlf)
    (printout t "5 - Transport públic" crlf)
    (printout t "6 - Zones comercials" crlf)
    (printout t "7 - Zones esportives" crlf)
    (printout t "8 - Zones verdes" crlf)
    (bind ?punts-interes (preguntar-nombres 1 8))
    (loop-for-count (?i 1 (length$ ?punts-interes)) do
        (switch ?i
            (case 1 then (modify ?preferencies (vol-aprop-centres-salut FALSE)))
            (case 2 then (modify ?preferencies (vol-aprop-hipermercats FALSE)))
            (case 3 then (modify ?preferencies (vol-aprop-oci-nocturn FALSE)))
            (case 4 then (modify ?preferencies (vol-aprop-supermercats FALSE)))
            (case 5 then (modify ?preferencies (vol-aprop-transport-public FALSE)))
            (case 6 then (modify ?preferencies (vol-aprop-zones-comercials FALSE)))
            (case 7 then (modify ?preferencies (vol-aprop-zones-esportives FALSE)))
            (case 8 then (modify ?preferencies (vol-aprop-zones-verdes FALSE)))
        )
    )
    (printout t crlf)
)

(defrule preguntes::preguntar-vol-aprop-localitzacions
    (declare (salience 98))
    ?fet <- (vol-aprop-localitzacions preguntar)
    ?preferencies <- (preferencies)
    =>
    (retract ?fet)
    (bind ?continua TRUE)
    (bind $?llista (create$))
    (while (eq ?continua TRUE)
        (bind ?continua (preguntar-si-o-no "Vols indicar una nova localització concreta que vulguis tenir aprop? (escola, feina, ...)"))
        (if (eq ?continua TRUE)
            then 
            (printout t "Introdueix la latitud: ")
            (bind ?latitud (read))
            (printout t crlf)
            (printout t "Introdueix la longitud: ")
            (bind ?longitud (read))
            (printout t crlf)
            (assert (coordenades (latitud ?latitud) (longitud ?longitud)))
            (bind ?llista (insert$ ?llista (+ (length$ ?llista) 1) coordenades))
        )
    )
    (retract ?fet)
    (modify ?preferencies (vol-aprop-localitzacions ?llista))
    (printout t crlf)
)

(defrule preguntes::passar-a-seleccio "Passa al modul de selecció"
    (declare (salience -10))
    (informacio)
    (preferencies)
    (restriccions)
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
    (not (vol-traster preguntar))
    (not (vol-places-garatge preguntar))
    (not (vol-aprop-punts-interes preguntar))
    (not (vol-lluny-punts-interes preguntar))
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

    (switch ?self:estat_de_l_obra
        (case "NOVA" then (printout t "És obra nova" crlf))
        (case "BON-ESTAT" then (printout t "L'obra està en bon estat" crlf))
        (case "PER-REFORMAR" then (printout t "Cal reformar l'obra" crlf))
    )
    (switch ?self:sol
        (case "mati" then (printout t "Li toca el sol al matí" crlf))
        (case "tarda" then (printout t "Li toca el sol a la tarda" crlf))
        (case "tot el dia" then (printout t "Li toca el sol tot el dia" crlf))
        (case "mai" then (printout t "No li toca mai el sol" crlf))
    )
    (switch ?self:te_bones_vistes
        (case TRUE then (printout t "Té bones vistes" crlf))
        (case FALSE then (printout t "No té bones vistes" crlf))
    )
    (switch ?self:te_ascensor
        (case TRUE then (printout t "Té ascensor" crlf))
        (case FALSE then (printout t "No té ascensor" crlf))
    )
    (switch ?self:te_aire_condicionat
        (case TRUE then (printout t "Té aire condicionat" crlf))
        (case FALSE then (printout t "No té aire condicionat" crlf))
    )
    (switch ?self:te_calefaccio
        (case TRUE then (printout t "Té calefacció" crlf))
        (case FALSE then (printout t "No té calefacció" crlf))
    )
    (switch ?self:te_traster
        (case TRUE then (printout t "Té traster"  crlf))
        (case FALSE then (printout t "No té traster" crlf))
    )
    (switch ?self:piscina
        (case TRUE then (printout t "Té piscina" crlf))
        (case FALSE then (printout t "No té piscina" crlf))
    )
    (switch ?self:te_balco
        (case TRUE then (printout t "Té balcó" crlf))
        (case FALSE then (printout t "No té balcó" crlf))
    )
    (switch ?self:te_terrassa
        (case TRUE then (printout t "Té terrassa" crlf))
        (case FALSE then (printout t "No té terrassa" crlf))
    )
    (switch ?self:te_jardi
        (case TRUE then (printout t "Té jardí" crlf))
        (case FALSE then (printout t "No té jardí" crlf))
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
    (bind ?linia (format nil "%s" ?self:descripcio))
    (printout t ?linia crlf)
    (printout t crlf)
    (printout t "=============================== Informació sobre l'oferta =" crlf)
    (printout t "Costa " ?self:preu "€ al mes" crlf)
    (switch ?self:inclou_mobles
        (case TRUE then (printout t "Està moblat" crlf))
        (case FALSE then (printout t "No està moblat" crlf))
    )
    (switch ?self:inclou_electrodomestics
        (case TRUE then (printout t "Inclou electrodomèstics" crlf))
        (case FALSE then (printout t "No inclou electrodomèstics" crlf))
    )
    (switch ?self:admet_mascotes
        (case TRUE then (printout t "Admet mascotes" crlf))
        (case FALSE then (printout t "No admet mascotes" crlf))
    )
    (switch ?self:numero_de_places_de_garatge
        (case 0 then (printout t "No té places de garatge" crlf))
        (case 1 then (printout t "Té 1 plaça de garatge" crlf))
        (default (printout t "Té " ?self:numero_de_places_de_garatge " places de garatge" crlf))
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

