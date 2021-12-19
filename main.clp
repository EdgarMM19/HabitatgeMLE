;;;======================================================
;;;     Cal executar CLIPS des del directori del projecte
;;;     i carregar-lo:
;;;
;;;     (load "ontologia.clp")
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

(defmodule contruccio-abstracte
    (import MAIN ?ALL)
    (import abstraccio ?ALL)
    (export ?ALL)
)

(defmodule construccio
    (import MAIN ?ALL)
    (import contruccio-abstracte ?ALL)
    (export ?ALL)
)

(defmodule presentacio
    (import MAIN ?ALL)
    (import construccio ?ALL)
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
    (slot vol-a-prop-centres-salut (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-a-prop-hipermercats (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (multislot vol-a-prop-localitzacions (type INSTANCE))
    (slot vol-a-prop-oci-nocturn (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-a-prop-supermercats (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-a-prop-transport-public (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-a-prop-zones-comercials (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-a-prop-zones-esportives (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))
    (slot vol-a-prop-zones-verdes (type SYMBOL) (allowed-values TRUE FALSE NA) (default NA))

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

    (slot a-prop-transport (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot a-prop-zona-comercial (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot a-prop-supermercat (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot a-prop-hipermercat (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot a-prop-centre-educatiu (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot a-prop-centre-salut (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot a-prop-zona-verda (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot a-prop-esport (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot a-prop-oci-nocturn (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))

    (slot adequat-familia (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot adequat-ancians (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot adequat-joves (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
    (slot adequat-parelles (type SYMBOL) (allowed-values TRUE FALSE) (create-accessor read-write))
	(slot puntuacio (type INTEGER) (create-accessor read-write) (default 0))
	(multislot justificacio-puntuacio (type STRING) (create-accessor read-write))
)

(defclass OfertaSolucio (is-a USER) (role concrete)
    (slot oferta (type INSTANCE) (create-accessor read-write))

    (slot posicio (type INTEGER) (create-accessor read-write))
    (slot nombre-restriccions-insatisfetes (type INTEGER) (create-accessor read-write) (default 0))
    (slot nombre-preferencies-insatisfetes (type INTEGER) (create-accessor read-write) (default 0))
    (slot nombre-extres (type INTEGER) (create-accessor read-write) (default 0))

    (multislot justificacions-restriccions-insatisfetes (type STRING) (create-accessor read-write))
    (multislot justificacions-preferencies-insatisfetes (type STRING) (create-accessor read-write))
    (multislot justificacions-extres (type STRING) (create-accessor read-write))
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
    (while (not (and (integerp ?resposta) (and (>= ?resposta ?cota-inferior) (<= ?resposta ?cota-superior)))) do
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

        (send ?ofertaAbstracta calcula-a-prop-oci-nocturn)
        (send ?ofertaAbstracta calcula-a-prop-esport)
        (send ?ofertaAbstracta calcula-a-prop-zona-verda)
        (send ?ofertaAbstracta calcula-a-prop-centre-salut)
        (send ?ofertaAbstracta calcula-a-prop-centre-educatiu)
        (send ?ofertaAbstracta calcula-a-prop-hipermercat)
        (send ?ofertaAbstracta calcula-a-prop-supermercat)
        (send ?ofertaAbstracta calcula-a-prop-zona-comercial)
        (send ?ofertaAbstracta calcula-a-prop-transport)

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

(deffunction comparar-ofertes-solucio (?oferta1 ?oferta2)
    (< (send ?oferta1 get-posicio) (send ?oferta2 get-posicio))
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

    (vol-a-prop-punts-interes preguntar)
    (vol-lluny-punts-interes preguntar)
    (vol-a-prop-localitzacions preguntar)
)

(defrule preguntes::preguntar-nombre-recomanacions
    (declare (salience 51))
    ?fet <- (nombre-recomanacions preguntar)
    ?informacio <- (informacio)
    =>
    (bind ?nombre-recomanacions (preguntar-nombre "Quantes ofertes vols que et recomani com a màxim?" 1 10))
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

(defrule preguntes::preguntar-vol-a-prop-punts-interes
    (declare (salience 23))
    ?fet <- (vol-a-prop-punts-interes preguntar)
    ?preferencies <- (preferencies)
    =>
    (printout t "Quins d'aquests punts d'interès vols tenir a prop?" crlf)
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
            (case 1 then (modify ?preferencies (vol-a-prop-centres-salut TRUE)))
            (case 2 then (modify ?preferencies (vol-a-prop-hipermercats TRUE)))
            (case 3 then (modify ?preferencies (vol-a-prop-oci-nocturn TRUE)))
            (case 4 then (modify ?preferencies (vol-a-prop-supermercats TRUE)))
            (case 5 then (modify ?preferencies (vol-a-prop-transport-public TRUE)))
            (case 6 then (modify ?preferencies (vol-a-prop-zones-comercials TRUE)))
            (case 7 then (modify ?preferencies (vol-a-prop-zones-esportives TRUE)))
            (case 8 then (modify ?preferencies (vol-a-prop-zones-verdes TRUE)))
        )
    )
    (printout t crlf)
    (retract ?fet)
)

(defrule preguntes::preguntar-vol-lluny-punts-interes
    (declare (salience 22))
    ?fet <- (vol-lluny-punts-interes preguntar)
    ?preferencies <- (preferencies)
    =>
    (retract ?fet)
    (printout t "Quins d'aquests punts d'interès no vols tenir a prop?" crlf)
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
            (case 1 then (modify ?preferencies (vol-a-prop-centres-salut FALSE)))
            (case 2 then (modify ?preferencies (vol-a-prop-hipermercats FALSE)))
            (case 3 then (modify ?preferencies (vol-a-prop-oci-nocturn FALSE)))
            (case 4 then (modify ?preferencies (vol-a-prop-supermercats FALSE)))
            (case 5 then (modify ?preferencies (vol-a-prop-transport-public FALSE)))
            (case 6 then (modify ?preferencies (vol-a-prop-zones-comercials FALSE)))
            (case 7 then (modify ?preferencies (vol-a-prop-zones-esportives FALSE)))
            (case 8 then (modify ?preferencies (vol-a-prop-zones-verdes FALSE)))
        )
    )
    (printout t crlf)
)

(defrule preguntes::preguntar-vol-a-prop-localitzacions
    (declare (salience 21))
    ?fet <- (vol-a-prop-localitzacions preguntar)
    ?preferencies <- (preferencies)
    =>
    (retract ?fet)
    (bind ?continua TRUE)
    (bind $?llista (create$))
    (while (eq ?continua TRUE)
        (bind ?continua (preguntar-si-o-no "Vols indicar una nova localització concreta que vulguis tenir a prop? (escola, feina, ...)"))
        (if (eq ?continua TRUE)
            then 
            (printout t "Introdueix la latitud: ")
            (bind ?latitud (read))
            (printout t crlf)
            (printout t "Introdueix la longitud: ")
            (bind ?longitud (read))
            (printout t crlf)
            (bind ?localitzacio (make-instance (sym-cat elementLocalitzable- (gensym)) of ElementLocalitzable))
		    (send ?localitzacio put-latitud ?latitud)
            (send ?localitzacio put-longitud ?longitud)
            (bind ?llista (insert$ ?llista (+ (length$ ?llista) 1) ?localitzacio))
        )
    )
    (retract ?fet)
    (modify ?preferencies (vol-a-prop-localitzacions ?llista))
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
    (not (vol-a-prop-punts-interes preguntar))
    (not (vol-lluny-punts-interes preguntar))
    (not (vol-a-prop-localitzacions preguntar))
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
    (familia abstreure)
    (ancians abstreure)
    (joves abstreure)
    (parella abstreure)
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


(defrule abstraccio::abstreure-familia-true
    ?fet <- (familia abstreure)
    (informacio (hi-ha-infants ?infants))
    (informacio (hi-ha-adolescents ?adolescents))
    ?e <- (problema-abstracte)
    (test (or (eq ?infants TRUE) (eq ?adolescents TRUE)))
    =>
    (modify ?e (familia TRUE)) 
    (retract ?fet)
)

(defrule abstraccio::abstreure-familia-false
    ?fet <- (familia abstreure)
    (informacio (hi-ha-infants ?infants))
    (informacio (hi-ha-adolescents ?adolescents))
    ?e <- (problema-abstracte)
    (test (and (eq ?infants FALSE) (eq ?adolescents FALSE)))
    =>
    (modify ?e (familia FALSE)) 
    (retract ?fet)
)

(defrule abstraccio::abstreure-joves-true
    ?fet <- (joves abstreure)
    (informacio (hi-ha-joves ?joves))
    (informacio (hi-ha-infants ?infants))
    (informacio (hi-ha-ancians ?ancians))
    ?e <- (problema-abstracte)
    (test (eq ?joves TRUE))
    (test (eq ?infants FALSE))
    (test (eq ?ancians FALSE))
    =>
    (modify ?e (joves TRUE)) 
    (retract ?fet)
)

(defrule abstraccio::abstreure-joves-false
    ?fet <- (joves abstreure)
    (informacio (hi-ha-joves ?joves))
    (informacio (hi-ha-infants ?infants))
    (informacio (hi-ha-ancians ?ancians))
    ?e <- (problema-abstracte)
    (test (or (eq ?infants TRUE) (or (eq ?joves FALSE) (eq ?ancians TRUE))))
    =>
    (modify ?e (joves FALSE)) 
    (retract ?fet)
)

(defrule abstraccio::abstreure-ancians-true
    ?fet <- (ancians abstreure)
    (informacio (hi-ha-ancians ?ancians))
    ?e <- (problema-abstracte)
    (test (eq ?ancians TRUE))
    =>
    (modify ?e (ancians TRUE)) 
    (retract ?fet)
)

(defrule abstraccio::abstreure-ancians-false
    ?fet <- (ancians abstreure)
    (informacio (hi-ha-ancians ?ancians))
    ?e <- (problema-abstracte)
    (test (eq ?ancians FALSE))
    =>
    (modify ?e (ancians FALSE)) 
    (retract ?fet)
)

(defrule abstraccio::abstreure-parella-true
    ?fet <- (parella abstreure)
    (informacio (hi-ha-adults ?adults))
    (informacio (hi-ha-infants ?infants))
    (informacio (hi-ha-adolescents ?adolescents))
    (informacio (hi-ha-ancians ?ancians))
    (informacio (hi-ha-joves ?joves))
    ?e <- (problema-abstracte)
    (test (eq ?adults TRUE))
    (test (eq ?infants FALSE))
    (test (eq ?ancians FALSE))
    (test (eq ?joves FALSE))
    (test (eq ?adolescents FALSE))
    =>
    (modify ?e (parella-sense-fills TRUE)) 
    (retract ?fet)
)

(defrule abstraccio::abstreure-parella-false
    ?fet <- (parella abstreure)
    (informacio (hi-ha-adults ?adults))
    (informacio (hi-ha-infants ?infants))
    (informacio (hi-ha-ancians ?ancians))
    (informacio (hi-ha-joves ?joves))
    (informacio (hi-ha-adolescents ?adolescents))
    ?e <- (problema-abstracte)
    (test (or (or (eq ?adults FALSE) (eq ?infants TRUE)) (or (eq ?ancians TRUE) (or (eq ?joves TRUE) (eq ?adolescents TRUE)))))
    =>
    (modify ?e (parella-sense-fills FALSE)) 
    (retract ?fet)
)


(defrule abstraccio::passar-a-construccio-abstracta
    (declare (salience -10))
    (not (mida-habitatge abstreure))
    (not (pressupost abstreure))
    (not (familia abstreure))
    (not (ancians abstreure))
    (not (joves abstreure))
    (not (parella abstreure))
    =>
    (printout t "Generant resultats abstractes..." crlf)
    (focus contruccio-abstracte)
)

;;**************************************************
;;*  MODUL DE CONSTRUCCIO DE LA SOLUCIO ABSTRACTA  *
;;**************************************************

(deffacts contruccio-abstracte
    (construir abstracte)
)

(defrule contruccio-abstracte::calcular-puntuacions
    (declare (salience 10))
    ?fet <- (construir abstracte)
    (problema-abstracte (mida-habitatge ?mida-habitatge))
    (problema-abstracte (pressupost ?pressupost))
    (problema-abstracte (familia ?familia))
    (problema-abstracte (parella-sense-fills ?parella-sense-fills))
    (problema-abstracte (joves ?joves))
    (problema-abstracte (ancians ?ancians))
    =>
    (bind ?llista-ofertes-abstractes (find-all-instances ((?inst OfertaAbstracta)) TRUE))
    (loop-for-count (?i 1 (length$ ?llista-ofertes-abstractes)) do
        (bind ?oferta-abstracta (nth$ ?i ?llista-ofertes-abstractes))
        (send ?oferta-abstracta calcula-puntuacio-mida-habitatge ?mida-habitatge)
        (send ?oferta-abstracta calcula-puntuacio-preu ?pressupost)
        (send ?oferta-abstracta calcula-puntuacio-tipus-familia ?familia ?parella-sense-fills ?joves ?ancians)
    )
    (bind ?llista-ordenada (sort comparar-ofertes ?llista-ofertes-abstractes))
    (loop-for-count (?i 1 (length$ ?llista-ordenada)) do
        (bind ?oferta-abstracta (nth$ ?i ?llista-ordenada))
        (bind ?ofertaSolucio (make-instance (sym-cat OfertaSolucio- (gensym)) of OfertaSolucio))
        (send ?ofertaSolucio put-oferta (send ?oferta-abstracta get-oferta))
        (send ?ofertaSolucio put-posicio ?i)
    )
    (retract ?fet)
)

(defrule contruccio-abstracte::passar-a-construccio
    (declare (salience -10))
    (not (construir abstracte))
    =>
    (printout t "Generant resultats..." crlf)
    (focus construccio)
)

;;**************************
;;*  MODUL DE CONSTRUCCIO  *
;;**************************

(deffacts contruccio
    (construccio concreta)
)

(defrule construccio::construir-restriccions-estat-obra-bon-estat
    (declare (salience 50))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (restriccions (estat-obra-minim ?estat-obra-minim))
    (test (eq ?estat-obra-minim BON-ESTAT))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (eq (send ?habitatge get-estat_de_l_obra) PER-REFORMAR)
        then 
            (send ?oferta-solucio put-nombre-restriccions-insatisfetes (+ (send ?oferta-solucio get-nombre-restriccions-insatisfetes) 1))
            (bind ?justificacio "No satisfà la restricció d'estat de l'obra mínim")
            (slot-insert$ ?oferta-solucio justificacions-restriccions-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-restriccions-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-restriccions-estat-obra-nova
    (declare (salience 50))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (restriccions (estat-obra-minim ?estat-obra-minim))
    (test (eq ?estat-obra-minim NOVA))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (eq (send ?habitatge get-estat_de_l_obra) NOVA))
        then (send ?oferta-solucio put-nombre-restriccions-insatisfetes (+ (send ?oferta-solucio get-nombre-restriccions-insatisfetes) 1))
        (bind ?justificacio "No satisfà la restricció d'estat de l'obra mínim")
        (slot-insert$ ?oferta-solucio justificacions-restriccions-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-restriccions-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-restriccions-nombre-banys
    (declare (salience 49))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (restriccions (nombre-banys-minim ?nombre-banys-minim))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (< (send ?habitatge get-nombre_de_banys) ?nombre-banys-minim)
        then (send ?oferta-solucio put-nombre-restriccions-insatisfetes (+ (send ?oferta-solucio get-nombre-restriccions-insatisfetes) 1))
        (bind ?justificacio "No satisfà la restricció del nombre de banys mínim")
        (slot-insert$ ?oferta-solucio justificacions-restriccions-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-restriccions-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-restriccions-nombre-habitants
    (declare (salience 48))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (restriccions (nombre-habitants ?nombre-habitants))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (< (send ?habitatge get-nombre_d_habitants_maxim) ?nombre-habitants)
        then (send ?oferta-solucio put-nombre-restriccions-insatisfetes (+ (send ?oferta-solucio get-nombre-restriccions-insatisfetes) 1))
        (bind ?justificacio "No satisfà la restricció del nombre d'habitants")
        (slot-insert$ ?oferta-solucio justificacions-restriccions-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-restriccions-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-restriccions-nombre-dormitoris-dobles
    (declare (salience 47))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (restriccions (nombre-dormitoris-dobles ?nombre-dormitoris-dobles))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (< (send ?habitatge get-nombre_de_dormitoris_dobles) ?nombre-dormitoris-dobles)
        then (send ?oferta-solucio put-nombre-restriccions-insatisfetes (+ (send ?oferta-solucio get-nombre-restriccions-insatisfetes) 1))
        (bind ?justificacio "No satisfà la restricció del nombre de dormitoris dobles")
        (slot-insert$ ?oferta-solucio justificacions-restriccions-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-restriccions-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-restriccions-preu-maxim 
    (declare (salience 46))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (informacio (preu-maxim-estricte ?preu-maxim-estricte))
    (restriccions (preu-maxim ?preu-maxim))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (> (send ?oferta get-preu) ?preu-maxim)
        then (if (or (eq ?preu-maxim-estricte TRUE) (> (send ?oferta get-preu) (* ?preu-maxim 1.25)))
            then (send ?oferta-solucio put-nombre-restriccions-insatisfetes (+ (send ?oferta-solucio get-nombre-restriccions-insatisfetes) 1))
            (bind ?justificacio "No satisfà la restricció del preu màxim")
            (slot-insert$ ?oferta-solucio justificacions-restriccions-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-restriccions-insatisfetes))) ?justificacio)
        )
    )
)

(defrule construccio::construir-restriccions-preu-minim 
    (declare (salience 45))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (restriccions (preu-minim ?preu-minim))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (< (send ?oferta get-preu) ?preu-minim)
        then (send ?oferta-solucio put-nombre-restriccions-insatisfetes (+ (send ?oferta-solucio get-nombre-restriccions-insatisfetes) 1))
        (bind ?justificacio "No satisfà la restricció del preu mínim")
        (slot-insert$ ?oferta-solucio justificacions-restriccions-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-restriccions-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-restriccions-superficie-habitable-maxima
    (declare (salience 44))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (restriccions (superficie-habitable-maxima ?superficie-habitable-maxima))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (> (send ?habitatge get-superficie_habitable) ?superficie-habitable-maxima)
        then (send ?oferta-solucio put-nombre-restriccions-insatisfetes (+ (send ?oferta-solucio get-nombre-restriccions-insatisfetes) 1))
        (bind ?justificacio "No satisfà la restricció de la superfície habitable màxima")
        (slot-insert$ ?oferta-solucio justificacions-restriccions-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-restriccions-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-restriccions-superficie-habitable-minima
    (declare (salience 43))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (restriccions (superficie-habitable-minima ?superficie-habitable-minima))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (< (send ?habitatge get-superficie_habitable) ?superficie-habitable-minima)
        then (send ?oferta-solucio put-nombre-restriccions-insatisfetes (+ (send ?oferta-solucio get-nombre-restriccions-insatisfetes) 1))
        (bind ?justificacio "No satisfà la restricció de la superfície habitable mínima")
        (slot-insert$ ?oferta-solucio justificacions-restriccions-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-restriccions-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-restriccions-mascotes
    (declare (salience 42))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (restriccions (te-mascotes ?te-mascotes))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (and (not (send ?oferta get-admet_mascotes)) ?te-mascotes)
        then (send ?oferta-solucio put-nombre-restriccions-insatisfetes (+ (send ?oferta-solucio get-nombre-restriccions-insatisfetes) 1))
        (bind ?justificacio "No satisfà la restricció d'admetre mascotes'")
        (slot-insert$ ?oferta-solucio justificacions-restriccions-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-restriccions-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-restriccions-mobilitat-reduida
    (declare (salience 41))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (restriccions (te-mobilitat-reduida ?te-mobilitat-reduida))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (and (not (send ?habitatge apte-mobilitat-reduida)) ?te-mobilitat-reduida)
        then (send ?oferta-solucio put-nombre-restriccions-insatisfetes (+ (send ?oferta-solucio get-nombre-restriccions-insatisfetes) 1))
        (bind ?justificacio "No satisfà la restricció de ser apte per persones amb mobilitat reduïda'")
        (slot-insert$ ?oferta-solucio justificacions-restriccions-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-restriccions-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-centres-salut-cert
    (declare (salience 40))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-centres-salut ?vol-a-prop-centres-salut))
    (test (eq ?vol-a-prop-centres-salut TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?oferta esta-a-prop-centres-salut))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar a prop de centres de salut")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-centres-salut-fals
    (declare (salience 40))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-centres-salut ?vol-a-prop-centres-salut))
    (test (eq ?vol-a-prop-centres-salut FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?oferta esta-a-prop-centres-salut)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar lluny de centres de salut")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-hipermercats-cert
    (declare (salience 39))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-hipermercats ?vol-a-prop-hipermercats))
    (test (eq ?vol-a-prop-hipermercats TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?oferta esta-a-prop-hipermercats))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar a prop d'hipermercats")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-hipermercats-fals
    (declare (salience 39))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-hipermercats ?vol-a-prop-hipermercats))
    (test (eq ?vol-a-prop-hipermercats FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?oferta esta-a-prop-hipermercats)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar lluny d'hipermercats")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-localitzacions
    (declare (salience 38))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-localitzacions $?vol-a-prop-localitzacions))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (loop-for-count (?i 1 (length$ ?vol-a-prop-localitzacions)) do
        (bind ?localitzacio (nth$ ?i ?vol-a-prop-localitzacions))
        (if (not (send ?oferta esta-a-prop ?localitzacio))
            then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
            (bind ?justificacio (format nil "No satisfà la preferència d'estar a prop de la localització amb coordenades %f N %f E" (send ?localitzacio get-latitud) (send ?localitzacio get-longitud)))
            (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
        )
    )
)

(defrule construccio::construir-preferencies-a-prop-oci-nocturn-cert
    (declare (salience 37))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-oci-nocturn ?vol-a-prop-oci-nocturn))
    (test (eq ?vol-a-prop-oci-nocturn TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?oferta esta-a-prop-oci-nocturn))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar a prop de zones d'oci nocturn")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-oci-nocturn-fals
    (declare (salience 37))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-oci-nocturn ?vol-a-prop-oci-nocturn))
    (test (eq ?vol-a-prop-oci-nocturn FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?oferta esta-a-prop-oci-nocturn)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar lluny de zones d'oci nocturn")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-supermercats-cert
    (declare (salience 36))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-supermercats ?vol-a-prop-supermercats))
    (test (eq ?vol-a-prop-supermercats TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?oferta esta-a-prop-supermercats))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar a prop de supermercats")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-supermercats-fals
    (declare (salience 36))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-supermercats ?vol-a-prop-supermercats))
    (test (eq ?vol-a-prop-supermercats FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?oferta esta-a-prop-supermercats)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar lluny de supermercats")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-transport-public-cert
    (declare (salience 35))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-transport-public ?vol-a-prop-transport-public))
    (test (eq ?vol-a-prop-transport-public TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?oferta esta-a-prop-transport-public))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar a prop de parades de transport públic")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-transport-public-fals
    (declare (salience 35))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-transport-public ?vol-a-prop-transport-public))
    (test (eq ?vol-a-prop-transport-public FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?oferta esta-a-prop-transport-public)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar lluny de parades de transport públic")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-zones-comercials-cert
    (declare (salience 34))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-zones-comercials ?vol-a-prop-zones-comercials))
    (test (eq ?vol-a-prop-zones-comercials TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?oferta esta-a-prop-zones-comercials))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar a prop de zones comercials")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-zones-comercials-fals
    (declare (salience 34))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-zones-comercials ?vol-a-prop-zones-comercials))
    (test (eq ?vol-a-prop-zones-comercials FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?oferta esta-a-prop-zones-comercials)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar lluny de zones comercials")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-zones-esportives-cert
    (declare (salience 33))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-zones-esportives ?vol-a-prop-zones-esportives))
    (test (eq ?vol-a-prop-zones-esportives TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?oferta esta-a-prop-zones-esportives))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar a prop de zones esportives")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-zones-esportives-fals
    (declare (salience 33))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-zones-esportives ?vol-a-prop-zones-esportives))
    (test (eq ?vol-a-prop-zones-esportives FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?oferta esta-a-prop-zones-esportives)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar lluny de zones esportives")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-zones-verdes-cert
    (declare (salience 32))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-zones-verdes ?vol-a-prop-zones-verdes))
    (test (eq ?vol-a-prop-zones-verdes TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?oferta esta-a-prop-zones-verdes))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar a prop de zones verdes")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-a-prop-zones-verdes-fals
    (declare (salience 32))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-a-prop-zones-verdes ?vol-a-prop-zones-verdes))
    (test (eq ?vol-a-prop-zones-verdes FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?oferta esta-a-prop-zones-verdes)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència d'estar lluny de zones verdes")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-aire-condicionat-cert
    (declare (salience 31))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-aire-condicionat ?vol-aire-condicionat))
    (test (eq ?vol-aire-condicionat TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?habitatge get-te_aire_condicionat))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de tenir aire condicionat")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-aire-condicionat-fals
    (declare (salience 31))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-aire-condicionat ?vol-aire-condicionat))
    (test (eq ?vol-aire-condicionat FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?habitatge get-te_aire_condicionat)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de no tenir aire condicionat")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-ascensor-cert
    (declare (salience 30))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-ascensor ?vol-ascensor))
    (test (eq ?vol-ascensor TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?habitatge get-te_ascensor))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de tenir ascensor")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-ascensor-fals
    (declare (salience 30))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-ascensor ?vol-ascensor))
    (test (eq ?vol-ascensor FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?habitatge get-te_ascensor)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de no tenir ascensor")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-balco-cert
    (declare (salience 29))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-balco ?vol-balco))
    (test (eq ?vol-balco TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?habitatge get-te_balco))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de tenir balcó")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-balco-fals
    (declare (salience 29))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-balco ?vol-balco))
    (test (eq ?vol-balco FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?habitatge get-te_balco)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de no tenir balcó")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-calefaccio-cert
    (declare (salience 28))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-calefaccio ?vol-calefaccio))
    (test (eq ?vol-calefaccio TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?habitatge get-te_calefaccio))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de tenir calefacció")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-calefaccio-fals
    (declare (salience 28))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-calefaccio ?vol-calefaccio))
    (test (eq ?vol-calefaccio FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?habitatge get-te_calefaccio)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de no tenir calefacció")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-electrodomestics-cert
    (declare (salience 27))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-electrodomestics ?vol-electrodomestics))
    (test (eq ?vol-electrodomestics TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?oferta get-inclou_electrodomestics))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de tenir electrodomèstics")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-electrodomestics-fals
    (declare (salience 27))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-electrodomestics ?vol-electrodomestics))
    (test (eq ?vol-electrodomestics FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?oferta get-inclou_electrodomestics)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de no tenir electrodomèstics")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-jardi-cert
    (declare (salience 26))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-jardi ?vol-jardi))
    (test (eq ?vol-jardi TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?habitatge get-te_jardi))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de tenir jardí")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-jardi-fals
    (declare (salience 26))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-jardi ?vol-jardi))
    (test (eq ?vol-jardi FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?habitatge get-te_jardi)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de no tenir jardí")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-mobles-cert
    (declare (salience 25))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-mobles ?vol-mobles))
    (test (eq ?vol-mobles TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?oferta get-inclou_mobles))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de tenir mobles")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-mobles-fals
    (declare (salience 25))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-mobles ?vol-mobles))
    (test (eq ?vol-mobles FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?oferta get-inclou_mobles)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de no tenir mobles")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-piscina-cert
    (declare (salience 24))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-piscina ?vol-piscina))
    (test (eq ?vol-piscina TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?habitatge get-te_piscina))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de tenir piscina")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-piscina-fals
    (declare (salience 24))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-piscina ?vol-piscina))
    (test (eq ?vol-piscina FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?habitatge get-te_piscina)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de no tenir piscina")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-places-garatge
    (declare (salience 23))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-places-garatge ?vol-places-garatge))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (< (send ?oferta get-numero_de_places_de_garatge) ?vol-places-garatge)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència del nombre de places de garatge")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-terrassa-cert
    (declare (salience 22))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-terrassa ?vol-terrassa))
    (test (eq ?vol-terrassa TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?habitatge get-te_terrassa))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de tenir terrassa")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-terrassa-fals
    (declare (salience 22))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-terrassa ?vol-terrassa))
    (test (eq ?vol-terrassa FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?habitatge get-te_terrassa)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de no tenir terrassa")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-traster-cert
    (declare (salience 21))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-traster ?vol-traster))
    (test (eq ?vol-traster TRUE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (not (send ?habitatge get-te_traster))
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de tenir traster")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir-preferencies-traster-fals
    (declare (salience 21))
    ?oferta-solucio <- (object (is-a OfertaSolucio))
    (preferencies (vol-traster ?vol-traster))
    (test (eq ?vol-traster FALSE))
    =>
    (bind ?oferta (send ?oferta-solucio get-oferta))
    (bind ?habitatge (send ?oferta get-ofereix_a))
    (if (send ?habitatge get-te_traster)
        then (send ?oferta-solucio put-nombre-preferencies-insatisfetes (+ (send ?oferta-solucio get-nombre-preferencies-insatisfetes) 1))
        (bind ?justificacio "No satisfà la preferència de no tenir traster")
        (slot-insert$ ?oferta-solucio justificacions-preferencies-insatisfetes (+ 1 (length$ (send ?oferta-solucio get-justificacions-preferencies-insatisfetes))) ?justificacio)
    )
)

(defrule construccio::construir
    (declare (salience 10))
    ?fet <- (construccio concreta)
    (informacio (nombre-recomanacions ?nombre-recomanacions))
    =>
    (bind ?llista-concreta (create$))
    (bind ?recomanacions-fetes 0)

    (bind ?llista-ofertes-solucio (find-all-instances ((?inst OfertaSolucio)) TRUE))
    (bind ?llista-ordenada (sort comparar-ofertes-solucio ?llista-ofertes-solucio))

    (loop-for-count (?i 1 (length$ ?llista-ordenada)) do
        (if (eq ?recomanacions-fetes ?nombre-recomanacions) then
        (break))
        (bind ?oferta-solucio (nth$ ?i ?llista-ordenada))
        (if (eq 0 (send ?oferta-solucio get-nombre-restriccions-insatisfetes)) then
            (bind ?llista-concreta (insert$ ?llista-concreta (+ 1 (length$ ?llista-concreta)) ?oferta-solucio))
            (bind ?recomanacions-fetes (+ 1 ?recomanacions-fetes))
        )
    )
    (assert (llista-recomanacions (recomanacions ?llista-concreta)))
    (retract ?fet)
)

(defrule construccio::passar-a-presentacio
    (declare (salience -10))
    (llista-recomanacions (recomanacions $?))
    =>
    (printout t "Presentant resultats..." crlf)
    (focus presentacio)
)

;;**************************
;;*  MODUL DE PRESENTACIO  *
;;**************************

(defrule presentacio::mostrar-recomanacions
    (not (final))
    ?informacio <- (informacio)
    ?preferencies <- (preferencies)
    ?restriccions <- (restriccions)
    (llista-recomanacions (recomanacions $?llista-ordenada))
    =>
    (if (> (length$ ?llista-ordenada) 0)
        then
            (loop-for-count (?i 1 (length$ ?llista-ordenada)) do
                (bind ?oferta-solucio (nth$ ?i ?llista-ordenada))
                (send ?oferta-solucio imprimir ?i)
            )
        else
            (printout t "No hi ha cap oferta que satisfaci els teus criteris." crlf)
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

(defmessage-handler MAIN::OfertaAbstracta calcula-a-prop-transport ()
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
    (send ?self put-a-prop-transport ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-a-prop-zona-comercial  ()
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
    (send ?self put-a-prop-zona-comercial ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-a-prop-supermercat ()
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
    (send ?self put-a-prop-supermercat ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-a-prop-hipermercat ()
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
    (send ?self put-a-prop-hipermercat ?resposta)
)
    
(defmessage-handler MAIN::OfertaAbstracta calcula-a-prop-centre-educatiu ()
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
    (send ?self put-a-prop-centre-educatiu ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-a-prop-centre-salut ()
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
    (send ?self put-a-prop-centre-salut ?resposta)
)

   
(defmessage-handler MAIN::OfertaAbstracta calcula-a-prop-zona-verda ()
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
    (send ?self put-a-prop-zona-verda ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-a-prop-esport ()
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
    (send ?self put-a-prop-esport ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-a-prop-oci-nocturn ()
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
    (send ?self put-a-prop-oci-nocturn ?resposta)
)

(defmessage-handler MAIN::OfertaAbstracta calcula-a-prop-transport ()
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
    (send ?self put-a-prop-transport ?resposta)
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

(defmessage-handler MAIN::OfertaAbstracta calcula-puntuacio-tipus-familia (?familia ?parella-sense-fills ?joves ?ancians)
    (if (and (eq ?familia TRUE) (eq ?self:adequat-familia TRUE))
        then
            (send ?self put-puntuacio (+ 10 (send ?self get-puntuacio)))
    )
    (if (and (eq ?parella-sense-fills TRUE) (eq ?self:adequat-parelles TRUE))
        then
            (send ?self put-puntuacio (+ 10 (send ?self get-puntuacio)))
    )
    (if (and (eq ?joves TRUE) (eq ?self:adequat-joves TRUE))
        then
            (send ?self put-puntuacio (+ 10 (send ?self get-puntuacio)))
    )
    (if (and (eq ?ancians TRUE) (eq ?self:adequat-ancians TRUE))
        then
            (send ?self put-puntuacio (+ 10 (send ?self get-puntuacio)))
    )
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
    (if (eq (send ?habitatge get-te_piscina) "true") then
        (bind ?punts (+ ?punts 4))
    )
    (if (eq ?self:a-prop-zona-verda TRUE) then
            (bind ?punts (+ ?punts 4))
    )
    (if (eq ?self:a-prop-zona-comercial TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (or (not (eq ?no-adequat 0))
            (< ?punts 10))
        then (send ?self put-adequat-familia FALSE)
        else (send ?self put-adequat-familia TRUE)
    )
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

    (if (eq ?self:a-prop-zona-verda TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (eq ?self:a-prop-centre-salut TRUE) then
            (bind ?punts (+ ?punts 3))
    )
    (if (eq ?self:a-prop-supermercat TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (eq ?self:a-prop-oci-nocturn FALSE) then
            (bind ?punts (+ ?punts 1))
    )
    (if (< ?punts 10)
        then (send ?self put-adequat-ancians FALSE)
        else (send ?self put-adequat-ancians TRUE)
    )
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

    (if (eq ?self:a-prop-oci-nocturn TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (eq ?self:a-prop-esport TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (eq ?self:a-prop-transport TRUE) then
            (bind ?punts (+ ?punts 3))
    )
    (if (or (not (eq ?no-adequat 0))
            (< ?punts 15))
        then (send ?self put-adequat-joves FALSE)
        else (send ?self put-adequat-joves TRUE)
    )
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

    (if (eq (send ?habitatge get-te_piscina) "true") then
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

    (if (eq ?self:a-prop-oci-nocturn TRUE) then
            (bind ?punts (+ ?punts 1))
    )
    (if (eq ?self:a-prop-esport TRUE) then
            (bind ?punts (+ ?punts 3))
    )
    (if (eq ?self:a-prop-zona-comercial TRUE) then
            (bind ?punts (+ ?punts 2))
    )
    (if (eq ?self:a-prop-transport TRUE) then
            (bind ?punts (+ ?punts 2))
    )

    (if (or (not (eq ?no-adequat 0))
            (< ?punts 15))
        then (send ?self put-adequat-parelles FALSE)
        else (send ?self put-adequat-parelles TRUE)
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
    (switch ?self:te_piscina
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

(defmessage-handler MAIN::Oferta esta-a-prop (?element-localitzable)
    (bind ?latitud (send ?element-localitzable get-latitud))
    (bind ?longitud (send ?element-localitzable get-longitud))
    (bind ?habitatge ?self:ofereix_a)
    (bind ?distancia (send ?habitatge distancia ?latitud ?longitud))
    (bind ?resposta FALSE)
    (if (< ?distancia 1000) then    
        (bind ?resposta TRUE)
    )
    ?resposta
)

(defmessage-handler MAIN::Oferta esta-a-prop-centres-salut ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Centre de salut")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) 
                    then (bind ?resposta TRUE)
                )
        )
    )
    ?resposta
)

(defmessage-handler MAIN::Oferta esta-a-prop-hipermercats ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Hipermercat")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) 
                    then (bind ?resposta TRUE)
                )
        )
    )
    ?resposta
)

(defmessage-handler MAIN::Oferta esta-a-prop-oci-nocturn ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Zona d'oci nocturn")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) 
                    then (bind ?resposta TRUE)
                )
        )
    )
    ?resposta
)

(defmessage-handler MAIN::Oferta esta-a-prop-supermercats ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Supermercat")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) 
                    then (bind ?resposta TRUE)
                )
        )
    )
    ?resposta
)

(defmessage-handler MAIN::Oferta esta-a-prop-transport-public ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Parada de transport public")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) 
                    then (bind ?resposta TRUE)
                )
        )
    )
    ?resposta
)

(defmessage-handler MAIN::Oferta esta-a-prop-zones-comercials ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Zona comercial")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) 
                    then (bind ?resposta TRUE)
                )
        )
    )
    ?resposta
)

(defmessage-handler MAIN::Oferta esta-a-prop-zones-esportives ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Zona esportiva")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) 
                    then (bind ?resposta TRUE)
                )
        )
    )
    ?resposta
)

(defmessage-handler MAIN::Oferta esta-a-prop-zones-verdes ()
    (bind ?resposta FALSE)
    (bind ?llista-punt-interes (find-all-instances ((?inst PuntDInteres)) TRUE))
    (bind ?punts (length$ ?llista-punt-interes))
    (loop-for-count (?i 1 ?punts) do
        (bind ?punt (nth$ ?i ?llista-punt-interes))
        (if (eq (send ?punt get-categoria) "Zona verda")
            then 
                (if (eq (send ?self esta-a-prop ?punt) TRUE) 
                    then (bind ?resposta TRUE)
                )
        )
    )
    ?resposta
)

(defmessage-handler MAIN::Oferta imprimir ()
    (bind ?linia (format nil "%s" ?self:descripcio))
    (printout t ?linia crlf)
    (printout t crlf)
    (printout t "= Informació sobre l'oferta ===============================" crlf)
    (printout t crlf)
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
    (printout t "= Informació sobre l'habitatge ============================" crlf)
    (printout t crlf)
    (send ?self:ofereix_a imprimir)
    (printout t crlf)
)

;;********************
;;*  OfertaHandlers  *
;;********************

(defmessage-handler MAIN::OfertaSolucio imprimir (?n)
    
    (printout t crlf)
    (printout t "- OFERTA " ?n " ------------------------------------------------" crlf)
    (printout t crlf)

    (bind ?oferta (send ?self get-oferta))
    (send ?oferta imprimir)

    (printout t "= Adequació de l'oferta ===================================" crlf)
    (printout t crlf)

    (if (or (> ?self:nombre-restriccions-insatisfetes 0) (> ?self:nombre-preferencies-insatisfetes 2)) 
        then (printout t "L'oferta no és adequada" crlf)
        else
            (if (> ?self:nombre-preferencies-insatisfetes 0) 
                then
                    (printout t "L'oferta és parcialment adequada" crlf)
                    (printout t crlf)
                    (loop-for-count (?i 1 (length$ ?self:justificacions-preferencies-insatisfetes)) do
                        (bind ?justificacio (nth$ ?i ?self:justificacions-preferencies-insatisfetes))
                        (printout t ?justificacio crlf)
                    )
                else
                    (if (> ?self:nombre-extres 0) 
                        then
                            (printout t "L'oferta és molt recomanable" crlf)
                            (printout t crlf)
                            (send ?self imprimir-extres)
                        else (printout t "L'oferta és adequada" crlf)
                    )
            )
    )
    (printout t crlf)
    (printout t "-----------------------------------------------------------" crlf)
    (printout t crlf)
)

;;*************************
;;*  ElementLocalitzable  *
;;*************************

(defmessage-handler MAIN::ElementLocalitzable distancia (?latitud ?longitud)
    (bind ?latitud2 (send ?self get-latitud))
    (bind ?longitud2 (send ?self get-longitud))

    (bind ?rad-lat1 (/ ?latitud 57.29577951))
    (bind ?rad-lat2 (/ ?latitud2 57.29577951))

    (bind ?rad-lon1 (/ ?longitud 57.29577951))
    (bind ?rad-lon2 (/ ?longitud2 57.29577951))
    (bind ?distancia (* 3963.0 (acos (+ (* (sin ?rad-lat1) (sin ?rad-lat2)) (* (cos ?rad-lat1) (* (cos ?rad-lat2) (cos (- ?rad-lon2 ?rad-lon1))))))))
    (bind ?distancia (* 1609.344 ?distancia))
    ?distancia
)