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

;;**********************
;;*    DEFTEMPLATES    *
;;**********************

; Restriccions del sol·licitant
(deftemplate MAIN::restriccions
    (slot superficie-habitable-minima (type FLOAT))
)

; Preferencies del sol·licitant
(deftemplate MAIN::preferencies
)

; Recomanacions pel sol·licitant
(deftemplate MAIN::recomanacio
    (slot oferta 
        (type INSTANCE))
    (slot puntuacio
        (type INTEGER))
    (multislot justificacions
        (type STRING))
)

(deftemplate MAIN::llista-recomanacions
    (multislot recomanacions (type INSTANCE))
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

;;*****************
;;*      MAIN     *
;;*****************

(defmodule MAIN (export ?ALL))

(defrule initial_rule "Regla inicial"
    (initial-fact)
    =>
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

(defmodule preguntes
    (import MAIN ?ALL)
    (export ?ALL)
)

(deffacts dades
    (preferencies)
    (restriccions)
    (llista-recomanacions)
    (superficie-habitable-minima preguntar)
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

(defrule preguntes::passar-a-seleccio "Passa al modul de seleccio"
    (declare (salience -10))
    =>
    (printout t "Seleccionant ofertes candidates..." crlf)
    (focus seleccio)
)

;;***********************
;;*  MODUL DE SELECCIO  *
;;***********************

(defmodule seleccio
    (import MAIN ?ALL)
    (export ?ALL)
)

(deffacts seleccio
    (iniciar-seleccio)
)

(defrule seleccio::afegir-ofertes "Afegir totes les ofertes"
    (declare (salience 10))
    ?fet <- (iniciar-seleccio)
    ?llista-recomanacions <- (llista-recomanacions (recomanacions ?recomanacions))
    =>
    (bind $?llista (find-all-instances ((?instancia Oferta)) TRUE))
    (format t "Ofertes considerades: %d" (length$ llista) crlf)
    (retract ?fet)
)

(defrule seleccio::passar-a-construccio
    (declare (salience -10))
    =>
    (printout t "Generant resultats..." crlf)
    (focus construccio)
)

;;**************************
;;*  MODUL DE CONSTRUCCIO  *
;;**************************

(defmodule construccio
    (import MAIN ?ALL)
    (export ?ALL)
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

(defmodule presentacio
    (import MAIN ?ALL)
    (export ?ALL)
)

(defrule presentacio::mostrar-recomanacions
    =>
    (printout t crlf)
    (printout t "Et recomano aquestes ofertes:" crlf)
    (bind ?i 1)
)

;;**********************
;;*  MESSAGE HANDLERS  *
;;**********************