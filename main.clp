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

;;**********************
;;*    DEFCLASSES    *
;;**********************

;;****************************
;;* DEFFUNCTIONS - PREGUNTES *
;;****************************

(deffunction fes-una-pregunta (?pregunta $?valors-permesos)
    (printout t ?pregunta)
    (bind ?resposta (read))
    (if (lexemep ?resposta)
        then (bind ?resposta (lowcase ?resposta)))
    (while (not (member ?resposta ?valors-permesos)) do
        (printout t ?pregunta)
        (bind ?resposta (read))
        (if (lexemep ?resposta)
            then (bind ?resposta (lowcase ?resposta))))
    ?resposta)

(deffunction fes-una-pregunta-si-o-no (?pregunta)
   (bind ?resposta (fes-una-pregunta ?pregunta si no s n))
   (if (or (eq ?resposta si) (eq ?resposta s))
       then TRUE
       else FALSE))

(deffunction fes-una-pregunta-numerica (?pregunta ?cota-inferior ?cota-superior)
    (format t "%s [%d, %d] " ?pregunta ?cota-inferior ?cota-superior)
    (bind ?resposta (read))
    (while (not (and (>= ?resposta ?cota-inferior) (<= ?resposta ?cota-superior))) do
        (format t "�%s? [%d, %d] " ?pregunta ?cota-inferior ?cota-superior)
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
)

(defrule preguntes::passar-a-seleccio "Passa al modul de seleccio"
    (declare (salience -10))
    =>
    (printout t "Seleccionant ofertes candidates..." crlf)c
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

(deffacts construccio
)

(defrule construccio::passar-a-presentacio
    (declare (salience -100))
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

(defrule presentacio::mostrar-resultats "Mostra les recomanacions obtingudes"
    =>
)

;;**********************
;;*  MESSAGE HANDLERS  *
;;**********************