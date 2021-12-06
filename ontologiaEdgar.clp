;;; ---------------------------------------------------------
;;; ontologia-turtle.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologia-turtle.owl
;;; :Date 02/12/2021 18:36:14

(defclass ElementLocalitzable
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (single-slot latitud
        (type FLOAT)
        (create-accessor read-write))
    (single-slot longitud
        (type FLOAT)
        (create-accessor read-write))
)

(defclass Habitatge
    (is-a ElementLocalitzable)
    (role concrete)
    (pattern-match reactive)
    (single-slot estat_de_l_obra
        (type STRING)
        (create-accessor read-write))
    (single-slot nombre_d_habitants_maxim
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_banys
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_dormitoris
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_dormitoris_dobles
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_dormitoris_simples
        (type INTEGER)
        (create-accessor read-write))
    (single-slot piscina
        (type STRING)
        (create-accessor read-write))
    (single-slot sol
        (type STRING)
        (create-accessor read-write))
    (single-slot superficie_habitable
        (type FLOAT)
        (create-accessor read-write))
    (single-slot te_aire_condicionat
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot te_ascensor
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot te_balco
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot te_bones_vistes
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot te_calefaccio
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot te_jardi
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot te_terrassa
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot te_traster
        (type SYMBOL)
        (create-accessor read-write))
)

(defclass HabitatgeCol·lectiu
    (is-a Habitatge)
    (role concrete)
    (pattern-match reactive)
    (single-slot planta
        (type INTEGER)
        (create-accessor read-write))
)

(defclass HabitatgeUnifamiliar
    (is-a Habitatge)
    (role concrete)
    (pattern-match reactive)
)

(defclass LlocDOcupacio
    (is-a ElementLocalitzable)
    (role concrete)
    (pattern-match reactive)
)

(defclass PuntDInteres
    (is-a ElementLocalitzable)
    (role concrete)
    (pattern-match reactive)
    (single-slot categoria
        (type STRING)
        (create-accessor read-write))
)

(defclass Oferta
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (single-slot ofereix_a
        (type INSTANCE)
        (create-accessor read-write))
    (single-slot admet_mascotes
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot es_per_compartir
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot inclou_electrodomestics
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot inclou_mobles
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot numero_de_places_de_garatge
        (type INTEGER)
        (create-accessor read-write))
    (single-slot preu
        (type FLOAT)
        (create-accessor read-write))
)

(defclass Peticio
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (single-slot demanada_per
        (type INSTANCE)
        (create-accessor read-write))
    (single-slot respecte_a
        (type INSTANCE)
        (create-accessor read-write))
    (single-slot distancia
        (type STRING)
        (create-accessor read-write))
    (single-slot grau_de_peticio
        (type STRING)
        (create-accessor read-write))
)

(defclass Recomanacio
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (single-slot dirigida_a
        (type INSTANCE)
        (create-accessor read-write))
    (single-slot recomana_a
        (type INSTANCE)
        (create-accessor read-write))
    (multislot caracteristiques_addicionals
        (type STRING)
        (create-accessor read-write))
    (multislot caracteristiques_incomplertes
        (type STRING)
        (create-accessor read-write))
    (single-slot grau_de_recomanacio
        (type STRING)
        (create-accessor read-write))
)

(defclass Sol·licitant
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (multislot es_desplaça_a
        (type INSTANCE)
        (create-accessor read-write))
    (single-slot accepta_compartir
        (type SYMBOL)
        (create-accessor read-write))
    (multislot accepta_estats_d_obra
        (type STRING)
        (create-accessor read-write))
    (single-slot accepta_habitatge_unifamiliar
        (type SYMBOL)
        (create-accessor read-write))
    (multislot accepta_plantes
        (type INTEGER)
        (create-accessor read-write))
    (multislot edats
        (type INTEGER)
        (create-accessor read-write))
    (single-slot marge_preu_maxim
        (type FLOAT)
        (create-accessor read-write))
    (single-slot nombre_d_adults
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_d_ancians
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_d_infants_i_adolescents
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_dormitoris_dobles_maxim
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_dormitoris_dobles_minim
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_dormitoris_maxim
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_dormitoris_minim
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_dormitoris_simples_maxim
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_dormitoris_simples_minim
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_joves
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_persones
        (type INTEGER)
        (create-accessor read-write))
    (single-slot nombre_de_recomanacions
        (type INTEGER)
        (create-accessor read-write))
    (single-slot preu_maxim
        (type FLOAT)
        (create-accessor read-write))
    (single-slot preu_minim
        (type FLOAT)
        (create-accessor read-write))
    (single-slot superficie_habitable_maxima
        (type FLOAT)
        (create-accessor read-write))
    (single-slot superficie_habitable_minima
        (type FLOAT)
        (create-accessor read-write))
    (single-slot te_cotxe
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot tipologia
        (type STRING)
        (create-accessor read-write))
    (single-slot vol_ascensor
        (type SYMBOL)
        (create-accessor read-write))
    (single-slot vol_traster
        (type SYMBOL)
        (create-accessor read-write))
)

;;; ---------------------------------------------------------
;;; ontologia.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologia.owl
;;; :Date 06/12/2021 14:40:02

(definstances instances
    ([Habitatge1] of HabitatgeCol·lectiu
         (nombre_de_banys  1)
         (te_terrassa  "false")
         (te_balco  "false")
         (te_calefaccio  "false")
         (piscina  "false")
         (te_ascensor  "false")
         (nombre_de_dormitoris_simples  0)
         (estat_de_l_obra  "bon estat")
         (te_aire_condicionat  "false")
         (te_bones_vistes  "false")
         (nombre_de_dormitoris  3)
         (nombre_d_habitants_maxim  6)
         (nombre_de_dormitoris_dobles  3)
         (sol  "tot el dia")
         (te_jardi  "false")
         (superficie_habitable  70.0)
         (te_traster  "false")
         (longitud  2.1658943641302772)
         (latitud  41.37864625856456)
    )

    ([ont2] of Oferta
         (preu  2500.0)
         (admet_mascotes  "false")
         (inclou_mobles "false")
         (inclou_electrodomestics  "false")
         (ofereix_a  [Habitatge1])
    )

    ([ont3] of Oferta
         (admet_mascotes  "false")
         (es_per_compartir  "false")
         (ofereix_a  [Habitatge3])
    )

    ([Habitatge3] of HabitatgeCol·lectiu
         (nombre_de_banys  1)
         (te_terrassa  "true")
         (te_balco  "true")
         (te_calefaccio  "true")
         (piscina  "false")
         (te_ascensor  "true")
         (nombre_de_dormitoris_simples  2)
         (estat_de_l_obra  "bon estat")
         (te_aire_condicionat  "false")
         (te_bones_vistes  "false")
         (nombre_de_dormitoris  3)
         (nombre_d_habitants_maxim  4)
         (nombre_de_dormitoris_dobles  1)
         (sol  "tot el dia")
         (te_jardi  "false")
         (superficie_habitable  72.0)
         (te_traster  "false")
         (longitud  2.1416599332274124)
         (latitud  41.38327533040126)
    )

    ([Habitatge2] of HabitatgeCol·lectiu
         (nombre_de_banys  2)
         (te_terrassa  "true")
         (te_balco  "true")
         (te_calefaccio  "true")
         (piscina  "false")
         (te_ascensor  "true")
         (nombre_de_dormitoris_simples  1)
         (estat_de_l_obra  "bon estat")
         (te_aire_condicionat  "false")
         (te_bones_vistes  "true")
         (nombre_de_dormitoris  3)
         (nombre_d_habitants_maxim  5)
         (nombre_de_dormitoris_dobles  2)
         (sol  "tarda")
         (te_jardi  "false")
         (superficie_habitable  137.0)
         (te_traster  "false")
         (longitud  2.124258404303795)
         (latitud  41.38703480006654)
    )

    ([ont6] of Oferta
         (ofereix_a  [Habitatge8])
    )

    ([ont7] of Oferta
         (ofereix_a  [Habitatge6])
    )

    ([ont8] of Oferta
         (ofereix_a  [Habitatge9])
    )

    ([ont9] of Oferta
         (ofereix_a  [Habitatge4])
    )

    ([ont10] of Oferta
         (ofereix_a  [Habitatge7])
    )

    ([Habitatge4] of HabitatgeCol·lectiu
         (planta  6)
         (nombre_de_banys  3)
         (te_terrassa  "false")
         (te_balco  "false")
         (te_calefaccio  "true")
         (piscina  "false")
         (te_ascensor  "true")
         (nombre_de_dormitoris_simples  0)
         (estat_de_l_obra  "bon estat")
         (te_aire_condicionat  "true")
         (te_bones_vistes  "true")
         (nombre_de_dormitoris  4)
         (nombre_d_habitants_maxim  8)
         (nombre_de_dormitoris_dobles  4)
         (sol  "tot el dia")
         (te_jardi  "false")
         (superficie_habitable  172.0)
         (te_traster  "false")
         (longitud  2.138963122983205)
         (latitud  41.40677437417774)
    )

    ([Habitatge5] of HabitatgeCol·lectiu
         (nombre_de_banys  2)
         (te_terrassa  "false")
         (te_balco  "true")
         (te_calefaccio  "true")
         (piscina  "false")
         (te_ascensor  "true")
         (nombre_de_dormitoris_simples  3)
         (estat_de_l_obra  "bon estat")
         (te_aire_condicionat  "true")
         (te_bones_vistes  "true")
         (nombre_de_dormitoris  4)
         (nombre_d_habitants_maxim  5)
         (nombre_de_dormitoris_dobles  1)
         (sol  "tot el dia")
         (te_jardi  "false")
         (superficie_habitable  115.0)
         (te_traster  "true")
         (longitud  2.1283853672138053)
         (latitud  41.38279885132334)
    )

    ([Habitatge6] of HabitatgeUnifamiliar
         (nombre_de_banys  4)
         (te_terrassa  "true")
         (te_balco  "false")
         (te_calefaccio  "true")
         (piscina  "true")
         (te_ascensor  "false")
         (nombre_de_dormitoris_simples  0)
         (estat_de_l_obra  "bon estat")
         (te_aire_condicionat  "true")
         (te_bones_vistes  "false")
         (nombre_de_dormitoris  3)
         (nombre_d_habitants_maxim  5)
         (nombre_de_dormitoris_dobles  3)
         (sol  "tot el dia")
         (te_jardi  "true")
         (superficie_habitable  178.0)
         (te_traster  "false")
         (longitud  2.146680444023863)
         (latitud  41.40743842574425)
    )

    ([ont14] of Oferta
         (preu  2100.0)
         (numero_de_places_de_garatge  0)
         (admet_mascotes  "false")
         (es_per_compartir  "false")
         (inclou_mobles  "false")
         (inclou_electrodomestics  "false")
         (ofereix_a  [Habitatge2])
    )

    ([ont15] of Oferta
         (ofereix_a  [Habitatge5])
    )

    ([Habitatge7] of HabitatgeCol·lectiu
         (planta  3)
         (nombre_de_banys  3)
         (te_terrassa  "true")
         (te_balco  "true")
         (te_calefaccio  "true")
         (piscina  "false")
         (te_ascensor  "true")
         (nombre_de_dormitoris_simples  2)
         (estat_de_l_obra  "bon estat")
         (te_aire_condicionat  "true")
         (te_bones_vistes  "true")
         (nombre_de_dormitoris  5)
         (nombre_d_habitants_maxim  8)
         (nombre_de_dormitoris_dobles  3)
         (sol  "tot el dia")
         (te_jardi  "false")
         (superficie_habitable  160.0)
         (te_traster  "true")
         (longitud  2.1343039618017734)
         (latitud  41.40252668742339)
    )

    ([Habitatge8] of HabitatgeCol·lectiu
         (nombre_de_banys  1)
         (te_terrassa  "true")
         (te_balco  "true")
         (te_calefaccio  "true")
         (piscina  "false")
         (te_ascensor  "true")
         (nombre_de_dormitoris_simples  0)
         (estat_de_l_obra  "bon estat")
         (te_aire_condicionat  "true")
         (te_bones_vistes  "true")
         (nombre_de_dormitoris  2)
         (nombre_d_habitants_maxim  4)
         (nombre_de_dormitoris_dobles  2)
         (sol  "tot el dia")
         (te_jardi  "false")
         (superficie_habitable  72.0)
         (te_traster  "false")
         (longitud  2.17467876673636)
         (latitud  41.38356267816411)
    )

    ([Habitatge9] of HabitatgeCol·lectiu
         (nombre_de_banys  1)
         (te_terrassa  "false")
         (te_balco  "true")
         (te_calefaccio  "false")
         (piscina  "false")
         (te_ascensor  "true")
         (nombre_de_dormitoris_simples  2)
         (estat_de_l_obra  "bon estat")
         (te_aire_condicionat  "true")
         (te_bones_vistes  "false")
         (nombre_de_dormitoris  4)
         (nombre_d_habitants_maxim  6)
         (nombre_de_dormitoris_dobles  2)
         (sol  "tarda")
         (te_jardi  "false")
         (superficie_habitable  87.0)
         (te_traster  "false")
         (longitud  2.158074688562444)
         (latitud  41.37893274451925)
    )

)
