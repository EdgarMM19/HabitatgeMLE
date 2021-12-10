;;; ---------------------------------------------------------
;;; ontologia.clp
;;; Translated by owl2clips
;;; Translated to CLIPS from ontology ontologia.owl
;;; :Date 10/12/2021 12:12:59

(defclass ElementLocalitzable
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot latitud
        (type FLOAT)
        (create-accessor read-write))
    (slot longitud
        (type FLOAT)
        (create-accessor read-write))
)

(defclass Habitatge
    (is-a ElementLocalitzable)
    (role concrete)
    (pattern-match reactive)
    (slot estat_de_l_obra
        (type STRING)
        (create-accessor read-write))
    (slot nombre_d_habitants_maxim
        (type INTEGER)
        (create-accessor read-write))
    (slot nombre_de_banys
        (type INTEGER)
        (create-accessor read-write))
    (slot nombre_de_dormitoris
        (type INTEGER)
        (create-accessor read-write))
    (slot nombre_de_dormitoris_dobles
        (type INTEGER)
        (create-accessor read-write))
    (slot nombre_de_dormitoris_simples
        (type INTEGER)
        (create-accessor read-write))
    (slot piscina
        (type STRING)
        (create-accessor read-write))
    (slot sol
        (type STRING)
        (create-accessor read-write))
    (slot superficie_habitable
        (type FLOAT)
        (create-accessor read-write))
    (slot te_aire_condicionat
        (type SYMBOL)
        (create-accessor read-write))
    (slot te_ascensor
        (type SYMBOL)
        (create-accessor read-write))
    (slot te_balco
        (type SYMBOL)
        (create-accessor read-write))
    (slot te_bones_vistes
        (type SYMBOL)
        (create-accessor read-write))
    (slot te_calefaccio
        (type SYMBOL)
        (create-accessor read-write))
    (slot te_jardi
        (type SYMBOL)
        (create-accessor read-write))
    (slot te_terrassa
        (type SYMBOL)
        (create-accessor read-write))
    (slot te_traster
        (type SYMBOL)
        (create-accessor read-write))
)

(defclass HabitatgeCol·lectiu
    (is-a Habitatge)
    (role concrete)
    (pattern-match reactive)
    (slot planta
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
    (slot categoria
        (type STRING)
        (create-accessor read-write))
)

(defclass Oferta
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    (slot ofereix_a
        (type INSTANCE)
        (create-accessor read-write))
    (slot admet_mascotes
        (type SYMBOL)
        (create-accessor read-write))
    (slot es_per_compartir
        (type SYMBOL)
        (create-accessor read-write))
    (slot inclou_electrodomestics
        (type SYMBOL)
        (create-accessor read-write))
    (slot inclou_mobles
        (type SYMBOL)
        (create-accessor read-write))
    (slot numero_de_places_de_garatge
        (type INTEGER)
        (create-accessor read-write))
    (slot preu
        (type FLOAT)
        (create-accessor read-write))
)

(definstances instances
    ([Caprabo] of PuntDInteres
         (categoria  "Hipermercat")
         (latitud  41.39098387985266)
         (longitud  2.134638732335127)
    )

    ([CentreComercialIlla] of PuntDInteres
         (categoria  "Zona comercial")
         (latitud  41.39098387985266)
         (longitud  2.134638732335127)
    )

    ([CentreComercialMaremagnum] of PuntDInteres
         (categoria  "Zona comercial")
         (latitud  41.37632103480868)
         (longitud  2.18246360236497)
    )

    ([CentreEsportsUPC] of PuntDInteres
         (categoria  "Zona esportiva")
         (latitud  41.38820905065717)
         (longitud  2.1133108271820897)
    )

    ([ClubTennisBarcino] of PuntDInteres
         (categoria  "Zona esportiva")
         (latitud  41.40954253025416)
         (longitud  2.1385854758277993)
    )

    ([FerrocarrilPutxet] of PuntDInteres
         (categoria  "Parada de transport public")
         (latitud  41.406132181200626)
         (longitud  2.1389102620469758)
    )

    ([Habitatge1] of HabitatgeCol·lectiu
         (planta  6)
         (estat_de_l_obra  "bon estat")
         (nombre_d_habitants_maxim  8)
         (nombre_de_banys  3)
         (nombre_de_dormitoris  4)
         (nombre_de_dormitoris_dobles  4)
         (nombre_de_dormitoris_simples  0)
         (piscina  "false")
         (sol  "tot el dia")
         (superficie_habitable  172.0)
         (te_aire_condicionat  "true")
         (te_ascensor  "true")
         (te_balco  "false")
         (te_bones_vistes  "true")
         (te_calefaccio  "true")
         (te_jardi  "false")
         (te_terrassa  "false")
         (te_traster  "false")
         (latitud  41.40677437417774)
         (longitud  2.138963122983205)
    )

    ([Habitatge2] of HabitatgeCol·lectiu
         (planta  3)
         (estat_de_l_obra  "bon estat")
         (nombre_d_habitants_maxim  8)
         (nombre_de_banys  3)
         (nombre_de_dormitoris  5)
         (nombre_de_dormitoris_dobles  3)
         (nombre_de_dormitoris_simples  2)
         (piscina  "false")
         (sol  "tot el dia")
         (superficie_habitable  160.0)
         (te_aire_condicionat  "true")
         (te_ascensor  "true")
         (te_balco  "true")
         (te_bones_vistes  "true")
         (te_calefaccio  "true")
         (te_jardi  "false")
         (te_terrassa  "true")
         (te_traster  "true")
         (latitud  41.40252668742339)
         (longitud  2.1343039618017734)
    )

    ([Habitatge3] of HabitatgeCol·lectiu
         (estat_de_l_obra  "bon estat")
         (nombre_d_habitants_maxim  6)
         (nombre_de_banys  1)
         (nombre_de_dormitoris  4)
         (nombre_de_dormitoris_dobles  2)
         (nombre_de_dormitoris_simples  2)
         (piscina  "false")
         (sol  "tarda")
         (superficie_habitable  87.0)
         (te_aire_condicionat  "true")
         (te_ascensor  "true")
         (te_balco  "true")
         (te_bones_vistes  "false")
         (te_calefaccio  "false")
         (te_jardi  "false")
         (te_terrassa  "false")
         (te_traster  "false")
         (latitud  41.37893274451925)
         (longitud  2.158074688562444)
    )

    ([Habitatge4] of HabitatgeCol·lectiu
         (estat_de_l_obra  "bon estat")
         (nombre_d_habitants_maxim  6)
         (nombre_de_banys  1)
         (nombre_de_dormitoris  3)
         (nombre_de_dormitoris_dobles  3)
         (nombre_de_dormitoris_simples  0)
         (piscina  "false")
         (sol  "tot el dia")
         (superficie_habitable  70.0)
         (te_aire_condicionat  "false")
         (te_ascensor  "false")
         (te_balco  "false")
         (te_bones_vistes  "false")
         (te_calefaccio  "false")
         (te_jardi  "false")
         (te_terrassa  "false")
         (te_traster  "false")
         (latitud  41.37864625856456)
         (longitud  2.1658943641302772)
    )

    ([Habitatge5] of HabitatgeCol·lectiu
         (estat_de_l_obra  "bon estat")
         (nombre_d_habitants_maxim  4)
         (nombre_de_banys  1)
         (nombre_de_dormitoris  2)
         (nombre_de_dormitoris_dobles  2)
         (nombre_de_dormitoris_simples  0)
         (piscina  "false")
         (sol  "tot el dia")
         (superficie_habitable  72.0)
         (te_aire_condicionat  "true")
         (te_ascensor  "true")
         (te_balco  "true")
         (te_bones_vistes  "true")
         (te_calefaccio  "true")
         (te_jardi  "false")
         (te_terrassa  "true")
         (te_traster  "false")
         (latitud  41.38356267816411)
         (longitud  2.17467876673636)
    )

    ([Habitatge6] of HabitatgeCol·lectiu
         (estat_de_l_obra  "bon estat")
         (nombre_d_habitants_maxim  4)
         (nombre_de_banys  1)
         (nombre_de_dormitoris  3)
         (nombre_de_dormitoris_dobles  1)
         (nombre_de_dormitoris_simples  2)
         (piscina  "false")
         (sol  "tot el dia")
         (superficie_habitable  72.0)
         (te_aire_condicionat  "false")
         (te_ascensor  "true")
         (te_balco  "true")
         (te_bones_vistes  "false")
         (te_calefaccio  "true")
         (te_jardi  "false")
         (te_terrassa  "true")
         (te_traster  "false")
         (latitud  41.38327533040126)
         (longitud  2.1416599332274124)
    )

    ([Habitatge7] of HabitatgeCol·lectiu
         (estat_de_l_obra  "bon estat")
         (nombre_d_habitants_maxim  5)
         (nombre_de_banys  2)
         (nombre_de_dormitoris  4)
         (nombre_de_dormitoris_dobles  1)
         (nombre_de_dormitoris_simples  3)
         (piscina  "false")
         (sol  "tot el dia")
         (superficie_habitable  115.0)
         (te_aire_condicionat  "true")
         (te_ascensor  "true")
         (te_balco  "true")
         (te_bones_vistes  "true")
         (te_calefaccio  "true")
         (te_jardi  "false")
         (te_terrassa  "false")
         (te_traster  "true")
         (latitud  41.38279885132334)
         (longitud  2.1283853672138053)
    )

    ([Habitatge8] of HabitatgeCol·lectiu
         (estat_de_l_obra  "bon estat")
         (nombre_d_habitants_maxim  5)
         (nombre_de_banys  2)
         (nombre_de_dormitoris  3)
         (nombre_de_dormitoris_dobles  2)
         (nombre_de_dormitoris_simples  1)
         (piscina  "false")
         (sol  "tarda")
         (superficie_habitable  137.0)
         (te_aire_condicionat  "false")
         (te_ascensor  "true")
         (te_balco  "true")
         (te_bones_vistes  "true")
         (te_calefaccio  "true")
         (te_jardi  "false")
         (te_terrassa  "true")
         (te_traster  "false")
         (latitud  41.38703480006654)
         (longitud  2.124258404303795)
    )

    ([Habitatge9] of HabitatgeUnifamiliar
         (estat_de_l_obra  "bon estat")
         (nombre_d_habitants_maxim  5)
         (nombre_de_banys  4)
         (nombre_de_dormitoris  3)
         (nombre_de_dormitoris_dobles  3)
         (nombre_de_dormitoris_simples  0)
         (piscina  "true")
         (sol  "tot el dia")
         (superficie_habitable  178.0)
         (te_aire_condicionat  "true")
         (te_ascensor  "false")
         (te_balco  "false")
         (te_bones_vistes  "false")
         (te_calefaccio  "true")
         (te_jardi  "true")
         (te_terrassa  "true")
         (te_traster  "false")
         (latitud  41.40743842574425)
         (longitud  2.146680444023863)
    )

    ([HospitalDexeus] of PuntDInteres
         (categoria  "Centre de salut")
         (latitud  41.38525786997681)
         (longitud  2.125472266722365)
    )

    ([HospitalSagratCor] of PuntDInteres
         (categoria  "Centre de salut")
         (latitud  41.389264945151496)
         (longitud  2.1457533617420514)
    )

    ([HospitalTeknon] of PuntDInteres
         (categoria  "Centre de salut")
         (latitud  41.40705245175463)
         (longitud  2.1272901328719946)
    )

    ([JardinsMagali] of PuntDInteres
         (categoria  "Zona verda")
         (latitud  41.385068340034614)
         (longitud  2.133808446347894)
    )

    ([JardinsPalauPedralbes] of PuntDInteres
         (categoria  "Zona verda")
         (latitud  41.38750511146732)
         (longitud  2.1188321929080898)
    )

    ([Lidl] of PuntDInteres
         (categoria  "Hipermercat")
         (latitud  41.38284809100044)
         (longitud  2.141567228250443)
    )

    ([Mercadona] of PuntDInteres
         (categoria  "Hipermercat")
         (latitud  41.40672765187097)
         (longitud  2.134579779124623)
    )

    ([MercatBoqueria] of PuntDInteres
         (categoria  "Supermercat")
         (latitud  41.38224121493596)
         (longitud  2.172049963491897)
    )

    ([MercatSantAntoni] of PuntDInteres
         (categoria  "Supermercat")
         (latitud  41.37909903259165)
         (longitud  2.161975181226727)
    )

    ([MetroJaumeI] of PuntDInteres
         (categoria  "Parada de transport public")
         (latitud  41.3839033224606)
         (longitud  2.1786349868399144)
    )

    ([MetroLesCorts] of PuntDInteres
         (categoria  "Parada de transport public")
         (latitud  41.38428313867461)
         (longitud  2.1308131956599334)
    )

    ([MetroLesseps] of PuntDInteres
         (categoria  "Parada de transport public")
         (latitud  41.40653818452237)
         (longitud  2.1499169061412893)
    )

    ([MetroMariaCristina] of PuntDInteres
         (categoria  "Parada de transport public")
         (latitud  41.3878570819902)
         (longitud  2.126121839674454)
    )

    ([MetroSantAntoni] of PuntDInteres
         (categoria  "Parada de transport public")
         (latitud  41.38026029157868)
         (longitud  2.1631586646855876)
    )

    ([Oferta1] of Oferta
         (ofereix_a  [Habitatge1])
         (admet_mascotes  "false")
         (es_per_compartir  "false")
         (inclou_electrodomestics  "false")
         (inclou_mobles  "false")
         (numero_de_places_de_garatge  1)
         (preu  2500.0)
    )

    ([Oferta2] of Oferta
         (ofereix_a  [Habitatge2])
         (admet_mascotes  "false")
         (es_per_compartir  "false")
         (inclou_electrodomestics  "false")
         (inclou_mobles  "false")
         (numero_de_places_de_garatge  0)
         (preu  2100.0)
    )

    ([Oferta3] of Oferta
         (ofereix_a  [Habitatge3])
         (admet_mascotes  "false")
         (es_per_compartir  "false")
         (inclou_electrodomestics  "true")
         (inclou_mobles  "false")
         (numero_de_places_de_garatge  0)
         (preu  1150.0)
    )

    ([Oferta4] of Oferta
         (ofereix_a  [Habitatge4])
         (admet_mascotes  "true")
         (es_per_compartir  "false")
         (inclou_electrodomestics  "true")
         (inclou_mobles  "true")
         (numero_de_places_de_garatge  0)
         (preu  937.0)
    )

    ([Oferta5] of Oferta
         (ofereix_a  [Habitatge5])
         (admet_mascotes  "true")
         (es_per_compartir  "false")
         (inclou_electrodomestics  "true")
         (inclou_mobles  "true")
         (numero_de_places_de_garatge  0)
         (preu  1350.0)
    )

    ([Oferta6] of Oferta
         (ofereix_a  [Habitatge6])
         (admet_mascotes  "true")
         (es_per_compartir  "false")
         (inclou_electrodomestics  "true")
         (inclou_mobles  "false")
         (numero_de_places_de_garatge  0)
         (preu  1030.0)
    )

    ([Oferta7] of Oferta
         (ofereix_a  [Habitatge7])
         (admet_mascotes  "true")
         (es_per_compartir  "false")
         (inclou_electrodomestics  "false")
         (inclou_mobles  "false")
         (numero_de_places_de_garatge  0)
         (preu  1500.0)
    )

    ([Oferta8] of Oferta
         (ofereix_a  [Habitatge8])
         (admet_mascotes  "false")
         (es_per_compartir  "false")
         (inclou_electrodomestics  "true")
         (inclou_mobles  "true")
         (numero_de_places_de_garatge  0)
         (preu  1750.0)
    )

    ([Oferta9] of Oferta
         (ofereix_a  [Habitatge9])
         (admet_mascotes  "true")
         (es_per_compartir  "false")
         (inclou_electrodomestics  "false")
         (inclou_mobles  "false")
         (numero_de_places_de_garatge  2)
         (preu  2990.0)
    )

    ([SalaApolo] of PuntDInteres
         (categoria  "Zona d'oci nocturn")
         (latitud  41.375592360009804)
         (longitud  2.171296373830083)
    )

    ([SantsEstacio] of PuntDInteres
         (categoria  "Parada de transport public")
         (latitud  41.38119639315609)
         (longitud  2.1416033156081298)
    )

)
