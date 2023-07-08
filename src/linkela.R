library(readxl)
library(tidyverse)

source("src/common.r")

linkela_users_path <- "data/linkela/Users.xlsx"
linkela_alsfrs_path <- "data/linkela/form_38.xlsx"
linkela_eat10_path <- "data/linkela/form_103.xlsx"
linkela_roads_path <- "data/linkela/form_108.xlsx"

linkela_analytics_path <- "data/linkela/Analytics.xlsx"
linkela_alarms_path <- "data/linkela/Alarms.xlsx"

linkela_enabled_forms <- c(35, 38, 103, 108)

linkela_parse_bool <- function(xs) {
    xs %>% case_match(
        "Yes" ~ TRUE,
        "No" ~ FALSE
    )
}

linkela_usuarios <- read_excel(linkela_users_path, na = "NULL") %>%
    normalize_colnames() %>%
    mutate(
        full_name = str_c(last_name, first_name, sep = ", "),
        birthdate = parse_date(birthdate),
        tax_number = str_replace(tax_number, "-", ""),
        last_login_at = parse_datetime(last_login_at),
        gender = factor(gender, labels = c("M", "F")),
        across(c(
            cuidador, hospital_del_mar, prueba_piloto,
            hospital_de_bellvitge, fundacion_miquel_valls
        ), linkela_parse_bool)
    )

linkela_alsfrs <- read_excel(linkela_alsfrs_path, na = "NULL") |>
    select(-starts_with("Pregunta sin etiqueta"), -starts_with("Formula MiToS graded")) %>%
    rename_with(~ str_replace_all(.x, "</?[A-Za-z]+>", "")) %>%
    rename_with(~ str_replace_all(.x, "&nbsp;", " ")) %>%
    normalize_colnames() %>%
    rename_with(~ str_replace(.x, "^x([0-9])", "q\\1")) %>%
    rename(
        q5a_cortar_nopeg = "q5a_cortado_de_comida_y_uso_de_utensilios_pacientes_sin_gastrostomia",
        q5b_cortar_peg = "q5b_cortar_comida_y_manejo_de_utensilios_alternativo_para_pacientes_con_gastrostomia",
        q6_vestido = "q6_vestido_e_higiene",
        q7_girarse = "q7_girarse_en_la_cama_y_ajustarse_la_ropa_de_la_cama",
        q8_caminar = "q8_andar",
        q9_escaleras = "q9_subir_escaleras",
        q10_disnea = "q10_disnea_sensacion_de_falta_de_aire",
        q11_ortopnea = "q11_ortopnea_falta_de_aire_estando_acostado",
        q12_insuf_resp = "q12_insuficiencia_respiratoria"
    ) %>%
    mutate(
        across(starts_with("data_"), ~ parse_datetime(.x, format = "%d/%m/%Y %H:%M")),
        q1_lenguaje = recode(q1_lenguaje,
            "Habla normal" = 4L,
            "Procesos del habla normales." = 4L,
            "Alteraciones en el habla detectables" = 3L,
            "Trastornos del habla detectables." = 3L,
            "Habla inteligible con repeticiones." = 2L,
            "Usa lenguaje verbal combinado con comunicación no verbal" = 1L,
            "Pérdida del habla útil." = 0L,
            "Sense resposta" = NA_integer_
        ),
        q2_salivacion = recode(q2_salivacion,
            "Normal." = 4L,
            "Aunque leve, definitivo exceso de saliva en la boca, puede haber sialorrea nocturna mínima." = 3L,
            "Exceso de saliva leve (pero claro) en boca; posible babeo nocturno" = 3L,
            "Exceso de saliva moderado; posible babeo mínimo" = 2L,
            "Exceso de saliva marcado con algo de babeo" = 1L,
            "Babeo marcado; que requiere uso de pañuelo constante" = 0L,
            "Sense resposta" = NA_integer_
        ),
        q3_tragar = recode(q3_tragar,
            "Hábitos de alimentación normales" = 4L,
            "Hábitos alimenticios normales." = 4L,
            "Problemas precoces para tragar (atragantamiento ocasional)" = 3L,
            "Problemas alimenticios tempranos, ahogamientos ocasionales." = 3L,
            "Precisa cambios en la consistencia de la dieta" = 2L,
            "Necesidad de alimentación suplementaria por sonda" = 1L,
            "Alimentación exclusiva por sonda" = 0L,
            "Sense resposta" = NA_integer_
        ),
        q4_escritura = recode(q4_escritura,
            "Normal." = 4L,
            "Lenta; pero todas las palabras son legibles" = 3L,
            "Un poco lenta y torpe, todas las palabras son legibles." = 3L,
            "No todas las palabras son legibles." = 2L,
            "Es capaz de sujetar el lápiz pero no es capaz de escribir" = 1L,
            "Incapaz de sostener el lápiz" = 0L,
            "Sense resposta" = NA_integer_
        ),
        q5a_cortar_nopeg = recode(q5a_cortar_nopeg,
            "Normal." = 4L,
            "Lento y torpe pero no precisa ayuda" = 3L,
            "Capaz de cortar la mayoría de los alimentos, torpe y lento; necesita alguna ayuda" = 2L,
            "Puede cortar la mayoría de las comidas, lento y torpe, requiere algo de ayuda." = 2L,
            "La comida requiere ser cortada por alguien más, aún puede alimentarse lentamente." = 1L,
            "Otra persona tiene que cortarle la comida, luego puede alimentarse lentamente" = 1L,
            "Precisa ser alimentado por otra persona" = 0L,
            "Sense resposta" = NA_integer_
        ),
        q5b_cortar_peg = recode(q5b_cortar_peg,
            "Normal." = 4L,
            "Torpe, puede manejar todos los utensilios." = 3L,
            "Lento y torpe pero capaz de realizar todas las manipulaciones de forma independiente" = 3L,
            "Requiere algo de ayuda con cierres y broches." = 2L,
            "Proporciona mínima ayuda al cuidador" = 1L,
            "Incapaz de realizar ningún aspecto de la tarea" = 0L,
            "Sense resposta" = NA_integer_
        ),
        q5_cortar = coalesce(q5a_cortar_nopeg, q5b_cortar_peg),
        q6_vestido = recode(q6_vestido,
            "Normal." = 4L,
            "Cuidado personal independiente y completo, pero con mayor esfuerzo" = 3L,
            "Precisa asistencia intermitente o el uso de métodos sustitutivos" = 2L,
            "Requiere ayuda intermitente o métodos sustitutos." = 2L,
            "Precisa ayuda para la mayor parte de las tareas" = 1L,
            "Requiere ayuda de cuidador para autocuidado." = 1L,
            "Dependencia completa" = 0L,
            "Sense resposta" = NA_integer_
        ),
        q7_girarse = recode(q7_girarse,
            "Normal." = 4L,
            "Algo lento y torpe, no necesita ayuda." = 3L,
            "Puede voltearse solo o ajustar las sábanas con dificultad." = 2L,
            "Puede girarse o ajustar sábanas solo, aunque con mucha dificultad" = 2L,
            "Puede iniciar el giro o el ajuste de las sábanas, pero no puede completarlo solo" = 1L,
            "Puede comenzar a voltearse sin terminar, no puede ajustar sábanas." = 1L,
            "Dependiente de otra persona" = 0L,
            "Sense resposta" = NA_integer_
        ),
        q8_caminar = recode(q8_caminar,
            "Normal." = 4L,
            "Dificultades incipientes para caminar" = 3L,
            "Dificultad temprana para la deambulación." = 3L,
            "Camina con ayuda" = 2L,
            "Puede caminar con ayuda." = 2L,
            "Puede realizar movimientos con piernas pero no puede caminar" = 1L,
            "No puede realizar movimiento voluntario alguno con las piernas" = 0L,
            "No hay movimiento voluntario de piernas." = 0L,
            "Sense resposta" = NA_integer_
        ),
        q9_escaleras = recode(q9_escaleras,
            "Normal" = 4L,
            "Lento." = 3L,
            "Lentamente" = 3L,
            "Leve inestabilidad o fatiga" = 2L,
            "Moderadamente inestable o fatiga." = 2L,
            "Necesita ayuda" = 1L,
            "Requiere ayuda." = 1L,
            "No puede." = 0L,
            "No puede hacerlo" = 0L,
            "Sense resposta" = NA_integer_
        ),
        q10_disnea = recode(q10_disnea,
            "Ninguna." = 4L,
            "No" = 4L,
            "Ocurre solo cuando camina" = 3L,
            "Ocurre con uno o más: comer, bañarse y vestirse." = 2L,
            "Ocurre en una o más de las siguientes actividades diarias: comer, asearse, vestirse..." = 2L,
            "Ocurre en reposo, dificultad respiratoria sentado o tumbado" = 1L,
            "Dificultad importante, se ha considerado el uso de soporte respiratorio o ventilatorio mecánico" = 0L,
            "Sense resposta" = NA_integer_
        ),
        q11_ortopnea = recode(q11_ortopnea,
            "Ninguna." = 4L,
            "Alguna dificultad para dormir por la noche. No necesita más de 2 almohadas" = 3L,
            "Necesita más de 2 almohadas para poder dormir" = 2L,
            "Requiere de almohadas extra para dormir (>2)" = 2L,
            "Solo puede dormir sentado" = 1L,
            "Incapaz de dormir por sensación de falta de aire" = 0L,
            "Sense resposta" = NA_integer_
        ),
        q12_insuf_resp = recode(q12_insuf_resp,
            "No" = 4L,
            "Ninguna." = 4L,
            "Uso intermitente de BiPAP." = 3L,
            "Uso continuo de BiPAP durante la noche" = 2L,
            "Uso continuo de BiPAP por las noches." = 2L,
            "Uso continuo de BiPAP, noche y día" = 1L,
            "Sense resposta" = NA_integer_
        )
    ) |>
    rowwise() |>
    mutate(
        total_bulbar = q1_lenguaje + q2_salivacion + q3_tragar,
        total_fmotor_nopeg = q4_escritura + q5a_cortar_nopeg + q6_vestido,
        total_fmotor_peg = q4_escritura + q5b_cortar_peg + q6_vestido,
        total_gmotor = q7_girarse + q8_caminar + q9_escaleras,
        total_resp = q10_disnea + q11_ortopnea + q12_insuf_resp,
        total_peg = total_bulbar + total_fmotor_peg + total_gmotor + total_resp,
        total_nopeg = total_bulbar + total_fmotor_nopeg + total_gmotor + total_resp
    )

linkela_roads <- read_excel(linkela_roads_path, na = "Sense resposta") %>%
    normalize_colnames() %>%
    rename_with(~ str_replace(.x, "^x([0-9])", "q\\1"))

linkela_eat10 <- read_excel(linkela_eat10_path, na = "NULL") %>%
    rename_with(~ str_replace_all(.x, "</?[A-Za-z]+>", "")) %>%
    normalize_colnames() %>%
    select(-starts_with("formula")) %>%
    rename(
        q1_perder_peso = "mi_problema_para_tragar_me_ha_llevado_a_perder_peso",
        q2_comer_fuera = "mi_problema_para_tragar_interfiere_con_mi_capacidad_para_comer_fuera_de_casa",
        q3_tragar_liquidos = "tragar_liquidos_me_supone_un_esfuerzo_extra",
        q4_tragar_solidos = "tragar_solidos_me_supone_un_esfuerzo_extra",
        q5_tragar_pastillas = "tragar_pastillas_me_supone_un_esfuerzo_extra",
        q6_dolor = "tragar_es_doloroso",
        q7_placer = "el_placer_de_comer_se_ve_afectado_por_mi_problema_para_tragar",
        q8_garganta = "cuando_trago_la_comida_se_pega_en_mi_garganta",
        q9_tos = "toso_cuando_como",
        q10_estres = "tragar_es_estresante"
    ) %>%
    mutate(
        across(matches("^q[0-9]{1,2}_"), ~ {
            .x <- as.integer(.x)
            if_else(.x |> between(0, 4), .x, NA_integer_)
        }),
        puntuacion_total = (
            q1_perder_peso + q2_comer_fuera + q3_tragar_liquidos +
                q4_tragar_solidos + q5_tragar_pastillas + q6_dolor +
                q7_placer + q8_garganta + q9_tos + q10_estres
        )
    )

linkela_logins <- read_excel(linkela_analytics_path,
    sheet = "Logins", skip = 4, na = "NULL"
) %>%
    normalize_colnames()

linkela_respuestas <- read_excel(linkela_analytics_path,
    sheet = "Formularis", skip = 10, na = "NULL"
) %>%
    normalize_colnames() %>%
    filter(form_id %in% linkela_enabled_forms) %>%
    select(-form_programmation_id, -fill_origin_id) %>%
    mutate(across(ends_with("_date"), ~ parse_date_time(.x, "Ymd HMS")))

linkela_alarmas <- read_excel(linkela_alarms_path, skip = 18, na = "NULL") %>%
    normalize_colnames() %>%
    filter(id_del_cuestionario %in% linkela_enabled_forms) %>%
    mutate(nombre_del_cuestionario = case_match(
        id_del_cuestionario,
        35 ~ "PNEUMO SINTOMAS",
        38 ~ "ALS-FRS",
        103 ~ "EAT-10",
        108 ~ "ROADS"
    ))
