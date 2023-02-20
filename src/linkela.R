library(readxl)
library(tidyverse)

linkela_users_path <- "data/2023.02.16_LinkELA_users_v5.xlsx"
linkela_alsfrs_path <- "data/2023.02.14_LinkELA_Formulario-38.xlsx"

linkela_users <- read_xlsx(linkela_users_path, na = "NULL") %>%
    rename(pid = "id") %>%
    mutate(
        birthdate = parse_date(birthdate),
        tax_number = gsub("-", "", tax_number),
        last_login_at = parse_datetime(last_login_at),
        gender = factor(gender, labels = c("M", "F")),
        cuidador = case_match(cuidador, "Yes" ~ TRUE, "No" ~ FALSE)
    )

linkela_alsfrs <- read_xlsx(linkela_alsfrs_path, na = "NULL") %>%
    select(-starts_with("Pregunta sin etiqueta")) %>%
    rename(
        pid = "Pacient",
        departament = "Departament",
        value_date = "Data Valor",
        answer_date = "Data resposta",
        speech = "1. Lenguaje",
        salivation = "2. Salivación",
        swallowing = "3. Tragar",
        handwriting = "4. Escritura",
        cutting_nopeg = "5a. Cortado de comida y uso de utensilios (pacientes sin gastrostomía)",
        cutting_peg = "5b Cortar comida y manejo de utensilios (alternativo para pacientes con gastrostomía)",
        dressing = "6. Vestido e higiene",
        bed = "7. Girarse en la cama y ajustarse la ropa de la cama",
        walking = "8. Andar",
        stairs = "9. Subir escaleras",
        dyspnea = "10.Disnea (sensación de falta de aire)",
        orthopnea = "11.Ortopnea (falta de aire estando acostado)",
        resp_insuf = "12.Insuficiencia respiratoria",
    ) %>%
    mutate(
        across(ends_with("_date"), \(x) parse_datetime(x, format = "%d/%m/%Y %H:%M:%S")),
        speech = recode(speech,
            "Habla normal" = 4L,
            "Procesos del habla normales." = 4L,
            "Alteraciones en el habla detectables" = 3L,
            "Trastornos del habla detectables." = 3L,
            "Habla inteligible con repeticiones." = 2L,
            "Usa lenguaje verbal combinado con comunicación no verbal" = 1L,
            "Pérdida del habla útil." = 0L,
            "Sense resposta" = NA_integer_
        ),
        salivation = recode(salivation,
            "Normal." = 4L,
            "Aunque leve, definitivo exceso de saliva en la boca, puede haber sialorrea nocturna mínima." = 3L,
            "Exceso de saliva leve (pero claro) en boca; posible babeo nocturno" = 3L,
            "Exceso de saliva moderado; posible babeo mínimo" = 2L,
            "Exceso de saliva marcado con algo de babeo" = 1L,
            "Babeo marcado; que requiere uso de pañuelo constante" = 0L,
            "Sense resposta" = NA_integer_
        ),
        swallowing = recode(swallowing,
            "Hábitos de alimentación normales" = 4L,
            "Hábitos alimenticios normales." = 4L,
            "Problemas precoces para tragar (atragantamiento ocasional)" = 3L,
            "Problemas alimenticios tempranos, ahogamientos ocasionales." = 3L,
            "Precisa cambios en la consistencia de la dieta" = 2L,
            "Necesidad de alimentación suplementaria por sonda" = 1L,
            "Alimentación exclusiva por sonda" = 0L,
            "Sense resposta" = NA_integer_
        ),
        handwriting = recode(handwriting,
            "Normal." = 4L,
            "Lenta; pero todas las palabras son legibles" = 3L,
            "Un poco lenta y torpe, todas las palabras son legibles." = 3L,
            "No todas las palabras son legibles." = 2L,
            "Es capaz de sujetar el lápiz pero no es capaz de escribir" = 1L,
            "Incapaz de sostener el lápiz" = 0L,
            "Sense resposta" = NA_integer_
        ),
        cutting_nopeg = recode(cutting_nopeg,
            "Normal." = 4L,
            "Lento y torpe pero no precisa ayuda" = 3L,
            "Capaz de cortar la mayoría de los alimentos, torpe y lento; necesita alguna ayuda" = 2L,
            "Puede cortar la mayoría de las comidas, lento y torpe, requiere algo de ayuda." = 2L,
            "La comida requiere ser cortada por alguien más, aún puede alimentarse lentamente." = 1L,
            "Otra persona tiene que cortarle la comida, luego puede alimentarse lentamente" = 1L,
            "Precisa ser alimentado por otra persona" = 0L,
            "Sense resposta" = NA_integer_
        ),
        cutting_peg = recode(cutting_peg,
            "Normal." = 4L,
            "Torpe, puede manejar todos los utensilios." = 3L,
            "Lento y torpe pero capaz de realizar todas las manipulaciones de forma independiente" = 3L,
            "Requiere algo de ayuda con cierres y broches." = 2L,
            "Proporciona mínima ayuda al cuidador" = 1L,
            "Incapaz de realizar ningún aspecto de la tarea" = 0L,
            "Sense resposta" = NA_integer_
        ),
        cutting = case_when(
            is.na(cutting_peg) ~ cutting_nopeg,
            is.na(cutting_nopeg) ~ cutting_peg,
        ),
        dressing = recode(dressing,
            "Normal." = 4L,
            "Cuidado personal independiente y completo, pero con mayor esfuerzo" = 3L,
            "Precisa asistencia intermitente o el uso de métodos sustitutivos" = 2L,
            "Requiere ayuda intermitente o métodos sustitutos." = 2L,
            "Precisa ayuda para la mayor parte de las tareas" = 1L,
            "Requiere ayuda de cuidador para autocuidado." = 1L,
            "Dependencia completa" = 0L,
            "Sense resposta" = NA_integer_
        ),
        bed = recode(bed,
            "Normal." = 4L,
            "Algo lento y torpe, no necesita ayuda." = 3L,
            "Puede voltearse solo o ajustar las sábanas con dificultad." = 2L,
            "Puede girarse o ajustar sábanas solo, aunque con mucha dificultad" = 2L,
            "Puede iniciar el giro o el ajuste de las sábanas, pero no puede completarlo solo" = 1L,
            "Puede comenzar a voltearse sin terminar, no puede ajustar sábanas." = 1L,
            "Dependiente de otra persona" = 0L,
            "Sense resposta" = NA_integer_
        ),
        walking = recode(walking,
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
        stairs = recode(stairs,
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
        dyspnea = recode(dyspnea,
            "Ninguna." = 4L,
            "No" = 4L,
            "Ocurre solo cuando camina" = 3L,
            "Ocurre con uno o más: comer, bañarse y vestirse." = 2L,
            "Ocurre en una o más de las siguientes actividades diarias: comer, asearse, vestirse..." = 2L,
            "Ocurre en reposo, dificultad respiratoria sentado o tumbado" = 1L,
            "Dificultad importante, se ha considerado el uso de soporte respiratorio o ventilatorio mecánico" = 0L,
            "Sense resposta" = NA_integer_
        ),
        orthopnea = recode(orthopnea,
            "Ninguna." = 4L,
            "Alguna dificultad para dormir por la noche. No necesita más de 2 almohadas" = 3L,
            "Necesita más de 2 almohadas para poder dormir" = 2L,
            "Requiere de almohadas extra para dormir (>2)" = 2L,
            "Solo puede dormir sentado" = 1L,
            "Incapaz de dormir por sensación de falta de aire" = 0L,
            "Sense resposta" = NA_integer_
        ),
        resp_insuf = recode(resp_insuf,
            "No" = 4L,
            "Ninguna." = 4L,
            "Uso intermitente de BiPAP." = 3L,
            "Uso continuo de BiPAP durante la noche" = 2L,
            "Uso continuo de BiPAP por las noches." = 2L,
            "Uso continuo de BiPAP, noche y día" = 1L,
            "Sense resposta" = NA_integer_
        )
    ) %>%
    relocate(
        cutting,
        .after = handwriting
    ) %>%
    rowwise() %>%
    mutate(
        bulbar_score = speech + salivation + swallowing,
        fmotor_peg_score = handwriting + cutting_peg + dressing,
        fmotor_nopeg_score = handwriting + cutting_nopeg + dressing,
        gmotor_score = bed + walking + stairs,
        resp_score = dyspnea + orthopnea + resp_insuf,
        total_peg_score = bulbar_score + fmotor_peg_score + gmotor_score + resp_score,
        total_nopeg_score = bulbar_score + fmotor_nopeg_score + gmotor_score + resp_score
    )
