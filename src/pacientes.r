library(dplyr)
library(lubridate)
library(magrittr)

source("src/linkela.r")
source("src/sectecnica.r")
source("src/ufmn.r")

pacientes <- ufmn_patients %>%
    select(
        id_paciente, nhc, cip, dni, fecha_nacimiento, sexo,
        telefono_fijo, telefono_movil, fecha_exitus
    ) %>%
    inner_join(
        ufmn_clinical %>% select(
            id_paciente, fecha_inicio_clinica, fecha_diagnostico,
            fenotipo_al_diagnostico, patron_debilidad_inicial,
            fumador, historia_familiar_motoneurona, resultado_estudio_cognitivo,
            estudio_gen_c9, estudio_gen_sod1, estudio_gen_atxn2,
        ),
        by = "id_paciente"
    ) %>%
    filter(fecha_diagnostico >= sectecnica_origin_date) %>%
    inner_join(
        ufmn_baseline %>% select(id_paciente, delta_fs),
        by = "id_paciente"
    ) %>%
    mutate(
        sexo = factor(sexo, levels = c("Male", "Female"), labels = c("M", "F")),
        edad_inicio = floor((fecha_inicio_clinica - fecha_nacimiento) / dyears(1)),
        edad_actual = floor((today() - fecha_nacimiento) / dyears(1)),
        mutacion_c9 = estudio_gen_c9 == "Alterado",
        mutacion_sod1 = estudio_gen_sod1 == "Alterado",
        mutacion_atxn2 = estudio_gen_atxn2 == "Alterado",
        total_mutaciones = (
            if_else(mutacion_c9 == TRUE, 1, 0, 0) +
                if_else(mutacion_sod1 == TRUE, 1, 0, 0) +
                if_else(mutacion_atxn2 == TRUE, 1, 0, 0)
        ),
        gen_causal = case_when(
            total_mutaciones == 0 ~ "Desconocido",
            total_mutaciones > 1 ~ "Múltiple",
            mutacion_c9 ~ "C9Orf72",
            mutacion_sod1 ~ "SOD1",
            mutacion_atxn2 ~ "ATXN2"
        )
    ) %>%
    drop_na(
        sexo, fumador, edad_inicio, fenotipo_al_diagnostico, delta_fs, gen_causal
    )

pacientes_linkela_dni <- linkela_usuarios %>%
    inner_join(pacientes, by = c(tax_number = "dni"), suffix = c("_linkela", "_hub"))

pacientes_linkela_fijo <- linkela_usuarios %>%
    inner_join(pacientes, by = c(phone = "telefono_fijo"), suffix = c("_linkela", "_hub"))

pacientes_linkela_movil <- linkela_usuarios %>%
    inner_join(pacientes, by = c(phone = "telefono_movil"), suffix = c("_linkela", "_hub"))

pacientes_linkela <- bind_rows(
    pacientes_linkela_dni,
    pacientes_linkela_fijo,
    pacientes_linkela_movil
) %>%
    rename(id_linkela = id) %>%
    slice_head(n = 1, by = id_linkela) %>%
    left_join(
        linkela_logins %>%
            select(id_linkela, fecha_login = "login_at") %>%
            slice_min(fecha_login, by = id_linkela, with_ties = FALSE, na_rm = TRUE),
        by = "id_linkela"
    ) %>%
    left_join(
        linkela_respuestas %>%
            select(id_linkela, fecha_inicio_formulario = "start_date") %>%
            slice_min(fecha_inicio_formulario, by = id_linkela, with_ties = FALSE, na_rm = TRUE),
        by = "id_linkela"
    ) %>%
    mutate(
        fecha_primer_login_linkela = as_date(fecha_login),
        fecha_primer_formulario_linkela = as_date(fecha_inicio_formulario),
        .keep = "unused"
    ) %>%
    mutate(
        edad_primer_login_linkela = floor(
            (fecha_primer_login_linkela - fecha_nacimiento) / dyears(1)
        )
    )

pacientes %<>%
    left_join(
        pacientes_linkela %>%
            select(id_paciente, fecha_primer_login_linkela, fecha_primer_formulario_linkela),
        by = "id_paciente"
    ) %>%
    mutate(
        grupo = factor(if_else(is.na(fecha_primer_formulario_linkela), "Control", "LinkELA"))
    )
