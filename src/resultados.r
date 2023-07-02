library(dplyr)
library(forcats)
library(ggplot2)
library(ggsurvfit)
library(lubridate)
library(magrittr)
library(marginaleffects)
library(MatchIt)
library(RColorBrewer)
library(stringr)
library(survival)
library(tidyr)

source("src/linkela.r")
source("src/sectecnica.r")
source("src/ufmn.r")

add_linkela_period_fields <- function(xs) {
    xs %>%
        mutate(
            inicio_periodo = case_match(
                grupo,
                "Control" ~ as_date(fecha_diagnostico),
                "LinkELA" ~ as_date(fecha_primer_formulario_linkela)
            ),
            fin_periodo = case_match(
                grupo,
                "Control" ~ pmin(
                    as_date(fecha_primer_formulario_linkela),
                    as_date(fecha_exitus),
                    as_date(.env$sectecnica_cutoff_date),
                    na.rm = TRUE
                ),
                "LinkELA" ~ pmin(
                    as_date(fecha_exitus),
                    as_date(.env$sectecnica_cutoff_date),
                    na.rm = TRUE
                )
            ),
            duracion_periodo = fin_periodo - inicio_periodo,
            .by = c(nhc, grupo)
        )
}

summarize_episodes <- function(xs, fecha) {
    xs %>%
        filter(
            {{ fecha }} >= inicio_periodo,
            {{ fecha }} < fin_periodo
        ) %>%
        summarize(
            inicio_periodo = first(inicio_periodo),
            fin_periodo = first(fin_periodo),
            duracion_periodo = first(duracion_periodo),
            total_episodios = n(),
            fecha_primer_episodio = min(as_date({{ fecha }})),
            episodios_por_año = total_episodios / (duracion_periodo / dyears(1)),
            .by = c(nhc, grupo)
        )
}

add_patients_without_episodes <- function(xs) {
    xs %>% bind_rows(
        pacientes %>%
            anti_join(xs, by = "nhc") %>%
            add_linkela_period_fields() %>%
            mutate(
                total_episodios = 0,
                episodios_por_año = 0,
                fecha_primer_episodio = as_date(NA)
            )
    )
}

pacientes <- ufmn_patients %>%
    select(
        pid, nhc, cip, dni, fecha_nacimiento, sexo,
        telefono_fijo, telefono_movil, fecha_exitus
    ) %>%
    inner_join(
        ufmn_clinical %>% select(
            pid, fecha_inicio_clinica, fecha_diagnostico,
            fenotipo_al_diagnostico, patron_debilidad_inicial,
            fumador, historia_familiar_motoneurona, resultado_estudio_cognitivo,
            estudio_gen_c9, estudio_gen_sod1, estudio_gen_atxn2,
        ),
        by = "pid"
    ) %>%
    filter(fecha_diagnostico >= sectecnica_origin_date) %>%
    inner_join(
        ufmn_baseline %>% select(pid, delta_fs),
        by = "pid"
    ) %>%
    mutate(
        sexo = factor(sexo, levels = c("Male", "Female"), labels = c("M", "F")),
        edad_inicio = floor((fecha_inicio_clinica - fecha_nacimiento) / dyears(1)),
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
    slice_head(n = 1, by = id) %>%
    left_join(
        linkela_logins %>%
            select(user_id, fecha_login = "login_at") %>%
            slice_min(fecha_login, by = user_id, with_ties = FALSE, na_rm = TRUE),
        by = c(id = "user_id")
    ) %>%
    left_join(
        linkela_respuestas %>%
            select(user_id, fecha_inicio_formulario = "start_date") %>%
            slice_min(fecha_inicio_formulario, by = user_id, with_ties = FALSE, na_rm = TRUE),
        by = c(id = "user_id")
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
            select(pid, fecha_primer_login_linkela, fecha_primer_formulario_linkela),
        by = "pid"
    ) %>%
    mutate(
        grupo = factor(if_else(is.na(fecha_primer_formulario_linkela), "Control", "LinkELA"))
    )

# Perfil de los pacientes LinkELA

perfil_pacientes_linkela <- pacientes_linkela %>%
    summarize(
        total = n(),
        total_hombres = sum(sexo == "M"),
        total_mujeres = sum(sexo == "F"),
        total_fenotipo_bulbar = sum(fenotipo_al_diagnostico == "ELA Bulbar"),
        total_fenotipo_espinal = sum(fenotipo_al_diagnostico == "ELA Espinal"),
        total_fenotipo_amp = sum(fenotipo_al_diagnostico == "AMP"),
        total_fenotipo_otro = total - (total_fenotipo_bulbar + total_fenotipo_espinal + total_fenotipo_amp),
        total_gen_sod1 = sum(gen_causal == "SOD1"),
        total_gen_atxn2 = sum(gen_causal == "ATXN2"),
        total_gen_desconocido = sum(gen_causal == "Desconocido"),
    )

palette <- brewer.pal(9, "Pastel1")

png("output/perfil-linkela.png", width = 2800, height = 960, res = 300)
par(mfrow = c(1, 3))

perfil_pacientes_linkela %$% pie(
    c(total_hombres, total_mujeres),
    labels = c(
        str_glue("Male ({total_hombres})"),
        str_glue("Female ({total_mujeres})")
    ),
    col = palette
)

perfil_pacientes_linkela %$% pie(
    c(
        total_fenotipo_espinal, total_fenotipo_bulbar,
        total_fenotipo_amp, total_fenotipo_otro
    ),
    labels = c(
        str_glue("Spinal ({total_fenotipo_espinal})"),
        str_glue("Bulbar ({total_fenotipo_bulbar})"),
        str_glue("PMA ({total_fenotipo_amp})"),
        str_glue("Other ({total_fenotipo_otro})")
    ), col = palette
)

perfil_pacientes_linkela %$% pie(
    c(
        total_gen_desconocido, total_gen_sod1, total_gen_atxn2
    ),
    labels = c(
        str_glue("Unknown ({total_gen_desconocido})"),
        str_glue("SOD1 ({total_gen_sod1})"),
        str_glue("ATXN2 ({total_gen_atxn2})")
    ), col = palette
)

par(mfrow = c(1, 1))
dev.off()

ggplot(pacientes_linkela, aes(x = edad_primer_login_linkela)) +
    geom_histogram(breaks = seq(40, 80, 10)) +
    labs(title = "Age Of Enrollment", x = "Age")
ggsave("output/perfil-linkela-edad.png")

# Sistema de alarmas

perfil_alarmas_linkela <- linkela_alarmas %>%
    transmute(
        pregunta = factor(nombre_de_la_pregunta) %>%
            fct_lump_prop(0.02) %>%
            fct_infreq() %>%
            fct_relevel("Other", after = Inf),
        severidad = factor(str_to_title(tipo_de_alarma)) %>%
            fct_infreq(),
        cuestionario = factor(id_del_cuestionario) %>%
            fct_lump_prop(0.05) %>%
            fct_infreq() %>%
            fct_relevel("Other", after = Inf)
    )

png("output/perfil-alarmas-linkela-preguntas.png", width = 3600, height = 1800, res = 300)
pie(table(perfil_alarmas_linkela$pregunta), col = palette, cex = 0.8)
dev.off()

png("output/perfil-alarmas-linkela-severidad.png", width = 3600, height = 1800, res = 300)
pie(table(perfil_alarmas_linkela$severidad), col = palette)
dev.off()

png("output/perfil-alarmas-linkela-cuestionario.png", width = 3600, height = 1800, res = 300)
pie(table(perfil_alarmas_linkela$cuestionario), col = palette)
dev.off()

# Propensity Score Matching

psm <- matchit(
    grupo ~ sexo + fumador + edad_inicio + fenotipo_al_diagnostico + delta_fs + gen_causal,
    data = pacientes, method = "full", distance = "glm"
)

pacientes_psm <- match.data(psm)

## Consultas a urgencias: controles vs LinkELA

consultas_urg <- sectecnica_episodios_urg %>%
    inner_join(pacientes_psm, by = "nhc") %>%
    add_linkela_period_fields() %>%
    summarize_episodes(data_entrada) %>%
    add_patients_without_episodes() %>%
    inner_join(pacientes_psm %>% select(nhc, weights), by = "nhc")

ggplot(consultas_urg, aes(sample = episodios_por_año)) +
    geom_qq() +
    geom_qq_line() +
    facet_wrap(~grupo)
ggsave("output/consultas-urgencias-qqplot.png")

ggplot(consultas_urg, aes(grupo, episodios_por_año)) +
    geom_boxplot(aes(fill = grupo)) +
    labs(title = "ER Episodes", x = NULL, y = "Episodes / Year", fill = NULL)
ggsave("output/consultas-urgencias-boxplot.png")

consultas_urg_survdata <- consultas_urg %>%
    mutate(
        time_to_end = as.duration(fin_periodo - inicio_periodo),
        time_to_event = as.duration(fecha_primer_episodio - inicio_periodo),
        duracion = pmin(time_to_end, time_to_event, na.rm = TRUE),
        evento = !is.na(time_to_event) & time_to_event < time_to_end,
    ) %>%
    filter(duracion >= 0)

consultas_urg_survdata %>%
    survfit2(Surv(duracion, evento) ~ grupo, .) %>%
    ggsurvfit() +
    add_confidence_interval() +
    labs(title = "Time to ER episode", x = "Time (months)")
ggsave("output/consultas-urgencias-km.png")

sink("output/consultas-urgencias.txt")

consultas_urg_control <- filter(consultas_urg, grupo == "Control")
shapiro.test(consultas_urg_control$episodios_por_año) %>% print()

consultas_urg_linkela <- filter(consultas_urg, grupo == "LinkELA")
shapiro.test(consultas_urg_linkela$episodios_por_año) %>% print()

# lm(episodios_por_año ~ grupo * (
#    sexo + fumador + edad_inicio + fenotipo_al_diagnostico + delta_fs + gen_causal
# ), consultas_urg, weights)

wilcox.test(
    episodios_por_año ~ grupo, consultas_urg,
    alternative = "greater"
) %>% print()

coxph(Surv(duracion, evento) ~ grupo, consultas_urg_survdata, weights) %>%
    print()

sink()

## Hospitalizaciones: controles vs LinkELA

ingresos_hosp <- sectecnica_episodios_hosp %>%
    inner_join(pacientes_psm, by = "nhc") %>%
    add_linkela_period_fields() %>%
    summarize_episodes(data_ingres) %>%
    add_patients_without_episodes() %>%
    inner_join(pacientes_psm %>% select(nhc, weights), by = "nhc")

ggplot(ingresos_hosp, aes(sample = episodios_por_año)) +
    geom_qq() +
    geom_qq_line() +
    facet_wrap(~grupo)
ggsave("output/hospitalizaciones-qqplot.png")

ggplot(ingresos_hosp, aes(grupo, episodios_por_año)) +
    geom_boxplot(aes(fill = grupo)) +
    labs(title = "Hospital admissions", x = NULL, y = "Episodes / Year", fill = NULL)
ggsave("output/hospitalizaciones-boxplot.png")

ingresos_hosp_survdata <- ingresos_hosp %>%
    mutate(
        time_to_end = fin_periodo - inicio_periodo,
        time_to_event = fecha_primer_episodio - inicio_periodo,
        duracion = pmin(time_to_end, time_to_event, na.rm = TRUE),
        evento = !is.na(time_to_event) & time_to_event < time_to_end,
    )

ingresos_hosp_survdata %>%
    survfit2(Surv(duracion, evento) ~ grupo, data = .) %>%
    ggsurvfit() +
    add_confidence_interval() +
    labs(title = "Time to hospitalization", x = "Time (months)")
ggsave("output/hospitalizaciones-km.png")

sink("output/hospitalizaciones.txt")

wilcox.test(
    episodios_por_año ~ grupo, ingresos_hosp,
    alternative = "greater"
) %>% print()

coxph(Surv(duracion, evento) ~ grupo, ingresos_hosp_survdata, weights) %>%
    print()

sink()
