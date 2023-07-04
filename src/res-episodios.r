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

source("src/pacientes.r")

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
