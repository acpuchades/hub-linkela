library(dplyr)
library(forcats)
library(ggplot2)
library(RColorBrewer)

source("src/pacientes.r")

palette <- brewer.pal(8, "Pastel1")

alarmas_linkela <- linkela_alarmas %>%
    rename(id_linkela = id_del_usuario) %>%
    inner_join(
        pacientes_linkela %>% select(id_linkela, id_paciente),
        by = "id_linkela"
    ) %>%
    left_join(ufmn_kings, by = "id_paciente") %>%
    left_join(ufmn_mitos, by = "id_paciente") %>%
    transmute(
        id_paciente, id_linkela,
        fecha_de_alarma,
        kings_1, kings_2, kings_3, kings_4,
        mitos_1, mitos_2, mitos_3, mitos_4,
        pregunta = factor(nombre_de_la_pregunta) %>%
            fct_lump_prop(0.02) %>%
            fct_infreq() %>%
            fct_relevel("Other", after = Inf),
        severidad = factor(str_to_title(tipo_de_alarma)) %>%
            fct_infreq(),
        formulario = factor(nombre_del_cuestionario) %>%
            fct_infreq(),
        kings = factor(case_when(
            fecha_de_alarma >= kings_4 ~ 4L,
            fecha_de_alarma >= kings_3 ~ 3L,
            fecha_de_alarma >= kings_2 ~ 2L,
            fecha_de_alarma >= kings_1 ~ 1L
        )) %>% fct_infreq(),
        mitos = factor(case_when(
            fecha_de_alarma >= mitos_4 ~ 4L,
            fecha_de_alarma >= mitos_3 ~ 3L,
            fecha_de_alarma >= mitos_2 ~ 2L,
            fecha_de_alarma >= mitos_1 ~ 1L
        )) %>% fct_infreq(),
    )

png("output/alarmas-linkela-preguntas.png", width = 3600, height = 1800, res = 300)
pie(table(alarmas_linkela$pregunta), col = palette, cex = 0.8)
dev.off()

png("output/alarmas-linkela-severidad.png", width = 3600, height = 1800, res = 300)
pie(table(alarmas_linkela$severidad), col = palette)
dev.off()

png("output/alarmas-linkela-formulario.png", width = 3600, height = 1800, res = 300)
pie(table(alarmas_linkela$formulario), col = palette)
dev.off()

png("output/alarmas-linkela-kings.png", width = 3600, height = 1800, res = 300)
pie(table(alarmas_linkela$kings), col = palette, cex = 0.8)
dev.off()

png("output/alarmas-linkela-mitos.png", width = 3600, height = 1800, res = 300)
pie(table(alarmas_linkela$mitos), col = palette, cex = 0.8)
dev.off()
