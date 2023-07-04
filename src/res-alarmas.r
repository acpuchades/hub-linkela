library(dplyr)
library(forcats)

source("src/pacientes.r")

palette <- brewer.pal(8, "Pastel1")

alarmas_linkela <- linkela_alarmas %>%
    transmute(
        pregunta = factor(nombre_de_la_pregunta) %>%
            fct_lump_prop(0.02) %>%
            fct_infreq() %>%
            fct_relevel("Other", after = Inf),
        severidad = factor(str_to_title(tipo_de_alarma)) %>%
            fct_infreq(),
        formulario = str_glue("Form {id_del_cuestionario}") %>%
            fct_lump_prop(0.05) %>%
            fct_infreq() %>%
            fct_relevel("Other", after = Inf)
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
