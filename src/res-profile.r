library(ggplot2)
library(ggsurvfit)
library(RColorBrewer)

source("src/pacientes.r")

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

png("output/pacientes-linkela.png", width = 2800, height = 960, res = 300)
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
ggsave("output/pacientes-linkela-edad-inclusion.png")

ggplot(pacientes_linkela, aes(x = edad_actual)) +
    geom_histogram(breaks = seq(40, 80, 10)) +
    labs(x = "Age")
ggsave("output/pacientes-linkela-edad-actual.png")
