library(ggplot2)
library(writexl)

source("src/scores-icc.r")

p <- ggplot(icc_overall, aes(x = score, y = value)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Overall Agreement Between Standard and Self-Administered ALSFRS-R",
        x = NULL, y = "ICC(1)"
    )

err <- geom_errorbar(aes(ymin = lbound, ymax = ubound), width = .2, position = position_dodge(.9))

ggsave("output/icc-overall.jpg", p)
ggsave("output/icc-overall-err.jpg", p + err)
write_xlsx(icc_overall, "output/icc-overall.xlsx")

p <- ggplot(icc_by_sex, aes(x = score, y = value, fill = sexo)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By Sex",
        x = NULL, y = "ICC(1)", fill = "Sex"
    )

ggsave("output/icc-by-sex.jpg", p)
ggsave("output/icc-by-sex-err.jpg", p + err)
write_xlsx(icc_by_sex, "output/icc-by-sex.xlsx")

p <- ggplot(icc_by_age, aes(x = score, y = value, fill = edad_c)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By Age",
        x = NULL, y = "ICC(1)", fill = "Age"
    )

ggsave("output/icc-by-age.jpg", p)
ggsave("output/icc-by-age-err.jpg", p + err)
write_xlsx(icc_by_age, "output/icc-by-age.xlsx")

p <- ggplot(icc_by_studies, aes(x = score, y = value, fill = estudios)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By Education",
        x = NULL, y = "ICC(1)", fill = "Education"
    )

ggsave("output/icc-by-education.jpg", p)
ggsave("output/icc-by-education-err.jpg", p + err)
write_xlsx(icc_by_studies, "output/icc-by-education.xlsx")

p <- ggplot(icc_by_peg, aes(x = score, y = value, fill = portador_peg)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By PEG Status",
        x = NULL, y = "ICC(1)", fill = "PEG carrier"
    )

ggsave("output/icc-by-peg.jpg", p)
ggsave("output/icc-by-peg-err.jpg", p + err)
write_xlsx(icc_by_peg, "output/icc-by-peg.xlsx")

p <- ggplot(icc_by_kings, aes(x = score, y = value, fill = kings_c)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By King's Stage",
        x = NULL, y = "ICC(1)", fill = "King's"
    )

ggsave("output/icc-by-kings.jpg", p)
ggsave("output/icc-by-kings-err.jpg", p + err)
write_xlsx(icc_by_kings, "output/icc-by-kings.xlsx")

p <- ggplot(icc_by_mitos, aes(x = score, y = value, fill = mitos)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By MiToS Stage",
        x = NULL, y = "ICC(1)", fill = "MiToS"
    )

ggsave("output/icc-by-mitos.jpg", p)
ggsave("output/icc-by-mitos-err.jpg", p + err)
write_xlsx(icc_by_mitos, "output/icc-by-mitos.xlsx")
