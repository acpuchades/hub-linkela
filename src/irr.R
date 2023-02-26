library(irr)
library(ggplot2)

source("src/scores.R")

alsfrs_scores_icc <- function(data, model = "oneway", type = "agreement") {
    bulbar <- icc(data %>% select(ends_with("_alsfrs_bulbar")), model = model, type = type)
    fmotor <- icc(data %>% select(ends_with("_alsfrs_fmotor")), model = model, type = type)
    gmotor <- icc(data %>% select(ends_with("_alsfrs_gmotor")), model = model, type = type)
    resp <- icc(data %>% select(ends_with("_alsfrs_resp")), model = model, type = type)
    total <- icc(data %>% select(ends_with("_alsfrs_total")), model = model, type = type)

    bind_rows(
        as_tibble(unclass(bulbar)),
        as_tibble(unclass(fmotor)),
        as_tibble(unclass(gmotor)),
        as_tibble(unclass(resp)),
        as_tibble(unclass(total))
    ) %>%
        mutate(
            score = c("Bulbar", "Fine motor", "Gross motor", "Respiratory", "Total")
        ) %>%
        relocate(score, .before = everything()) %>%
        select(score, subjects, value, p.value, conf.level, lbound, ubound)
}

overall <- alsfrs_scores_icc(scores)
write_xlsx(overall, "output/irr-overall.xlsx")

p <- ggplot(overall, aes(x = score, y = value)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Overall Agreement Between Standard and Self-Administered ALSFRS-R",
        x = NULL, y = "ICC(1)"
    )

err <- geom_errorbar(aes(ymin = lbound, ymax = ubound), width = .2, position = position_dodge(.9))

ggsave("output/irr-overall.jpg", p)
ggsave("output/irr-overall-err.jpg", p + err)

by_sex <- scores %>%
    group_by(sexo) %>%
    do(alsfrs_scores_icc(.))

write_xlsx(by_sex, "output/irr-by-sex.xlsx")

p <- ggplot(by_sex, aes(x = score, y = value, fill = sexo)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By Sex",
        x = NULL, y = "ICC(1)", fill = "Sex"
    )

ggsave("output/irr-by-sex.jpg", p)
ggsave("output/irr-by-sex-err.jpg", p + err)

by_age <- scores %>%
    mutate(edad_c = fct_lump_min(edad_c, min = 10)) %>%
    group_by(edad_c) %>%
    do(alsfrs_scores_icc(.))

write_xlsx(by_age, "output/irr-by-age.xlsx")

p <- ggplot(by_age, aes(x = score, y = value, fill = edad_c)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By Age",
        x = NULL, y = "ICC(1)", fill = "Age"
    )

ggsave("output/irr-by-age.jpg", p)
ggsave("output/irr-by-age-err.jpg", p + err)

by_studies <- scores %>%
    drop_na(estudios) %>%
    mutate(
        estudios = fct_lump_min(estudios, min = 5)
    ) %>%
    group_by(estudios) %>%
    do(alsfrs_scores_icc(.))

write_xlsx(by_studies, "output/irr-by-education.xlsx")

p <- ggplot(by_studies, aes(x = score, y = value, fill = estudios)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By Education",
        x = NULL, y = "ICC(1)", fill = "Education"
    )

ggsave("output/irr-by-education.jpg", p)
ggsave("output/irr-by-education-err.jpg", p + err)

by_peg <- scores %>%
    group_by(portador_peg) %>%
    mutate(
        portador_peg = case_match(
            portador_peg,
            TRUE ~ "Yes",
            FALSE ~ "No",
        )
    ) %>%
    do(alsfrs_scores_icc(.))

write_xlsx(by_peg, "output/irr-by-peg.xlsx")

p <- ggplot(by_peg, aes(x = score, y = value, fill = portador_peg)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By PEG Status",
        x = NULL, y = "ICC(1)", fill = "PEG carrier"
    )

ggsave("output/irr-by-peg.jpg", p)
ggsave("output/irr-by-peg-err.jpg", p + err)

by_kings <- scores %>%
    mutate(
        kings_c = fct_lump_min(kings_c, min = 5)
    ) %>%
    group_by(kings_c) %>%
    do(alsfrs_scores_icc(.))

write_xlsx(by_kings, "output/irr-by-kings.xlsx")

p <- ggplot(by_kings, aes(x = score, y = value, fill = kings_c)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By King's Stage",
        x = NULL, y = "ICC(1)", fill = "King's"
    )

ggsave("output/irr-by-kings.jpg", p)
ggsave("output/irr-by-kings-err.jpg", p + err)

by_mitos <- scores %>%
    group_by(mitos) %>%
    do(alsfrs_scores_icc(.))

write_xlsx(by_mitos, "output/irr-by-mitos.xlsx")

p <- ggplot(by_mitos, aes(x = score, y = value, fill = mitos)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent, breaks = seq(-1, 1, .2)) +
    labs(
        title = "Agreement Between Standard and Self-Administered ALSFRS-R By MiToS Stage",
        x = NULL, y = "ICC(1)", fill = "MiToS"
    )

ggsave("output/irr-by-mitos.jpg", p)
ggsave("output/irr-by-mitos-err.jpg", p + err)
