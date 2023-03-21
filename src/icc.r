library(irr)

source("src/scores.r")

alsfrs_scores_icc <- function(data, model = "oneway", type = "agreement") {
    bulbar <- icc(data |> select(ends_with("_alsfrs_bulbar")), model = model, type = type)
    fmotor <- icc(data |> select(ends_with("_alsfrs_fmotor")), model = model, type = type)
    gmotor <- icc(data |> select(ends_with("_alsfrs_gmotor")), model = model, type = type)
    resp <- icc(data |> select(ends_with("_alsfrs_resp")), model = model, type = type)
    total <- icc(data |> select(ends_with("_alsfrs_total")), model = model, type = type)

    bind_rows(
        as_tibble(unclass(bulbar)),
        as_tibble(unclass(fmotor)),
        as_tibble(unclass(gmotor)),
        as_tibble(unclass(resp)),
        as_tibble(unclass(total))
    ) |>
        mutate(
            score = c("Bulbar", "Fine motor", "Gross motor", "Respiratory", "Total")
        ) |>
        relocate(score, .before = everything()) |>
        select(score, subjects, value, p.value, conf.level, lbound, ubound)
}

icc_overall <- alsfrs_scores_icc(scores)

icc_by_sex <- scores |>
    group_by(sexo) |>
    do(alsfrs_scores_icc(.))

icc_by_age <- scores |>
    mutate(edad_c = fct_lump_min(edad_c, min = 10)) |>
    group_by(edad_c) |>
    do(alsfrs_scores_icc(.))

icc_by_kings <- scores |>
    mutate(
        kings_c = fct_lump_min(kings_c, min = 5)
    ) |>
    group_by(kings_c) |>
    do(alsfrs_scores_icc(.))

icc_by_mitos <- scores |>
    group_by(mitos) |>
    do(alsfrs_scores_icc(.))

icc_by_studies <- scores |>
    drop_na(estudios) |>
    mutate(
        estudios = fct_lump_min(estudios, min = 5)
    ) |>
    group_by(estudios) |>
    do(alsfrs_scores_icc(.))

icc_by_peg <- scores |>
    group_by(portador_peg) |>
    mutate(
        portador_peg = case_match(
            portador_peg,
            TRUE ~ "Yes",
            FALSE ~ "No",
        )
    ) |>
    do(alsfrs_scores_icc(.))
