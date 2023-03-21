library(irr)
library(tidyverse)
library(lubridate)

source("src/linkela.r")
source("src/ufmn.r")

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

linkela_scores <- linkela_alsfrs |>
    left_join(linkela_users, by = c(user_id = "id"), multiple = "all") |>
    select(
        linkela_id = user_id,
        tax_number,
        fecha_valor = value_date,
        starts_with("alsfrs_")
    ) |>
    rename_with(~ str_c("linkela_", .x), starts_with("alsfrs_"))

ufmn_scores <- ufmn_functional |>
    left_join(ufmn_patients, by = "pid", multiple = "all") |>
    mutate(
        edad = interval(fecha_nacimiento, fecha_visita) %/% dyears(1),
        edad_c = cut(edad, breaks = c(0, 40, 60, 70, 80, 90), right = FALSE)
    ) |>
    select(
        ufmn_id = pid,
        dni, fecha_visita,
        sexo, edad, edad_c, estudios,
        portador_peg, kings_c, mitos,
        starts_with("alsfrs_"),
    ) |>
    rename_with(~ str_c("ufmn_", .x), starts_with("alsfrs_"))

scores <- linkela_scores |>
    drop_na(tax_number) |>
    mutate(id = row_number()) |>
    inner_join(ufmn_scores, by = c(tax_number = "dni"), multiple = "all") |>
    mutate(
        date_diff = abs(difftime(fecha_visita, fecha_valor, units = "weeks"))
    ) |>
    group_by(id) |>
    slice_min(date_diff, n = 1) |>
    ungroup() |>
    filter(date_diff <= 6) |>
    mutate(
        linkela_alsfrs_fmotor = case_match(
            portador_peg,
            TRUE ~ linkela_alsfrs_fmotor_peg,
            FALSE ~ linkela_alsfrs_fmotor_nopeg,
        ),
        linkela_alsfrs_total = case_match(
            portador_peg,
            TRUE ~ linkela_alsfrs_total_peg,
            FALSE ~ linkela_alsfrs_total_nopeg,
        )
    ) |>
    rename(portador_peg__keep = "portador_peg") |>
    select(-c(id, tax_number, ends_with("_peg"), ends_with("_nopeg"))) |>
    rename(portador_peg = "portador_peg__keep") |>
    relocate(
        linkela_id, ufmn_id,
        fecha_visita, fecha_valor, date_diff,
        sexo, edad, edad_c, estudios,
        portador_peg, kings_c, mitos,
        ends_with("_alsfrs_bulbar"),
        ends_with("_alsfrs_fmotor"),
        ends_with("_alsfrs_gmotor"),
        ends_with("_alsfrs_resp")
    ) |>
    arrange(linkela_id, fecha_valor)

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
