library(tidyverse)
library(lubridate)
library(magrittr)
library(dplyr)
library(writexl)

source("src/linkela.r")
source("src/ufmn.r")

linkela_scores <- linkela_alsfrs |>
    left_join(linkela_users, by = "pid") |>
    select(
        linkela_id = pid,
        tax_number,
        fecha_valor = value_date,
        starts_with("alsfrs_")
    ) |>
    rename_with(~ str_c("linkela_", .x), starts_with("alsfrs_"))

ufmn_scores <- ufmn_functional |>
    left_join(ufmn_patients, by = "pid") |>
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
