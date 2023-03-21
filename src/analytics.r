library(rlang)

source("src/linkela.r")
source("src/ufmn.r")

user_info <- linkela_users |>
    rename(linkela_id = "id") |>
    inner_join(ufmn_patients, by = c("tax_number" = "dni")) |>
    select(linkela_id, ufmn_id = "pid")

add_patient_info <- function(data, id, ...) {
    data |> left_join(
        user_info |>
            left_join(ufmn_patients, by = c("ufmn_id" = "pid")) |>
            left_join(ufmn_clinical, by = c("ufmn_id" = "pid")) |>
            select({{ id }} := linkela_id, ...),
        by = as_name(ensym(id)),
        multiple = "all"
    )
}

add_follow_up_info <- function(data, id, dtime, ...) {
    data |>
        mutate(add_follow_up_info__keep = TRUE) |>
        bind_rows(
            ufmn_functional |>
                rename(ufmn_id = "pid") |>
                inner_join(user_info, by = "ufmn_id", multiple = "all") |>
                select({{ id }} := linkela_id, {{ dtime }} := fecha_visita, ...)
        ) |>
        group_by({{ id }}) |>
        arrange({{ dtime }}, .by_group = TRUE) |>
        fill(...) |>
        ungroup() |>
        drop_na(add_follow_up_info__keep) |>
        select(-add_follow_up_info__keep)
}

patient_info <- exprs(ufmn_id, sexo, fecha_nacimiento, fenotipo_al_diagnostico)
follow_up_info <- exprs(portador_peg, kings_c, mitos)

responses_info <- linkela_responses |>
    add_patient_info(id = user_id, !!!patient_info) |>
    add_follow_up_info(id = user_id, dtime = start_date, !!!follow_up_info) |>
    mutate(
        has_clinical_data = !is.na(ufmn_id),
        edad = interval(fecha_nacimiento, start_date) %/% dyears(1)
    ) |>
    rename(response_id = "id", linkela_uid = "user_id") |>
    select(-c(ufmn_id, fecha_nacimiento)) |>
    relocate(has_clinical_data, .after = linkela_uid)

logins_info <- linkela_logins |>
    add_patient_info(id = user_id, !!!patient_info) |>
    add_follow_up_info(id = user_id, dtime = login_dtime, !!!follow_up_info) |>
    mutate(
        has_clinical_data = !is.na(ufmn_id),
        edad = interval(fecha_nacimiento, login_dtime) %/% dyears(1)
    ) |>
    rename(linkela_uid = "user_id") |>
    select(-c(ufmn_id, fecha_nacimiento)) |>
    relocate(has_clinical_data, .after = linkela_uid)

write_xlsx(responses_info, "output/responses-info.xlsx")
write_xlsx(logins_info, "output/logins-info.xlsx")
