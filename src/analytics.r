source("src/linkela.r")
source("src/ufmn.r")

user_info <- linkela_users |>
    rename(linkela_id = "id") |>
    inner_join(ufmn_patients, by = c("tax_number" = "dni"), multiple = "all") |>
    select(linkela_id, ufmn_id = "pid", sexo, fecha_nacimiento)

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

responses_info <- linkela_responses |>
    add_follow_up_info(
        id = user_id, dtime = start_date,
        ufmn_id, fecha_nacimiento, sexo, portador_peg, kings_c, mitos
    ) |>
    mutate(edad = interval(fecha_nacimiento, start_date) %/% dyears(1)) |>
    select(
        response_id = "id", form_id,
        linkela_id = "user_id", ufmn_id,
        start_date, end_date, scheduled_date,
        edad, sexo, portador_peg, kings_c, mitos
    )

logins_info <- linkela_logins |>
    add_follow_up_info(
        id = user_id, dtime = login_dtime,
        ufmn_id, fecha_nacimiento, sexo, portador_peg, kings_c, mitos
    ) |>
    mutate(edad = interval(fecha_nacimiento, login_dtime) %/% dyears(1)) |>
    select(
        linkela_id = user_id, ufmn_id,
        login_successful, login_dtime,
        edad, sexo, portador_peg, kings_c, mitos
    )
