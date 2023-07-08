library(broom)
library(dplyr)
library(ggplot2)
library(irr)
library(rlang)
library(tidyr)
library(writexl)

source("src/pacientes.r")
source("src/ufmn.r")

as_domain <- function(d) {
    factor(
        d,
        levels = c(
            "total_bulbar",
            "total_motor_fino",
            "total_motor_grosero",
            "total_respiratorio",
            "total"
        ),
        labels = c(
            "Bulbar",
            "Fine Motor",
            "Gross Motor",
            "Respiratory",
            "Total"
        )
    )
}

calculate_icc <- function(data, x, y, ..., by = NULL) {
    data %>%
        drop_na({{ by }}) %>%
        summarize(
            pick({{ x }}, {{ y }}) %>%
                icc(...) %>%
                unclass() %>%
                as_tibble(),
            .by = {{ by }}
        )
}

make_scatterplot <- function(data, x, y, by = NULL,
                             title = NULL, limits = NULL,
                             jitter_w = .2, jitter_h = .2,
                             abline_alpha = .25) {
    if (quo_is_null(enquo(by))) {
        p <- ggplot(data, aes({{ x }}, {{ y }}))
    } else {
        data %<>% drop_na({{ by }})
        p <- ggplot(data, aes({{ x }}, {{ y }}, color = {{ by }}))
    }

    p +
        geom_jitter(width = jitter_w, height = jitter_h) +
        geom_abline(slope = 1, alpha = abline_alpha) +
        lims(x = limits, y = limits) +
        labs(title = title, x = "Standard", y = "Self-Assessed") +
        theme_bw() +
        theme(legend.position = "bottom")
}

alsfrs_linkela <- linkela_alsfrs %>%
    select(id_linkela, fecha = "data_valor", starts_with("total_")) %>%
    inner_join(pacientes_linkela, by = "id_linkela") %>%
    left_join(ufmn_portador_peg, by = "id_paciente", relationship = "many-to-many") %>%
    filter(fecha %>% between(inicio_periodo, fin_periodo)) %>%
    mutate(
        across(fecha, as_date),
        total_motor_fino = if_else(portador_peg, total_motor_fino_peg, total_motor_fino_nopeg),
        total = if_else(portador_peg, total_peg, total_nopeg),
    ) %>%
    select(-total_peg, -total_nopeg, -total_motor_fino_peg, -total_motor_fino_nopeg)

alsfrs_bellvitge <- ufmn_alsfrs %>%
    select(id_paciente, fecha = "fecha_visita", starts_with("total"), kings = "kings_c", mitos)

cross_alsfrs <- alsfrs_linkela %>%
    inner_join(
        alsfrs_bellvitge,
        by = "id_paciente",
        relationship = "many-to-many",
        suffix = c("_linkela", "_hub")
    ) %>%
    mutate(
        peg_status = factor(
            if_else(portador_peg, "Carrier", "Non-Carrier"),
            levels = c("Non-Carrier", "Carrier")
        ),
        diff_time = fecha_hub - fecha_linkela
    ) %>%
    filter(abs(diff_time) <= dweeks(3)) %>%
    arrange(abs(diff_time)) %>%
    relocate(
        id_paciente, id_linkela,
        sexo, portador_peg, kings, mitos,
        fecha_hub, fecha_linkela, diff_time,
        total_bulbar_hub, total_bulbar_linkela,
        total_motor_fino_hub, total_motor_fino_linkela,
        total_motor_grosero_hub, total_motor_grosero_linkela,
        total_respiratorio_hub, total_respiratorio_linkela,
        total_hub, total_linkela
    ) %T>%
    write_xlsx("cross-alsfrs.xlsx")

alsfrs_domains <- list(
    total_bulbar = list(title = "Bulbar ALSFRS-R", limits = c(0, 12)),
    total_motor_fino = list(title = "Fine Motor ALSFRS-R", limits = c(0, 12)),
    total_motor_grosero = list(title = "Gross Motor ALSFRS-R", limits = c(0, 12)),
    total_respiratorio = list(title = "Respiratory ALSFRS-R", limits = c(0, 12)),
    total = list(title = "Total ALSFRS-R", limits = c(0, 48))
)

alsfrs_groups <- list(
    sexo = "Sexo",
    kings = "King's Stage",
    mitos = "MiToS Stage",
    peg_status = "PEG Status"
)

suppressWarnings({
    unlink("output/@overall", recursive = TRUE)
    for (group in names(alsfrs_groups)) {
        unlink(file.path("output", group), recursive = TRUE)
    }
})

overall_icc <- tibble()
for (d in names(alsfrs_domains)) {
    dparams <- alsfrs_domains[[d]]
    hub_domain_col <- sym(str_c(d, "_hub"))
    linkela_domain_col <- sym(str_c(d, "_linkela"))

    domain_icc <- calculate_icc(
        cross_alsfrs, !!hub_domain_col, !!linkela_domain_col,
        model = "oneway", type = "agreement"
    )

    overall_icc %<>% bind_rows(
        domain_icc %>% mutate(domain = as_domain(d), .before = everything())
    )

    root_dir <- file.path("output", "@overall")
    dir.create(root_dir, showWarnings = FALSE, recursive = TRUE)

    ggsave(
        file.path(root_dir, str_glue("alsfrs-{d}.png")),
        make_scatterplot(
            cross_alsfrs, !!hub_domain_col, !!linkela_domain_col,
            title = dparams$title, limits = dparams$limits
        )
    )
}

overall_icc %>%
    arrange(domain) %>%
    mutate(value = pmax(.01, value)) %T>%
    write_xlsx(file.path(root_dir, "alsfrs-icc.xlsx")) %>%
    ggplot(aes(domain, value)) +
    geom_bar(stat = "identity") +
    labs(x = NULL, y = "ICC(1)")
ggsave(file.path(root_dir, "alsfrs-icc.png"))

for (group in names(alsfrs_groups)) {
    group_dir <- file.path("output", group)
    dir.create(group_dir, showWarnings = FALSE, recursive = TRUE)

    domain_icc <- tibble()
    for (d in names(alsfrs_domains)) {
        dparams <- alsfrs_domains[[d]]
        hub_domain_col <- sym(str_c(d, "_hub"))
        linkela_domain_col <- sym(str_c(d, "_linkela"))

        label <- alsfrs_groups[[group]]
        group_icc <- calculate_icc(
            cross_alsfrs, !!hub_domain_col, !!linkela_domain_col,
            by = !!sym(group), model = "oneway", type = "agreement"
        )

        ggsave(
            file.path(group_dir, str_glue("alsfrs-{d}.png")),
            make_scatterplot(
                cross_alsfrs, !!hub_domain_col, !!linkela_domain_col,
                by = !!sym(group), title = dparams$title, limits = dparams$limits
            ) + labs(color = label)
        )

        domain_icc %<>% bind_rows(
            group_icc %>% mutate(domain = as_domain(d), .before = everything())
        )
    }

    domain_icc %>%
        arrange(domain, !!sym(group)) %>%
        mutate(value = pmax(.01, value)) %T>%
        write_xlsx(file.path(group_dir, "alsfrs-icc.xlsx")) %>%
        ggplot(aes(domain, value, fill = !!sym(group))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = NULL, y = "ICC(1)", fill = label)
    ggsave(file.path(group_dir, "alsfrs-icc.png"))
}
