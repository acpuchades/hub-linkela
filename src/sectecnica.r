library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)

source("src/common.r")

sectecnica_data_path <- "data/hub/sectecnica.xlsx"

sectecnica_hosp_sheet <- "Hospitalització"
sectecnica_hosp_skip <- 6

sectecnica_urg_sheet <- "Urgències hospitalàries"
sectecnica_urg_skip <- 6

sectecnica_origin_date <- dmy("01-01-2008")
sectecnica_cutoff_date <- dmy("21-06-2023")

sectecnica_datos_hosp <- read_excel(
  sectecnica_data_path,
  sheet = sectecnica_hosp_sheet,
  skip = sectecnica_hosp_skip
) %>%
  normalize_colnames() %>%
  select(-pacient_nhc)

sectecnica_datos_urg <- read_excel(
  sectecnica_data_path,
  sheet = sectecnica_urg_sheet,
  skip = sectecnica_urg_skip
) %>%
  normalize_colnames() %>%
  select(-pacient_nhc)

sectecnica_episodios_hosp <- sectecnica_datos_hosp %>%
  select(-diagnostics_de_l_episodi, -fecha_diagnostico) %>%
  slice_head(n = 1, by = episodi)

sectenica_diagnosticos_hosp <- sectecnica_datos_hosp %>%
  select(
    episodi,
    diagnostico = "diagnostics_de_l_episodi",
    data_diagnostic = "fecha_diagnostico"
  )

sectecnica_episodios_urg <- sectecnica_datos_urg %>%
  select(-diagnostics_de_l_episodi, -fecha_diagnostico) %>%
  slice_head(n = 1, by = episodi)

sectecnica_diagnosticos_urg <- sectecnica_datos_urg %>%
  select(
    episodi,
    diagnostic = "diagnostics_de_l_episodi",
    data_diagnostic = "fecha_diagnostico"
  )
