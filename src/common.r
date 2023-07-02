library(dplyr)
library(stringi)
library(stringr)

normalize_colnames <- function(xs) {
    xs %>%
        rename_with(~ .x %>%
            stri_trans_general(id = "Latin-ASCII") %>%
            str_to_lower() %>%
            str_replace_all("<", "lt_") %>%
            str_replace_all(">", "gt_") %>%
            str_replace_all("[^A-Za-z0-9]+", "_") %>%
            str_replace_all("(^_)|(_$)", "") %>%
            str_replace_all("^([^A-Za-z])", "x\\1"))
}
