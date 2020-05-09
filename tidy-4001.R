library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(fs)
read4001 <- function(path, sheet) {
  read_excel(path = path, sheet = sheet) %>% 
    pivot_longer(
      cols = starts_with(c("1", "2")), 
      names_to = "date",  
      values_to = "n") %>%
    rename(
      index = Index, 
      index_libelle = `libellé index`
    ) %>%
    mutate(
      date = str_replace_all(string = date, pattern = "\\_", replacement = "-")
    )
}
read4001(path = "data-raw/tableaux-4001-ts.xlsx", sheet = "France_Métro") %>%
  write_csv(path = "data-raw//etat_4001_france_metropolitaine.csv")
file_size(path = "data-raw/etat_4001_france_metropolitaine.csv")
read4001(path = "data-raw/tableaux-4001-ts.xlsx", sheet = "France_Entière") %>%
  write_csv(path = "data-raw/etat_4001_france_entiere.csv")
file_size(path = "data-raw/etat_4001_france_entiere.csv")
excel_sheets(path = "data-raw/tableaux-4001-ts.xlsx") %>%
  keep(.p = grepl(pattern = "^[[:digit:]]", x = .)) %>%
  set_names() %>%
  map_df(
    .f = function(x) {read4001(path = "data-raw/tableaux-4001-ts.xlsx", sheet = x)}, 
    .id = "code_departement"
  ) %>%
  write_csv(path = "data-raw/etat_4001_departements.csv.gz")
file_size(path = "data-raw/etat_4001_departements.csv.gz")
