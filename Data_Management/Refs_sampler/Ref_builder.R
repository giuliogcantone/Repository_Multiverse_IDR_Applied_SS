pacman::p_load(
  tidyverse, readxl,writexl, openalexR, data.table
)
source("short_names.R")

fread(
  "Imputs/WoS_Scientific_db.csv"
) |>
  add_row(
    fread(
      "Imputs/WoS_Social_Sciences_db.csv"
    )
  ) |>
  mutate(
    ISSN = ifelse(ISSN == "",NA,ISSN),
    ISSN = coalesce(ISSN,eISSN),
  ) |>
  transmute(ISSN,discip = `Web of Science Categories` |> tolower()) |>
  mutate(discip = str_split(discip, "\\|")) %>%
  unnest(discip) %>%
  mutate(discip = str_trim(discip)) |>
  left_join(
    relationship = "many-to-many",
    fread("Imputs/WoS_categories.csv") |>
      transmute(discip = Category |> tolower(),
                Group)
  ) |>
  transmute(ISSN,
            i = str_split(Group,";")) |>
  unnest(i) |>
  distinct() |>
  filter(i != "Multidisciplinary") |>
  abbreviations_WoS(i) |>
  mutate(
    .by = ISSN,
    p = 1/n()
  ) -> WoS_categories

read_xlsx(
  "Imputs/Scopus_essential.xlsx"
) |>
  mutate(
    ISSN = coalesce(ISSN_print,ISSN_e),
  ) %>%
  select(ISSN, everything(),-c(ISSN_print,ISSN_e)) %>%
  pivot_longer(cols = 2:ncol(.), 
               names_to = "Field_1", 
               values_to = "i") |>
  select(-Field_1) |>
  filter(!i |> is.na()) %>%
  mutate(ISSN = str_sub(ISSN,
                        1, 4) %>% 
           paste0("-",
                  str_sub(ISSN, 5))
  ) |>
  abbreviations_Scopus(i) |>
  mutate(
    .by = ISSN,
    p = 1/n()
  ) -> Scopus_categories

rbindlist(lapply(paste0("Refs_sampler/refs_", 0:33, ".csv"),
                 fread)) |>
  transmute(refs = id,ISSN = issn_l) |>
  left_join(WoS_categories,
            relationship = "many-to-many") |>
  filter(!i |> is.na()) |>
  fwrite("Refs_sampler/refs_WoS.csv")

rbindlist(lapply(paste0("Refs_sampler/refs_", 0:33, ".csv"),
                 fread)) |>
  transmute(refs = id, ISSN = issn_l) |>
  left_join(Scopus_categories,
            relationship = "many-to-many") |>
  filter(!i |> is.na()) |>
  fwrite("Refs_sampler/refs_Scopus.csv")
  
