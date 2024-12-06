pacman::p_load(
  tidyverse, readxl,writexl, openalexR, data.table
)
source("short_names.R")

e_list = list()

journals |>
  select(journal = Journal,
         topics) |>
  unnest(topics) |>
  filter(name == "field") |>
  mutate(
    .by = journal,
    i = display_name,
    e = count/sum(count)
  ) |>
  summarise(
    .by = c(journal,i),
    e = sum(e) |> round(5)
  ) |>
  abbreviations_Scopus(i) -> e_list$topics

journals %>%
  transmute(journal = Journal, concepts) |>
  unnest(concepts) |>
  filter(level == 0) |>
  mutate(
    .by = journal,
    i = display_name,
    e = score/sum(score)
  ) |>
  transmute(journal,i,e) |>
  abbreviations_concepts(i) -> e_list$concepts

papers |>
  unnest(refs) |>
  transmute(journal,refs) |>
  left_join(fread("Refs_sampler/refs_Scopus.csv"),
            relationship = "many-to-many") |>
  summarise(
    .by = c(journal,i),
    e = sum(p)
  ) |>
  filter(!i |> is.na()) |>
  mutate(
    .by = journal,
    e = e/sum(e, na.rm = T)
  ) -> e_list$Scopus

papers |>
  unnest(refs) |>
  transmute(journal,refs) |>
  left_join(fread("Refs_sampler/refs_WoS.csv"),
            relationship = "many-to-many") |>
  summarise(
    .by = c(journal,i),
    e = sum(p)
  ) |>
  filter(!i |> is.na()) |>
  mutate(
    .by = journal,
    e = e/sum(e, na.rm = T)
  ) -> e_list$WoS
