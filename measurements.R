pacman::p_load(
  tidyverse,readxl,writexl
)
source("short_names.R")
source("IDR_Measures.R")
source("journals_preprocess.R")

### First part

papers %>%
  unnest(author) %>%
  rename(author = au_display_name) %>%
  filter(!author %>% is.na()) %>%
  dplyr::count(title) %>%
  rename(n_authors = n) |>
  right_join(papers) -> papers
  
papers |>  
  select(paper,counts_by_year,pub_year) |>
  unnest(counts_by_year) |>
  reframe(
    y_3 = case_when(
      pub_year == 2013 ~ sum(cited_by_count[year <= 2016]),
      pub_year == 2018 ~ sum(cited_by_count[year <= 2021])
    ),
    y_5 = case_when(
      pub_year == 2013 ~ sum(cited_by_count[year <= 2018]),
      pub_year == 2018 ~ sum(cited_by_count[year <= 2023])
    ),
    .by = paper
  ) |>
  distinct() |>
  right_join(papers) -> papers

### IDR measures

papers %>%
  transmute(paper, concepts, journal) |>
  unnest(concepts) |>
  filter(level == 0) |>
  mutate(
    .by = paper,
    i = display_name,
    p = score/sum(score)
  ) |>
  filter(score > 0) |>
  select(paper,i,p, journal) |>
  abbreviations_concepts(i) |>
  IDR_measures(e_concepts,"Sem_Concepts",
             "Similarity_matrices/Concepts_Ochiai_similarity.xlsx")

papers %>%
  transmute(paper, topics, journal) |>
  unnest(topics) |>
  filter(name == "field") |>
  mutate(
    .by = paper,
    i = display_name,
    p = score/sum(score)
  ) |>
  summarise(
    .by = c(paper,i,journal),
    p = sum(p) |> round(5)
  ) |>
  abbreviations_Scopus(i) |>
  IDR_measures(e_concepts,"Sem_Topics",
               "Similarity_matrices/Scopus_cosine_similarity.xlsx")
