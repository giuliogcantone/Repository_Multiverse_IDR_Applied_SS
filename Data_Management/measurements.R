pacman::p_load(
  tidyverse,readxl,writexl,data.table,broom
)
load("sampled_works.RData")
source("Utilities/short_names.R")
source("Data_Management/IDR_Measures.R")

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
  right_join(papers) |>
  filter(!y_3 |> is.na(),
         !y_5 |> is.na()) |>
  mutate(Decile = factor(ntile(y_5, 10),
                         levels = 1:10)) -> papers

### IDR measures

papers |>
  select(-c(refs,topics,concepts,author,title,counts_by_year)) |>
  left_join(papers %>%
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
              IDR_measures(e_list$concepts,"Concepts",
                           "Utilities/Similarity_matrices/Concepts_similarity.xlsx")
            ) |>
  left_join(papers %>%
              transmute(paper, topics, journal) |>
              unnest(topics) |>
              filter(name == "field") |>
              transmute(paper,i = display_name, score,journal) |>
              summarise(
                .by = c(paper,i,journal),
                score = sum(score)
              ) |>
              mutate(
                .by = paper,
                p = score/sum(score)
              ) |>
              select(paper,i,p,journal) |>
              abbreviations_Scopus(i) |>
              IDR_measures(e_list$topics,"Topics",
                           "Utilities/Similarity_matrices/Scopus_cosine_similarity.xlsx")
            ) |>
  left_join(papers %>%
              transmute(paper, journal, refs) |>
              unnest(refs) |>
              left_join(
                fread("Data_Management/Refs_sampler/refs_Scopus.csv"),
                relationship = "many-to-many"
              ) |>
              filter(!i |> is.na()) |>
              summarise(
                .by = c(paper,i,journal),
                p = sum(p) |> round(5)
              ) |>
              mutate(
                .by = paper,
                p = p/sum(p)
              ) |>
              IDR_measures(e_list$Scopus,"Scopus",
                           "Utilities/Similarity_matrices/Scopus_cosine_similarity.xlsx")
            ) |>
  left_join(papers %>%
              transmute(paper, journal, refs) |>
              unnest(refs) |>
              left_join(
                fread("Data_Management/Refs_sampler/refs_WoS.csv"),
                relationship = "many-to-many"
              ) |>
              filter(!i |> is.na()) |>
              summarise(
                .by = c(paper,i,journal),
                p = sum(p) |> round(5)
              ) |>
              mutate(
                .by = paper,
                p = p/sum(p)
              ) |>
              IDR_measures(e_list$WoS,"JCR",
                           "Utilities/Similarity_matrices/WoS_cosine_similarity.xlsx")
            ) |>
  left_join(fread("Data_Management/clusters_post.csv") |>
              rename(journal = Journal)) |>
  fwrite("db_analysis.csv")

papers |>
  unnest(refs) |>
  pull(refs) |>
  unique() |>
  length()
