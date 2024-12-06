pacman::p_load(
  tidyverse,readxl,writexl,data.table
)
load("sampled_works.RData")
source("short_names.R")
source("IDR_Measures.R")

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

papers |>
  mutate(
    .by = .c(publication_date,journal,n_authors),
    batch = cur_group_id()
  ) |>
  mutate(
    .by = batch,
    n_batch = n()
  ) |>
  mutate(
    .by = batch,
    Y_3 =
      case_when(
        y_3 > median(y_3) ~ 1,
        y_3 == median(y_3) ~ NA,
        y_3 < median(y_3) ~ 0,
      ),
    Y_5 =
      case_when(
        y_5 > median(y_5) ~ 1,
        y_5 == median(y_5) ~ NA,
        y_5 < median(y_5) ~ 0,
      )
  ) -> papers

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
              IDR_measures(e_list$concepts,"Sem_Concepts",
                           "Similarity_matrices/Concepts_Ochiai_similarity.xlsx")
            ) |>
  left_join(papers %>%
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
              IDR_measures(e_list$topics,"Sem_Fields",
                           "Similarity_matrices/Scopus_cosine_similarity.xlsx")
            ) |>
  left_join(papers %>%
              transmute(paper, journal, refs) |>
              unnest(refs) |>
              left_join(
                fread("Refs_sampler/refs_Scopus.csv"),
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
              IDR_measures(e_list$Scopus,"Cog_Fields",
                           "Similarity_matrices/Scopus_cosine_similarity.xlsx")
            ) |>
  left_join(papers %>%
              transmute(paper, journal, refs) |>
              unnest(refs) |>
              left_join(
                fread("Refs_sampler/refs_WoS.csv"),
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
              IDR_measures(e_list$WoS,"Cog_WoS",
                           "Similarity_matrices/WoS_cosine_similarity.xlsx")
            ) |>
  fwrite("db_analysis.csv")
  
