pacman::p_load(
  tidyverse, readxl,writexl, openalexR
)
source("concept_converter.R")

read_excel("Imputs/ACB.xlsx") %>%
  filter(`Level 2021` %in% c("4.0","4*")) %>%
  transmute(ISSN, Title = Title %>% toupper(),ABS=1) %>%
  full_join(
    read_excel("Imputs/Area13B.xlsx") %>%
      filter_at(vars(-TITOLO), any_vars(. %>% str_detect("A"))) %>%
      transmute(
        Title = TITOLO %>% toupper(),
        ISSN = ISSN %>% str_replace("‐","-"),
        ANVUR = 1
      )) %>%
  mutate(
    ABS = coalesce(ABS,0),
    ANVUR = coalesce(ANVUR,0)
  ) %>%
  mutate(ANVUR = ifelse(sum(ANVUR > 0),1,0),
         ABS = ifelse(sum(ABS > 0),1,0),
         .by = Title) %>%
  filter(ABS == 1,
         ANVUR == 1) |>
  transmute(ISSN,Journal=Title) |>
  filter(!Journal %in%
           (toupper(c("Cognition",
             "European Journal of Operational Research",
             "Annals of Statistics",
             "Biometrika")))
         ) -> journals

oa_fetch(
  entity = "sources",
  issn = journals %>% pull(ISSN) %>% unique(),
  output = "list",
) -> journals

sources2df(journals) |>
  transmute(
    id,
    Journal = display_name,
    ISSN = issn_l,
    counts_by_year,
    works_count,
    works_api_url,
    topics,
    concepts = lapply(journals, \(x) concept_converter(x$x_concepts))
    ) |>
  distinct() -> journals

oa_fetch(
  entity = "works",
  primary_location.source.id = journals$id,
  publication_year = c(2013,2018)
) -> papers_raw

papers_raw |>
filter(!author %>% is.na(),
       !publication_date %>% is.na(),
       !referenced_works %>% is.na(),
       !doi %>% is.na()) %>%
  transmute(paper = id,
            title = display_name %>% tolower(),
            author,
            journal = so,
            publication_date = publication_date %>% as.Date(),
            pub_year = publication_year,
            counts_by_year,
            topics,
            concepts,
            refs = referenced_works
  ) %>%
  arrange(publication_date) %>%
  filter(title %>% str_detect("erratum", negate = T),
         title %>% str_detect("editor", negate = T),
         title %>% str_detect("corrige", negate = T),
         title %>% str_detect("publisher correction", negate = T),
         title %>% str_detect("book review", negate = T),
         title %>% str_detect("isbn", negate = T)
  ) %>%
  distinct(title, .keep_all = T) -> papers

papers |>
  unnest(refs) |>
  select(refs) |>
  distinct() |>
  data.table::fwrite("refs.csv")
