pacman::p_load(
  tidyverse,readxl,data.table
)
load("sampled_works.RData")
source("Utilities/journals_preprocess.R")

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
              abbreviations_concepts(i)
            ) |>
    left_join(
      fread("Data_Management/clusters_post.csv") |>
        rename(journal = Journal)
    ) |>
  summarise(.by = c(i,Cluster),
            p = sum(p),
            Taxonomy = "Concepts",
            Sample = "Articles") |>
  mutate(p = p/sum(p),
         .by = Cluster) |>
  arrange(-p) |>
  add_row(
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
      left_join(
        fread("Data_Management/clusters_post.csv") |>
          rename(journal = Journal)
      ) |>
      abbreviations_Scopus(i) |>
      summarise(.by = c(i,Cluster),
                p = sum(p),
                Taxonomy = "Topics",
                Sample = "Articles") |>
      mutate(p = p/sum(p),
             .by = Cluster) |>
      arrange(-p)
  ) |>
  add_row(
    papers %>%
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
      left_join(
        fread("Data_Management/clusters_post.csv") |>
          rename(journal = Journal)
      ) |>
      summarise(.by = c(i,Cluster),
                p = sum(p),
                Taxonomy = "Scopus",
                Sample = "Articles") |>
      mutate(p = p/sum(p),
             .by = Cluster) |>
      arrange(-p)
  ) |>
  add_row(
    papers %>%
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
      left_join(
        fread("Data_Management/clusters_post.csv") |>
          rename(journal = Journal)
      ) |>
      summarise(.by = c(i,Cluster),
                p = sum(p),
                Taxonomy = "JCR",
                Sample = "Articles") |>
      mutate(p = p/sum(p),
             .by = Cluster) |>
      arrange(-p)
  ) |>
  add_row(
    bind_rows(
      e_list$concepts |>
        left_join(
          fread("Data_Management/clusters_post.csv") |>
            rename(journal = Journal)
        ) |>
        summarise(.by = c(i,Cluster),
                  p = sum(e),
                  Taxonomy = "Concepts"),
      e_list$topics |>
        left_join(
          fread("Data_Management/clusters_post.csv") |>
            rename(journal = Journal)
        ) |>
        summarise(.by = c(i,Cluster),
                  p = sum(e),
                  Taxonomy = "Topics"),
      e_list$Scopus |>
        left_join(
          fread("Data_Management/clusters_post.csv") |>
            rename(journal = Journal)
        ) |>
        summarise(.by = c(i,Cluster),
                  p = sum(e),
                  Taxonomy = "Scopus"),
      e_list$WoS |>
        left_join(
          fread("Data_Management/clusters_post.csv") |>
            rename(journal = Journal)
        ) |>
        summarise(.by = c(i,Cluster),
                  p = sum(e),
                  Taxonomy = "JCR")
    ) |>
      mutate(Sample = "Journals",
             p = p/sum(p),
             .by = c(Taxonomy,Cluster)
             ) |>
      arrange(Taxonomy,-p)
  ) |>
    filter(!Cluster |> is.na()) |>
    write_xlsx("Results/Descriptives/pies.xlsx")
