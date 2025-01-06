pacman::p_load(
  tidyverse,
  openalexR,
  readxl,
  writexl
)
source("short_names.R")

oa_fetch(
  entity = "concepts",
  level = 0
) -> oa_concepts

oa_concepts |>
  transmute(i = display_name) |>
  arrange(i) |>
  abbreviations_concepts(i) %>%
  tidyr::expand(i,j = i) |>
  left_join(
    oa_concepts |>
      transmute(i = display_name,
                related_concepts) |>
      abbreviations_concepts(i) |>
      unnest(related_concepts) |>
      filter(level == 0) |>
      transmute(
        i,j = display_name,c=score
      ) |>
      abbreviations_concepts(j)
  ) -> concepts_score

concepts_score |>
  mutate(c = coalesce(c,0)) |>
  mutate(p_i = c/max(c),
         .by = i) |>
  mutate(p_j = c/max(c),
         .by = j) |>
  transmute(i,j,
            z = (p_i + p_j) /2,
            z = z * .95,
            z = ifelse(i==j, 1, z)
            ) |>
  writexl::write_xlsx("Similarity_matrices/Concepts_similarity.xlsx")
