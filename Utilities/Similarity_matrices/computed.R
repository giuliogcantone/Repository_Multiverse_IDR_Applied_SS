pacman::p_load(readxl,tidyverse)

read_xlsx("Utilities/Similarity_matrices/WoS_cosine_similarity.xlsx") |>
  filter(z != 1) |>
  pull(z) |> mean()

read_xlsx("Utilities/Similarity_matrices/Scopus_cosine_similarity.xlsx") |>
  filter(z != 1) |>
  pull(z) |> mean()

read_xlsx("Utilities/Similarity_matrices/Concepts_similarity.xlsx") |>
  filter(z != 1) |>
  pull(z) |> mean()

