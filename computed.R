pacman::p_load(readxl)

read_xlsx("Similarity_matrices/WoS_cosine_similarity.xlsx") |>
  filter(z != 1) |>
  pull(z) |> mean()

read_xlsx("Similarity_matrices/Scopus_cosine_similarity.xlsx") |>
  filter(z != 1) |>
  pull(z) |> mean()

read_xlsx("Similarity_matrices/Concepts_Ochiai_similarity.xlsx") |>
  filter(z != 1) |>
  pull(z) |> mean()

