pacman::p_load(
  tidyverse, readxl,writexl, openalexR, data.table
)
fread("Refs_sampler/refs.csv") -> imput

for (t in 15:32) {
  t |> print()
  oa_fetch(
    "works",
    id = imput$refs[((t*10000)+1):((t+1)*10000)],
    output = "list"
  ) -> fetched
  
  tibble(
    id = map_chr(fetched, "id"),
    issn_l = map_chr(fetched, ~ .x$primary_location$source$issn_l %||% NA_character_)
  ) |>
    filter(!issn_l |> is.na()) |>
    fwrite(paste0("Refs_sampler/refs_",t,".csv"))
}


oa_fetch(
  "works",
  id = imput$refs[330001:330626],
  output = "list"
) -> fetched

tibble(
  id = map_chr(fetched, "id"),
  issn_l = map_chr(fetched, ~ .x$primary_location$source$issn_l %||% NA_character_)
) |>
  filter(!issn_l |> is.na()) |>
  fwrite(paste0("Refs_sampler/refs_","33",".csv"))

