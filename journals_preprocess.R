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
  abbreviations_Scopus(i) -> e_topics

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
  abbreviations_concepts(i) -> e_concepts