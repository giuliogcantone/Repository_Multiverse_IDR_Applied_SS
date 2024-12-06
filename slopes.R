results = list()

estimates = tibble(
  journal = db$journal |> unique()
)

db |>
  dplyr::select(starts_with("Sem_") | starts_with("Cog_")) %>%
  names()

for (y in (c("y_3","y_5"))) {
  for (x in db |>
       dplyr::select(starts_with("Sem_") | starts_with("Cog_")) %>%
       names()) {
    
    iteration = paste0(y,"_",x)
    
    db |>
      mutate(y_3 = coalesce(y_3,0),
             y_5 = coalesce(y_5,0)) %>%
      dplyr::mutate(dplyr::across(
        .cols = dplyr::matches("^Sem_|^Cog_"),
        .fns = ~ as.numeric(scale(.))
      )) %>%
      MASS::glm.nb(
        as.formula(paste0(y,
                          " ~ journal * ",
                          x,
                          " - ",
                          x,
                          " + offset(log(n_authors))")),
        .
      ) |>
      tidy() |>
    transmute(
      journal = term |> str_remove("journal"),
      !!iteration := estimate
    ) |>
      right_join(estimates) -> estimates_2
  } 
}

estimates  %>%
  mutate(across(where(is.numeric), ~coalesce(., 0))) %>%
  mutate(across(where(is.numeric), ~scale(.) %>% as.vector())
         ) -> estimates_2 

estimates_2 %>%
  pivot_longer(
    cols = -journal,
    names_to = "regression",
    values_to = "Deviation"
  ) |>
  summarise(
    score = mean(Deviation),
    error = sd(Deviation),
    .by = journal
  ) |>
  arrange(-score) %>%
  janitor::adorn_rounding(2) |> View()

for (y in (c("y_3","y_5"))) {
  for (x in db |>
       dplyr::select(starts_with("Sem_") | starts_with("Cog_")) %>%
       names()) {
    
    iteration = paste0(y,"_",x)
    
    db |>
      mutate(y_3 = coalesce(y_3,0),
             y_5 = coalesce(y_5,0)) %>%
      group_by(journal) %>%
      dplyr::mutate(dplyr::across(
        .cols = dplyr::matches("^Sem_|^Cog_"),
        .fns = ~ as.numeric(scale(.))
      )) %>%
      do(model = fixest::fenegbin(
        as.formula(paste0(y,
                          " ~",
                          x,
                          " - 1 + offset(log(n_authors))")),
        
        data = .) |>
           tidy()) |>
      as_tibble() |>
      unnest(model) |>
      filter(term != ".theta") |>
      transmute(
        journal, 
        !!iteration := estimate
      ) |>
      right_join(estimates) -> estimates
    
  } 
}

estimates  %>%
  mutate(across(where(is.numeric), ~coalesce(., 0))) %>%
  mutate(across(where(is.numeric), ~scale(.) %>% as.vector())
  ) -> estimates_2 

estimates %>%
  pivot_longer(
    cols = -journal,
    names_to = "regression",
    values_to = "Deviation"
  ) |>
  summarise(
    score = mean(Deviation),
    error = sd(Deviation),
    .by = journal
  ) |>
  arrange(-score) %>%
  janitor::adorn_rounding(2) |> View()
