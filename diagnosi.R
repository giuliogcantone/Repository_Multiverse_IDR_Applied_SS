papers_profiles %>%
  filter(
    paper == "https://openalex.org/W2082127127"
  ) %>%
  arrange(Journal, paper, i) %>%
  filter(level == 0) %>%
  mutate(z_Raw_0 = map(i, ~ Z$Raw_0[.x,i]),
         z_Naive_0 = map(i, ~ Z$Naive_0[.x,i]),
         z_Correct_0 = map(i, ~ Z$Correct_0[.x,i]),
         .by = paper) %>%
  mutate(p_e_Raw_0 = map_dbl(z_Raw_0, ~ sum(.x * p_e)),
         p_e_Naive_0 = map_dbl(z_Naive_0, ~ sum(.x * p_e)),
         p_e_Correct_0 = map_dbl(z_Correct_0, ~ sum(.x * p_e)),
         p_o_Raw_0 = map_dbl(z_Raw_0, ~ sum(.x * p_o)),
         p_o_Naive_0 = map_dbl(z_Naive_0, ~ sum(.x * p_o)),
         p_o_Correct_0 = map_dbl(z_Correct_0, ~ sum(.x * p_o)),
         p_year_Raw_0 = map_dbl(z_Raw_0, ~ sum(.x * p_year)),
         p_year_Naive_0 = map_dbl(z_Naive_0, ~ sum(.x * p_year)),
         p_year_Correct_0 = map_dbl(z_Correct_0, ~ sum(.x * p_year)),
         p_e_Raw_0 = p_e_Raw_0/sum(p_e_Raw_0),
         p_e_Naive_0 = p_e_Naive_0/sum(p_e_Naive_0),
         p_e_Correct_0 = p_e_Correct_0/sum(p_e_Correct_0),
         p_o_Raw_0 = p_o_Raw_0/sum(p_o_Raw_0),
         p_o_Naive_0 = p_o_Naive_0/sum(p_o_Naive_0),
         p_o_Correct_0 = p_o_Correct_0/sum(p_o_Correct_0),
         p_year_Raw_0 = p_year_Raw_0/sum(p_year_Raw_0),
         p_year_Naive_0 = p_year_Naive_0/sum(p_year_Naive_0),
         p_year_Correct_0 = p_year_Correct_0/sum(p_year_Correct_0),
         .by = paper) %>%
  summarise(
    phi_e_Raw_0 = sum(abs(p_e_Raw_0 - p_o_Raw_0)/((abs(p_e_Raw_0) + abs(p_o_Raw_0))*2), na.rm = T),
    phi_e_Naive_0 = sum(abs(p_e_Naive_0 - p_o_Naive_0)/((abs(p_e_Naive_0) + abs(p_o_Naive_0))*2), na.rm = T),
    phi_e_Correct_0 = sum(abs(p_e_Correct_0 - p_o_Correct_0)/((abs(p_e_Correct_0) + abs(p_o_Correct_0))*2), na.rm = T),
    phi_year_Raw_0 = sum(abs(p_year_Raw_0 - p_o_Raw_0)/((abs(p_year_Raw_0) + abs(p_o_Raw_0))*2), na.rm = T),
    phi_year_Naive_0 = sum(abs(p_year_Naive_0 - p_o_Naive_0)/((abs(p_year_Naive_0) + abs(p_o_Naive_0))*2), na.rm = T),
    phi_year_Correct_0 = sum(abs(p_year_Correct_0 - p_o_Correct_0)/((abs(p_year_Correct_0) + abs(p_o_Correct_0))*2), na.rm = T),
    .by = paper
  ) %>%
  left_join(
    papers_profiles %>%
      filter(
        paper == "https://openalex.org/W2082127127"
      ) %>%
      filter(level == 1) %>%
      arrange(Journal, paper, i) %>%
      mutate(z_Raw_1 = map(i, ~ Z$Raw_1[.x,i]),
             z_Naive_1 = map(i, ~ Z$Naive_1[.x,i]),
             z_Correct_1 = map(i, ~ Z$Correct_1[.x,i]),
             .by = paper) %>%
      mutate(p_e_Raw_1 = map_dbl(z_Raw_1, ~ sum(.x * p_e)),
             p_e_Naive_1 = map_dbl(z_Naive_1, ~ sum(.x * p_e)),
             p_e_Correct_1 = map_dbl(z_Correct_1, ~ sum(.x * p_e)),
             p_o_Raw_1 = map_dbl(z_Raw_1, ~ sum(.x * p_o)),
             p_o_Naive_1 = map_dbl(z_Naive_1, ~ sum(.x * p_o)),
             p_o_Correct_1 = map_dbl(z_Correct_1, ~ sum(.x * p_o)),
             p_year_Raw_1 = map_dbl(z_Raw_1, ~ sum(.x * p_year)),
             p_year_Naive_1 = map_dbl(z_Naive_1, ~ sum(.x * p_year)),
             p_year_Correct_1 = map_dbl(z_Correct_1, ~ sum(.x * p_year)),
             p_e_Raw_1 = p_e_Raw_1/sum(p_e_Raw_1),
             p_e_Naive_1 = p_e_Naive_1/sum(p_e_Naive_1),
             p_e_Correct_1 = p_e_Correct_1/sum(p_e_Correct_1),
             p_o_Raw_1 = p_o_Raw_1/sum(p_o_Raw_1),
             p_o_Naive_1 = p_o_Naive_1/sum(p_o_Naive_1),
             p_o_Correct_1 = p_o_Correct_1/sum(p_o_Correct_1),
             p_year_Raw_1 = p_year_Raw_1/sum(p_year_Raw_1),
             p_year_Naive_1 = p_year_Naive_1/sum(p_year_Naive_1),
             p_year_Correct_1 = p_year_Correct_1/sum(p_year_Correct_1),
             .by = paper) %>%
      summarise(
        phi_e_Raw_1 = sum(abs(p_e_Raw_1 - p_o_Raw_1)/((abs(p_e_Raw_1) + abs(p_o_Raw_1))*2), na.rm = T),
        phi_e_Naive_1 = sum(abs(p_e_Naive_1 - p_o_Naive_1)/((abs(p_e_Naive_1) + abs(p_o_Naive_1))*2), na.rm = T),
        phi_e_Correct_1 = sum(abs(p_e_Correct_1 - p_o_Correct_1)/((abs(p_e_Correct_1) + abs(p_o_Correct_1))*2), na.rm = T),
        phi_year_Raw_1 = sum(abs(p_year_Raw_1 - p_o_Raw_1)/((abs(p_year_Raw_1) + abs(p_o_Raw_1))*2), na.rm = T),
        phi_year_Naive_1 = sum(abs(p_year_Naive_1 - p_o_Naive_1)/((abs(p_year_Naive_1) + abs(p_o_Naive_1))*2), na.rm = T),
        phi_year_Correct_1 = sum(abs(p_year_Correct_1 - p_o_Correct_1)/((abs(p_year_Correct_1) + abs(p_o_Correct_1))*2), na.rm = T),
        .by = paper
      )
  ) %>% View()

papers_profiles$paper %>% unique() %>% .[1:10]

papers_profiles %>%
  filter(
    paper %in% (papers_profiles$paper %>% unique() %>% .[1:10])
  ) %>%
  filter(level == 1) %>%
  arrange(Journal, paper, i) %>%
  mutate(z_Raw_1 = map(i, ~ Z$Raw_1[.x,i]),
         z_Naive_1 = map(i, ~ Z$Naive_1[.x,i]),
         z_Correct_1 = map(i, ~ Z$Correct_1[.x,i]),
         .by = paper) %>%
  mutate(p_e_Raw_1 = map_dbl(z_Raw_1, ~ sum(.x * p_e)),
         p_e_Naive_1 = map_dbl(z_Naive_1, ~ sum(.x * p_e)),
         p_e_Correct_1 = map_dbl(z_Correct_1, ~ sum(.x * p_e)),
         p_o_Raw_1 = map_dbl(z_Raw_1, ~ sum(.x * p_o)),
         p_o_Naive_1 = map_dbl(z_Naive_1, ~ sum(.x * p_o)),
         p_o_Correct_1 = map_dbl(z_Correct_1, ~ sum(.x * p_o)),
         p_year_Raw_1 = map_dbl(z_Raw_1, ~ sum(.x * p_year)),
         p_year_Naive_1 = map_dbl(z_Naive_1, ~ sum(.x * p_year)),
         p_year_Correct_1 = map_dbl(z_Correct_1, ~ sum(.x * p_year)),
         p_e_Raw_1 = p_e_Raw_1/sum(p_e_Raw_1),
         p_e_Naive_1 = p_e_Naive_1/sum(p_e_Naive_1),
         p_e_Correct_1 = p_e_Correct_1/sum(p_e_Correct_1),
         p_o_Raw_1 = p_o_Raw_1/sum(p_o_Raw_1),
         p_o_Naive_1 = p_o_Naive_1/sum(p_o_Naive_1),
         p_o_Correct_1 = p_o_Correct_1/sum(p_o_Correct_1),
         p_year_Raw_1 = p_year_Raw_1/sum(p_year_Raw_1),
         p_year_Naive_1 = p_year_Naive_1/sum(p_year_Naive_1),
         p_year_Correct_1 = p_year_Correct_1/sum(p_year_Correct_1),
         .by = paper) -> prova

prova %>%
  select(p_year_Correct_1,p_o_Correct_1,paper) %>%
  mutate(
   phi_year_Correct_1 =
     (abs(p_year_Correct_1 - p_o_Correct_1)*(p_o_Correct_1)) /
     ((abs(p_year_Correct_1) + abs(p_o_Correct_1)))
   ) %>%
  summarise(phi = 1/sum(phi_year_Correct_1),
            .by = paper)
  
abs(prova$p_year_Correct_1[2] - prova$p_o_Correct_1[2]) /
  ((abs(prova$p_year_Correct_1[2]) + abs(prova$p_o_Correct_1[2]))*2)

abs(prova$p_year_Correct_1[2] - prova$p_o_Correct_1[2])

  summarise(
    phi_e_Raw_1 = sum(abs(p_e_Raw_1 - p_o_Raw_1)/((abs(p_e_Raw_1) + abs(p_o_Raw_1))*2), na.rm = T),
    phi_e_Naive_1 = sum(abs(p_e_Naive_1 - p_o_Naive_1)/((abs(p_e_Naive_1) + abs(p_o_Naive_1))*2), na.rm = T),
    phi_e_Correct_1 = sum(abs(p_e_Correct_1 - p_o_Correct_1)/((abs(p_e_Correct_1) + abs(p_o_Correct_1))*2), na.rm = T),
    phi_year_Raw_1 = sum(abs(p_year_Raw_1 - p_o_Raw_1)/((abs(p_year_Raw_1) + abs(p_o_Raw_1))*2), na.rm = T),
    phi_year_Naive_1 = sum(abs(p_year_Naive_1 - p_o_Naive_1)/((abs(p_year_Naive_1) + abs(p_o_Naive_1))*2), na.rm = T),
    phi_year_Correct_1 = sum(abs(p_year_Correct_1 - p_o_Correct_1)/((abs(p_year_Correct_1) + abs(p_o_Correct_1))*2), na.rm = T),
    .by = paper
  ) %>% View()
