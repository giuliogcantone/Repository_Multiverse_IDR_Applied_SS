{r}
papers %>%
  sample_n(nrow(.), replace = FALSE) %>%
  reframe(
    Max_Div_Raw_0 = first(paper[Div_Raw_0 == max(Div_Raw_0)]),
    Min_Div_Raw_0 = first(paper[Div_Raw_0 == min(Div_Raw_0)]),
    Max_Div_Naive_0 = first(paper[Div_Naive_0 == max(Div_Naive_0)]),
    Min_Div_Naive_0 = first(paper[Div_Naive_0 == min(Div_Naive_0)]),
    Max_Div_Correct_0 = first(paper[Div_Correct_0 == max(Div_Correct_0)]),
    Min_Div_Correct_0 = first(paper[Div_Correct_0 == min(Div_Correct_0)]),
    Max_Div_Raw_1 = first(paper[Div_Raw_1 == max(Div_Raw_1)]),
    Min_Div_Raw_1 = first(paper[Div_Raw_1 == min(Div_Raw_1)]),
    Max_Div_Naive_1 = first(paper[Div_Naive_1 == max(Div_Naive_1)]),
    Min_Div_Naive_1 = first(paper[Div_Naive_1 == min(Div_Naive_1)]),
    Max_Div_Correct_1 = first(paper[Div_Correct_1 == max(Div_Correct_1)]),
    Min_Div_Correct_1 = first(paper[Div_Correct_0 == min(Div_Correct_0)]),
    Max_phi_e_Raw_0 = first(paper[phi_e_Raw_0 == max(phi_e_Raw_0)]),
    Min_phi_e_Raw_0 = first(paper[phi_e_Raw_0 == min(phi_e_Raw_0)]),
    Max_phi_e_Naive_0 = first(paper[phi_e_Naive_0 == max(phi_e_Naive_0)]),
    Min_phi_e_Naive_0 = first(paper[phi_e_Naive_0 == min(phi_e_Naive_0)]),
    Max_phi_e_Correct_0 = first(paper[phi_e_Correct_0 == max(phi_e_Correct_0)]),
    Min_phi_e_Correct_0 = first(paper[phi_e_Correct_0 == min(phi_e_Correct_0)]),
    Max_phi_e_Raw_1 = first(paper[phi_e_Raw_1 == max(phi_e_Raw_1)]),
    Min_phi_e_Raw_1 = first(paper[phi_e_Raw_1 == min(phi_e_Raw_1)]),
    Max_phi_e_Naive_1 = first(paper[phi_e_Naive_1 == max(phi_e_Naive_1)]),
    Min_phi_e_Naive_1 = first(paper[phi_e_Naive_1 == min(phi_e_Naive_1)]),
    Max_phi_e_Correct_1 = first(paper[phi_e_Correct_1 == max(phi_e_Correct_1)]),
    Min_phi_e_Correct_1 = first(paper[phi_e_Correct_0 == min(phi_e_Correct_0)]),
    .by = groups
  ) -> prova

prova %>%
  pivot_longer(cols = starts_with("Min") | starts_with("Max"),
               values_to = "paper") %>%
  arrange(groups) %>%
  mutate(
    max = ifelse(startsWith(name,"Max"),1,0),
    name = str_remove(name,"Max_|Min_")
  ) %>%
  arrange(groups,name) %>%
  left_join(papers %>%
              select(paper,y_1,y_3,y_5)) -> prova2

prova2 %>%
  filter(n_distinct(paper) == 1,
         .by = c(groups,name)) %>%
  arrange(groups,name,max) %>%
  count(name) %>%
  arrange(-n) %>%
  left_join(papers %>%
              select(paper,y_1,y_3,y_5)) -> prova2


###

read_xlsx("extended_papers_db.xlsx") -> papers

papers %>%
  summarise(
    n_paired = n(),
    cluster = cur_group_id(),
    .by = c(publication_date,Journal)) %>%
  right_join(papers) %>%
  mutate(pair = case_when(
    n_paired == 1 ~ "0",
    n_paired > 1 ~ cluster %>% as.character()
  )) -> papers

papers %>%
  filter(
    n_paired > 2
  ) %>%
  summarise(
    n_paired_2 = n(),
    cluster_2 = cur_group_id(),
    .by = c(pair,n_authors_class)) %>%
  right_join(papers) %>%
  mutate(
    pair = case_when(
      n_paired < 3 ~ pair, 
      n_paired_2 == 1 ~ "0",
      n_paired_2 == 2 ~ cluster %>% as.character(),
      n_paired_2 > 2 ~ str_c(cluster,"-",cluster_2)
    )
  ) %>%
  select(-c(n_paired,
            n_paired_2,
            cluster,
            cluster_2)) -> papers

papers %>%
  filter(pair != 0) %>%
  pivot_longer(cols = starts_with("Div_") | starts_with("phi_"),
               values_to = "value",
               names_to = "Measure") %>%
  mutate(X = ifelse(value >= median(value),1,0),
         .by = c(Measure,pair),
         .before = 1) %>%
  arrange(Measure,pair,X) -> match_db

###

match_db[18:36] %>%
  names

match_db %>%
  matchit(
    formula = as.formula(X ~ pair + Measure + Economics + Business + `Math.` + Sociology + Psychology + `Comp. Sci.` + `Law & Pol.`),
    data = .,
    distance = "mahalanobis",
    method = "nearest",
    exact = c("Measure","pair")
  ) -> matched_data

match.data(matched_data) %>%
  select(Measure,subclass,paper,title,pub_year,
         X,y_1,y_3,y_5) -> match_db

###

