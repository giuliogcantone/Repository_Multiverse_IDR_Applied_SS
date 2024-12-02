papers_profiles %>%
  filter(paper %in% c("https://openalex.org/W1554610666",
                      "https://openalex.org/W2168483283")
         )%>%
  filter(level == 0) %>%
  summarize(crossed = list(crossing(i, i)),
            .by = paper) %>%
  unnest(crossed) %>%
  rename(
    i = i...1,
    j = i...2
  ) %>%
  left_join(papers_profiles %>% transmute(
    paper,
    i = i,
    p_i = p_o
  )
  ) %>%
  left_join(papers_profiles %>% transmute(
    paper,
    j = i,
    p_j = p_o
  )
  ) %>%
  mutate(d_Raw_0 = map2_dbl(i,j,~Z$Raw_0[.x,.y]),
         d_Naive_0 = map2_dbl(i,j,~Z$Naive_0[.x,.y]),
         d_Correct_0 = map2_dbl(i,j,~Z$Correct_0[.x,.y]),
         phi_Z_Raw = map2_dbl(p_j,i,~.x * sum(d_Raw_0[i == .y])),
         phi_Z_Naive = map2_dbl(p_j,i,~.x * sum(d_Naive_0[i == .y])),
         phi_Z_Correct = map2_dbl(p_j,i,~.x * sum(d_Correct_0[i == .y])),
         .by = paper,
  ) %>%
  arrange(paper,j)%>%
  View()
  summarise(.by = paper,
            X_Div_k_Raw_0 = sum(p_i*(1 / sum(p_j * d_Raw_0))),
            X_Div_k_Naive_0 = sum(p_i*(1 / sum(p_j * d_Naive_0))),
            X_Div_k_Correct_0 = sum(p_i*(1 / sum(p_j * d_Correct_0))),
            X_Div_Bal_Raw_0 = 1 / sum(p_i * p_j * d_Raw_0),
            X_Div_Bal_Naive_0 = 1 / sum(p_i * p_j * d_Naive_0),
            X_Div_Bal_Correct_0 = 1 / sum(p_i * p_j * d_Correct_0),
            X_Div_Domi_Raw_0 = 1/max(phi_Z_Raw),
            X_Div_Domi_Naive_0 = 1/max(phi_Z_Naive),
            X_Div_Domi_Correct_0 = 1 /max(phi_Z_Correct)
  ) %>% View()
  
  X_Div_Domi_Raw_0 = 1 / max(p_i * sum(d_Raw_0[j == i[p_i]])),
  X_Div_Domi_Naive_0 = 1 / max(p_i * sum(d_Naive_0[j == i[p_i]])),
  X_Div_Domi_Correct_0 = 1 / max(p_i * sum(d_Correct_0[j == i[p_i]]))
  
  
  
  tibble(
    a = c(1,2,2,3,4,5),
    Jou = c("A","A","A","C","c","A"),
    mm = c("A","A","B","A","B","C"),
  ) %>%
    mutate(au = as.integer(a > median(a)),
           .by = c(Jou,mm))
  