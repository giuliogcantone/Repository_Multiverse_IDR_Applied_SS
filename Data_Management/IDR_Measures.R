pacman::p_load(
  tidyverse,readxl,writexl,ineq
)
source("Utilities/short_names.R")
source("Utilities/journals_preprocess.R")

hellinger_distance <- function(p, q) {
  p <- p / sum(p)
  q <- q / sum(q)
  sum_term <- sum((sqrt(p) - sqrt(q))^2)
  H <- sqrt(sum_term / 2)
  return(H)
}

IDR_measures <- function(p_source, e_source, prefix, simil_m) {
  p_source |>
    summarise(
      .by = paper,
      k = n(),
      DRR = 1-sum(p^2),
      DGINI = 1-Gini(p, corr = T),
      DGINI = ifelse(is.nan(DGINI),0,DGINI)
    ) -> result
  
  p_source |>
    select(-journal) |>
    inner_join(p_source |> select(-journal), by = "paper",
               relationship = "many-to-many") %>%
    rename(i = i.x, p_i = p.x,
           j = i.y, p_j = p.y) |>
    left_join(
      relationship = "many-to-many",
      readxl::read_xlsx(
        simil_m)
      ) -> crossed
  
  crossed |>
    summarise(
      .by = paper, 
      RS = sum(p_i*p_j*(1-z)),
      DLC = 1 / (1 - RS),
      sum_zetaLWB = sum(1 - z)
    ) |>
    right_join(result) |>
    transmute(
      paper,
      k, DRR ,DGINI,
      DLC,
      LWBR = k*DGINI * (sum_zetaLWB / (k * (k-1)))
    ) -> result
  
  crossed |> 
    transmute(paper,i,p_i,j,z,
              phi_i = p_i * z) |>
    summarise(phi_i = sum(phi_i),
              .by = c(paper,i)) |>
    mutate(phi_i = phi_i/sum(phi_i),
           .by = paper)%>%
    left_join(p_source,.) -> phi
  
  e_source |>
    inner_join(e_source, by = "journal",
               relationship = "many-to-many") %>%
    rename(i = i.x, e_i = e.x,
           j = i.y, e_j = e.y) |>
    left_join(
      relationship = "many-to-many",
      readxl::read_xlsx(simil_m)) |>
    transmute(journal,i,e_i,j,z,
              phie_i = e_i * z) |>
    summarise(phie_i = sum(phie_i),
              .by = c(journal,i)) |>
    mutate(phie_i = phie_i/sum(phie_i),
           .by = journal) -> phie
  
  e_source |>
    full_join(distinct(phi,paper,journal),
              relationship = "many-to-many") |>
    full_join(phi,
              relationship = "many-to-many") |>
    left_join(phie) |>
    mutate(
      phi_i = coalesce(phi_i,0),
      phie_i = coalesce(phie_i,0),
      p = coalesce(p,0),
      e = coalesce(e,0)) |>
    transmute(paper,i,p,phi = phi_i,
              journal,e, phie = phie_i) |>
    arrange(paper) -> paired
  
  paired |>
#    mutate(
#      a = (sqrt(p) - sqrt(e))^2,
#      b = (sqrt(phi) - sqrt(phie))^2
#    ) |>
    summarise(
      .by = paper,
      HELL = hellinger_distance(p,e),
      HELLphi = hellinger_distance(phi,phie)
    ) %>%
    left_join(result,.) |>
    rename(
      X_k = k,
      X_RR = DRR,
      X_GINI = DGINI,
      X_LC = DLC,
      X_LWBR = LWBR,
      X_HEL = HELL,
      X_HZ = HELLphi
    ) |>
    mutate(across(where(is.numeric),
                  ~ replace(., is.nan(.),
                            0))) |>
    rename_with(~ gsub("^X_",
                       paste0(prefix,"_"),
                       .))
  
}
