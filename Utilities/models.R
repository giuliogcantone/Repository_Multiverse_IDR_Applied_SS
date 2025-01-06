negbin_FE <- function(formula, data) {
  library(fixest)
  fixest::fenegbin(
    fml = as.formula(paste0(formula, "| journal")),
    data = data,
    notes = FALSE
  )
}

negbin_noFE <- function(formula, data) {
  library(fixest)
  fixest::fenegbin(
    fml = as.formula(paste0(formula, "")),
    data = data,
    notes = FALSE
  )
}

clean_multiverse <- function(db) {
    db |>
      mutate(x = x |> str_remove("I\\(") |> str_remove("\\^2\\)")) |>
      separate(x, into = c("Stylisation", "Indicator"), sep = "_") |>
      transmute(
        Cluster,
        Stylisation,
        Indicator,
        Dimension = case_when(
          Indicator == "k" ~ "Variety",
          Indicator %in% c("RR","GINI") ~ "Balance",
          Indicator %in% c("LC","LWBR") ~ "Integration",
          Indicator %in% c("HEL","HZ") ~ "Nonconformity"),
        Lag = y |> factor(),Year = pub_year |> factor(), FE,
        N_author = ifelse(controls == "no covariates",0,1) |> factor(),
        Impact = exp(estimate) - 1,
        SE = std.error,
        L.bound = exp(conf.low) - 1,
        U.bound = exp(conf.high) - 1,
        n.obs = fit_nobs,
        p = p.value,
        BIC = fit_BIC
      ) |>
      filter(!Year == "all")
  }

omega_fun <- function(formula,file,model_name){
  model_name <- ensym(model_name)
  effectsize::omega_squared(
    aov(
      paste0("Impact  ~ Cluster + Year + Lag + FE + N_author + ",formula)|>as.formula(),
      data = fread(file) |>
        mutate(
          Approach = case_when(
            Stylisation %in% c("JCR","Scopus") ~ "Cognitive",
            TRUE ~ "Semantic"))
    )) |> as_tibble() |>
    select(Parameter,
           !!model_name := Omega2_partial)
  }
