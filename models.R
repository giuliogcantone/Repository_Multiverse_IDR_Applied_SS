negbin_FE <- function(formula, data) {
  library(fixest)
  fixest::fenegbin(
    fml = as.formula(paste0(formula, "+ offset(log(n_authors)) | journal")),
    data = data,
    notes = FALSE
  )
}

negbin_noFE <- function(formula, data) {
  library(fixest)
  fixest::fenegbin(
    fml = as.formula(paste0(formula, "+ offset(log(n_authors))")),
    data = data,
    notes = FALSE
  )
}

logistic_binom <- function(formula, data) {
  glm(as.formula(paste0(formula, "- 1")), data, family = binomial())
}

