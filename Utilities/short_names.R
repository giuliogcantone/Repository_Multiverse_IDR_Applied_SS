pacman::p_load(
  tidyverse,
  fastverse,
  readxl,
  writexl
)

abbreviations_Scopus <- function(x,i) {
  column_sym <- ensym(i)
  
  x |>
    mutate(
      !!column_sym :=
        case_match(
          !!column_sym,
          "Agricultural and Biological Sciences" ~ "AGRBIO",
          "Arts and Humanities" ~ "ARTHUM",
          "Biochemistry, Genetics and Molecular Biology" ~ "BIOCHEM",
          "Business, Management and Accounting" ~ "BUSIN",
          "Chemical Engineering" ~ "CHEMENG",
          "Chemistry" ~ "CHEM",
          "Computer Science" ~ "COMP",
          "Decision Sciences" ~ "DECISION",
          "Dentistry" ~ "DENTIST",
          "Earth and Planetary Sciences" ~ "EARTH",
          "Economics, Econometrics and Finance" ~ "ECON",
          "Energy" ~ "ENERG",
          "Engineering" ~ "ENGIN",
          "Environmental Science" ~ "ENVIR",
          "Health Professions" ~ "HEALPRO",
          "Immunology and Microbiology" ~ "MICROBIO",
          "Materials Science" ~ "MATERIALS",
          "Mathematics" ~ "MATH",
          "Medicine" ~ "MEDIC",
          "Neuroscience" ~ "NEURO",
          "Nursing" ~ "NURS",
          "Pharmacology, Toxicology and Pharmaceutics" ~ "PHARMA",
          "Physics and Astronomy" ~ "PHYS",
          "Psychology" ~ "PSYCH",
          "Social Sciences" ~ "SOCIOL",
          "Veterinary" ~ "VETERIN"
        )
    )
}


abbreviations_concepts <- function(x,i) {
  column_sym <- ensym(i)
  
  x |>
    mutate(
      !!column_sym :=
        case_match(
          !!column_sym,
          "Art" ~ "ART",
          "Biology" ~ "BIO",
          "Business" ~ "BUSIN",
          "Chemistry" ~ "CHEM",
          "Computer science" ~ "COMP",
          "Economics" ~ "ECON",
          "Engineering" ~ "ENGIN",
          "Environmental science" ~ "ENVIR",
          "Geography" ~ "GEOGR",
          "Geology" ~ "GEOL",
          "History" ~ "HIST",
          "Mathematics" ~ "MATH",
          "Materials science" ~ "MATERIALS",
          "Medicine" ~ "MEDIC",
          "Philosophy" ~ "PHILO",
          "Physics" ~ "PHYS",
          "Political science" ~ "LAWPOL",
          "Psychology" ~ "PSYCH",
          "Sociology" ~ "SOCIOL"
        )
    )
}

abbreviations_WoS <- function(x, i) {
  column_sym <- ensym(i)
  
  x |>
    mutate(
      !!column_sym :=
        case_match(
          !!column_sym,
          "Agricultural Sciences" ~ "AGRIC",
          "Arts & Humanities, Interdisciplinary" ~ "ARTHUM",
          "Biology & Biochemistry" ~ "BIO",
          "Chemistry" ~ "CHEM",
          "Clinical Medicine" ~ "MEDIC",
          "Computer Science" ~ "COMP",
          "Economics & Business" ~ "ECBUS",
          "Engineering" ~ "ENGIN",
          "Environment/Ecology" ~ "ENVIRON",
          "Geosciences" ~ "EARTH",
          "History & Archaeology" ~ "HIST",
          "Literature & Language" ~ "LANG",
          "Materials Science" ~ "MATERIALS",
          "Mathematics" ~ "MATH",
          "Philosophy & Religion" ~ "PHILO",
          "Physics" ~ "PHYS",
          "Plant & Animal Science" ~ "PLANTANIM",
          "Psychiatry/Psychology" ~ "PSYCH",
          "Social Sciences, General" ~ "SOCIOL",
          "Visual & Performing Arts" ~ "VISART"
        )
    )
}
