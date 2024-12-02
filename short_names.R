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
          "Chemical Engineering" ~ "CHEMENG" ,
          "Chemistry" ~ "CHEM",
          "Computer Science" ~ "COMP",
          "Decision Sciences" ~ "DECISION",
          "Earth and Planetary Sciences" ~ "EARTH",
          "Economics, Econometrics and Finance" ~ "ECON",
          "Energy" ~ "ENERG",
          "Engineering" ~ "ENGIN",
          "Environmental Science" ~ "ENVIR",
          "Immunology and Microbiology" ~ "MICROBIO",
          "Materials Science" ~ "MATERIALS",
          "Mathematics" ~ "MATH",
          "Medicine" ~ "MEDIC",
          "Neuroscience" ~ "NEURO",
          "Nursing" ~ "NURS",
          "Pharmacology, Toxicology and Pharmaceutics" ~ "PHARMA",
          "Physics and Astronomy" ~ "PHYS",
          "Psychology" ~ "PSHYC",
          "Social Sciences" ~ "SOCIOL",
          "Veterinary" ~ "VETERIN",
          "Dentistry" ~ "DENTIST",
          "Health Professions" ~ "HEALPRO"
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
          "Philosophy" ~ "PHIL",
          "Physics" ~ "PHYS",
          "Political science" ~ "LAWPOL",
          "Psychology" ~ "PSYCH",
          "Sociology" ~ "SOCIOL",
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
          "Materials Science" ~ "MATERIALS",
          "Biology & Biochemistry" ~ "BIO",
          "Engineering" ~ "ENGIN",
          "Economics & Business" ~ "ECON",
          "Mathematics" ~ "MATH",
          "Geosciences" ~ "EARTH",
          "Chemistry" ~ "CHEM",
          "Clinical Medicine" ~ "MEDIC",
          "Computer Science" ~ "COMP",
          "Physics" ~ "PHYS",
          "Social Sciences, General" ~ "SOCIOL",
          "Philosophy & Religion" ~ "PHILO",
          "Visual & Performing Arts" ~ "VISART",
          "Environment/Ecology" ~ "ENVIRON",
          "Plant & Animal Science" ~ "PLANTANIM",
          "Agricultural Sciences" ~ "AGRIC",
          "Psychiatry/Psychology" ~ "PSYCH",
          "Arts & Humanities, Interdisciplinary" ~ "ARTHUM",
          "History & Archaeology" ~ "HIST",
          "Literature & Language" ~ "LANG"
        )
    )
}

tibble(a = "Physics") |>
  abbreviations_Scopus(a)
