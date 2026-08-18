# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## GBD-2019 -----
gbd19_raw <- import(
  "extra/IHME_GBD_2019_COD_CAUSE_ICD_CODE_MAP_Y2020M10D15.XLSX",
  skip = 1
)


## GBD-2023 -----
gbd23_raw <- import(
  "extra/IHME_GBD_2023_COD_CAUSE_ICD_CODE_MAP_Y2025M10D12.XLSX",
  skip = 1
)


# Función auxiliar de limpieza -------------------------------------------
clean_gbd <- function(x) {
  x |>
    # Estandarizar nombres de columnas
    clean_names() |>

    # Descartar códigos CIE9
    select(-3) |>

    # Seleccionar causas nivel 2 y 3
    filter(
      cause %in%
        c(
          "HIV/AIDS and sexually transmitted infections",
          "Respiratory infections and tuberculosis",
          "Enteric infections",
          "Neglected tropical diseases and malaria",
          "Other infectious diseases",
          "Maternal and neonatal disorders",
          "Nutritional deficiencies",
          "Neoplasms",
          "Cardiovascular diseases",
          "Chronic respiratory diseases",
          "Digestive diseases",
          "Neurological disorders",
          "Mental disorders",
          "Substance use disorders",
          "Diabetes mellitus",
          "Chronic kidney disease",
          "Skin and subcutaneous diseases",
          "Musculoskeletal disorders",
          "Other non-communicable diseases",
          "Road injuries",
          "Other transport injuries",
          "Unintentional injuries",
          "Self-harm",
          "Interpersonal violence",
          "Conflict and terrorism",
          "Police conflict and executions",
          "Garbage Code (GBD Level 1)",
          "Garbage Code (GBD Level 2)",
          "Garbage Code (GBD Level 3)",
          "Garbage Code (GBD Level 4)"
        )
    ) |>

    # Limpieza y separación en filas individuales
    separate_longer_delim(icd10, delim = ", ")
}


# Limpiar datos ----------------------------------------------------------
gbd19 <- clean_gbd(gbd19_raw)

gbd23 <- clean_gbd(gbd23_raw)

## Códigos añadidos y removidos 2023 -----
gbd <- full_join(
  gbd23,
  gbd19 |> rename(cause19 = cause)
) |>

  mutate(
    cat = case_when(
      is.na(cause) ~ "Removed 2023",
      is.na(cause19) ~ "Added 2023",
      cause != cause19 ~ "Recoded 2023",
      .default = "Unchanged"
    )
  ) |>

  select(contains("cau"), cat, icd10)


# Guardar datos ----------------------------------------------------------
export(gbd, "clean/codigos_gbd_19_23.xlsx")
