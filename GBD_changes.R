### Comparación categorías nivel 2: GBD-2017 y GBD-2023
### Autora: Tamara Ricardo
# Última modificación: 24-08-2026 13:21

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## GBD-2017 -----
gbd17_raw <- import(
  "extra/IHME_GBD_2017_ICD_CAUSE_MAP_CAUSES_OF_DEATH_Y2018M11D08.XLSX",
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

    # Seleccionar causas nivel 2
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
          "Diabetes and kidney diseases",
          "Skin and subcutaneous diseases",
          "Musculoskeletal disorders",
          "Other non-communicable diseases",
          "Transport injuries",
          "Unintentional injuries",
          "Self-harm and interpersonal violence",
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
gbd17 <- clean_gbd(gbd17_raw)

gbd23 <- clean_gbd(gbd23_raw)

## Códigos añadidos y removidos 2023 -----
gbd <- full_join(
  gbd17 |> rename(causa17 = cause),
  gbd23 |> rename(causa23 = cause)
) |>

  mutate(
    cat = case_when(
      causa17 == causa23 ~ "Sin cambios",
      is.na(causa23) ~ "Quitado 2023",
      is.na(causa17) ~ "Añadido 2023",
      .default = "Modificado 2023"
    )
  ) |>

  select(icd10, everything())


# Guardar datos ----------------------------------------------------------
export(gbd, "extra/codigos_gbd_17_23.xlsx")

## Limpiar environment -----
rm(list = ls())
