### Comparación categorías nivel 2: GBD-2019 y GBD-2023
### Autora: Tamara Ricardo
# Última modificación: 03-09-2026 14:49

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)

# Función auxiliar de limpieza -------------------------------------------
clean_gbd <- function(x) {
  x |>
    # --- Estandarizar nombres de columnas ---
    clean_names() |>

    # --- Seleccionar causas nivel 2 ---
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

    # --- Traducir categorías ---
    mutate(
      causa = case_when(
        str_detect(cause, "Cardio") ~ "Cardiovasculares",
        str_detect(cause, "Chronic") ~ "Respiratorias crónicas",
        str_detect(cause, "Diab") ~ "Diabetes y renales crónicas",
        str_detect(cause, "Dig") ~ "Digestivas",
        str_detect(cause, "Ente") ~ "Entéricas",
        str_detect(cause, "HIV") ~ "VIH/SIDA e ITS",
        str_detect(cause, "Mat") ~ "Maternas y neonatales",
        str_detect(cause, "Ment") ~ "Mentales",
        str_detect(cause, "Musc") ~ "Musculoesqueléticas",
        str_detect(
          cause,
          "Neg"
        ) ~ "Malaria y enfermedades tropicales desatendidas",
        str_detect(cause, "Neop") ~ "Neoplasias",
        str_detect(cause, "Neur") ~ "Neurológicas",
        str_detect(cause, "Nutr") ~ "Nutricionales",
        str_detect(cause, "Other i") ~ "Otras infecciosas",
        str_detect(cause, "Other n") ~ "Otras ENT",
        str_detect(cause, "Resp") ~ "Tuberculosis y respiratorias",
        str_detect(cause, "Self") ~ "Suicidio y violencia interpersonal",
        str_detect(cause, "Skin") ~ "Piel y subcutáneas",
        str_detect(cause, "Subs") ~ "Uso de sustancias",
        str_detect(cause, "Tran") ~ "Lesiones por transporte",
        str_detect(cause, "Unin") ~ "Lesiones no intencionales",
        .default = cause
      )
    ) |>

    # --- Limpieza y separación en filas individuales ---
    separate_longer_delim(icd10, delim = ", ") |>

    # --- Separar en columnas ---
    separate_wider_delim(
      icd10,
      names = c("begin", "end"),
      delim = "-",
      too_few = "align_start",
      cols_remove = FALSE
    ) |>

    # --- Seleccionar columnas ---
    select(-icd9, -cause)
}


# Cargar datos -----------------------------------------------------------
## GBD-2017 -----
gbd17_raw <- import(
  "extra/IHME_GBD_2017_ICD_CAUSE_MAP_CAUSES_OF_DEATH_Y2018M11D08.XLSX",
  skip = 1
)

# ## GBD-2019 -----
# gbd19_raw <- import(
#   "extra/IHME_GBD_2019_COD_CAUSE_ICD_CODE_MAP_Y2020M10D15.XLSX",
#   skip = 1
# )

# ## GBD-2021 -----
# gbd21_raw <- import(
#   "extra/IHME_GBD_2021_COD_CAUSE_ICD_CODE_MAP_Y2024M05D16.XLSX",
#   skip = 1
# )

## GBD-2023 -----
gbd23_raw <- import(
  "extra/IHME_GBD_2023_COD_CAUSE_ICD_CODE_MAP_Y2025M10D12.XLSX",
  skip = 1
)


# Limpiar y unir datos ---------------------------------------------------
tab_gbd <- clean_gbd(gbd17_raw) |>
  rename(causa17 = causa) |>

  # --- Unir datos 2023 ---
  full_join(
    clean_gbd(gbd23_raw) |>
      rename(causa23 = causa)
  ) |>

  # --- Detectar cambios ---
  mutate(
    cat = case_when(
      if_all(c(causa23), ~ .x == causa17) ~ "Sin cambios",

      .default = "Modificado 2023"
    )
  ) |>

  filter_out(cat == "Sin cambios") |>

  # --- Ordenar ---
  arrange(begin, causa17)

# Guardar datos ----------------------------------------------------------
export(tab_gbd, "extra/tabla_s13_gbd.xlsx")

## Limpiar environment -----
rm(list = ls())
