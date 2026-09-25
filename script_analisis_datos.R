### Mortalidad por códigos garbage en Argentina (2010–2023):
### redistribución hacia causas específicas
### Análisis de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 25-09-2026 13:07

# Cargar paquetes --------------------------------------------------------
# remotes::install_github("https://github.com/datos-ine/joinpointR")
pacman::p_load(
  PHEindicatormethods,
  joinpointR,
  officer,
  flextable,
  scales,
  here,
  rio,
  janitor,
  tidyverse
)

# Cargar/preparar datos --------------------------------------------------
## Población estándar Argentina (2022) -----
pob_est_2022 <- import(here("clean", "arg_pob_est_2022.rds"))

## Proyecciones poblacionales Argentina (2010-2023) -----
proy_2010_2023 <- import(here("clean", "arg_proy_2010_2023.rds"))


## Defunciones por grupo de causas -----
datos_gc <- import(here("clean", "arg_recod_defun_gbd23.rds")) |>

  # Ordenar datos
  mutate(
    jurisd_deis = fct_relevel(
      jurisd_deis,
      "Buenos Aires",
      "CABA",
      "Córdoba",
      "Entre Ríos",
      "Santa Fe",
      "Mendoza",
      "Cuyo2",
      "Chaco",
      "Corrientes",
      "Formosa",
      "Misiones",
      "Tucumán"
    )
  )


#  Tema flextable -----------------------------------------------------------
tab_fmt <- function(x) {
  x |>
    bold(part = "header") |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 9, part = "all") |>
    line_spacing(space = 1.5, part = "all") |>
    valign(valign = "top") |>
    align(align = "left", part = "all") |>
    merge_v(j = 1) |>
    merge_v(j = 2:3, combine = TRUE) |>
    (\(ft) {
      idx <- which(ft$body$dataset[[2]] != dplyr::lag(ft$body$dataset[[2]])) - 1
      idx <- idx[!is.na(idx) & idx > 0]
      hline(ft, i = idx)
    })()
}


# Tasas estandarizadas ---------------------------------------------------
## ---- Total país ----
tasa_gc_arg <- datos_gc |>
  # --- Seleccionar muertes codificadas como GC ---
  filter(str_detect(gbd_paso1, "GC")) |>
  droplevels() |>

  # --- Modificar niveles paso 1 ---
  mutate(nivel = str_remove(gbd_paso1, ".*:")) |>

  # --- Frecuencias anuales ---
  count(
    anio,
    grupo_edad,
    nivel,
    wt = n
  ) |>

  # --- Unir con proyecciones poblacionales ---
  left_join(
    proy_2010_2023 |>
      # Agrupar por año
      count(
        anio,
        grupo_edad,
        wt = proy,
        name = "pob"
      )
  ) |>

  # --- Añadir población estándar 2022 ---
  left_join(pob_est_2022) |>

  # --- Calcular tasa estandarizada ---
  group_by(anio, nivel) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  ) |>

  # Crear etiqueta región
  mutate(region_deis = "Argentina")


## ---- Región DEIS ----
tasa_gc_reg <- datos_gc |>
  # --- Seleccionar muertes codificadas como GC ---
  filter(str_detect(gbd_paso1, "GC")) |>
  droplevels() |>

  # --- Modificar niveles paso 1 ---
  mutate(nivel = str_remove(gbd_paso1, ".*:")) |>

  # --- Frecuencias anuales ---
  count(
    anio,
    grupo_edad,
    region_deis,
    nivel,
    wt = n
  ) |>

  # --- Unir con proyecciones poblacionales ---
  left_join(
    proy_2010_2023 |>
      # Agrupar por año
      count(
        anio,
        region_deis,
        grupo_edad,
        wt = proy,
        name = "pob"
      )
  ) |>

  # --- Añadir población estándar 2022 ---
  left_join(pob_est_2022) |>

  # --- Calcular tasa estandarizada ---
  group_by(anio, region_deis, nivel) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  )


## ---- Jurisdicción DEIS ----
tasa_gc_jur <- datos_gc |>
  # --- Seleccionar muertes codificadas como GC ---
  filter(str_detect(gbd_paso1, "GC")) |>
  droplevels() |>

  # --- Modificar niveles paso 1 ---
  mutate(nivel = str_remove(gbd_paso1, ".*:")) |>

  # --- Frecuencias anuales ---
  count(
    anio,
    grupo_edad,
    region_deis,
    jurisd_deis,
    nivel,
    wt = n
  ) |>

  # --- Unir con proyecciones poblacionales ---
  left_join(
    proy_2010_2023 |>
      # Agrupar por año
      count(
        anio,
        region_deis,
        jurisd_deis,
        grupo_edad,
        wt = proy,
        name = "pob"
      )
  ) |>

  # --- Añadir población estándar 2022 ---
  left_join(pob_est_2022) |>

  # --- Calcular tasa estandarizada ---
  group_by(anio, region_deis, jurisd_deis, nivel) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  ) |>

  # --- Crear etiqueta para las tablas ---
  mutate(
    label = fct_cross(region_deis, jurisd_deis)
  )


# Regresión joinpoint ----------------------------------------------------
## ---- Argentina ----
mod_ar <- model_jp_grid(
  data = tasa_gc_arg,
  rate = value,
  time = anio,
  group = "nivel"
)


## ---- Región ----
mod_reg <- model_jp_grid(
  data = tasa_gc_reg,
  rate = value,
  time = anio,
  group = c("nivel", "region_deis")
)


# ---- Jurisdicción ----
mod_jur <- model_jp_grid(
  data = tasa_gc_jur,
  rate = value,
  time = anio,
  group = c("nivel", "label")
)


# ---- Tabla de coeficientes ----
tab_mod <- get_summary(mods = c(mod_ar, mod_reg, mod_jur)) |>
  # --- Separar columnas ---
  separate(
    col = model,
    into = c("nivel", "region", "jurisdiccion"),
    fill = "right",
    extra = "merge"
  ) |>

  # --- Reemplazar NAs ---
  mutate(region = replace_na(region, "Argentina")) |>

  # --- Redondear cifras ---
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 2))) |>

  # --- Unir CI del APC ---
  mutate(
    ci = paste0(apc_lower, "; ", apc_upper, " ", apc_sig),
    .after = apc
  ) |>

  # Unir AAPC y significancia
  unite(col = "aapc", c(aapc, aapc_sig), sep = " ") |>

  # Ordenar filas
  arrange(nivel, region) |>

  # Seleccionar columnas
  select(-contains("_"))


# Tabla 1 ----------------------------------------------------------------
tab1 <- tab_mod |>
  # --- Filtrar GC1-GC2 ---
  filter(nivel %in% c("GC1", "GC2")) |>

  # --- Generar tabla ---
  flextable() |>

  # --- Encabezados ---
  set_header_labels(
    nivel = "Grupo",
    region = "Región",
    jurisdiccion = "Jurisdicción",
    jp = "JP",
    period = "Período",
    apc = "APC",
    ci = "95% IC",
    aapc = "AAPC"
  ) |>

  # --- Formato tabla ---
  tab_fmt() |>
  merge_v(j = 2:4, combine = TRUE) |>
  merge_v(j = "region") |>
  merge_v(j = "aapc") |>
  autofit() |>

  # --- Caption ---
  set_caption(
    autonum = FALSE,
    fp_p = fp_par(line_spacing = 1.5),
    caption = as_paragraph(
      as_chunk(
        "Tabla 1. Coeficientes de la regresión joinpoint de las tasas estandarizadas de mortalidad por códigos garbage nivel 1 y 2 (GC1-GC2) por región y jurisdicción, Argentina (2010-2023).",
        props = fp_text(
          font.size = 12,
          font.family = "Times New Roman",
          bold = TRUE
        )
      )
    )
  )


## Guardar como DOCX ----
# save_as_docx(tab1, path = "figs_tablas/Tabla1.docx")

# # Tabla 2 ----------------------------------------------------------------
tab2 <- tab_mod |>
  # --- Filtrar GC3-GC4 ---
  filter(nivel %in% c("GC3", "GC4")) |>

  # --- Generar tabla ---
  flextable() |>

  # --- Encabezados ---
  set_header_labels(
    nivel = "Grupo",
    region = "Región",
    jurisdiccion = "Jurisdicción",
    jp = "JP",
    period = "Período",
    apc = "APC",
    ci = "95% IC",
    aapc = "AAPC"
  ) |>

  # --- Formato tabla ---
  tab_fmt() |>
  merge_v(j = 2:4, combine = TRUE) |>

  merge_v(j = "region") |>
  merge_v(j = "aapc") |>
  autofit() |>

  # --- Caption ---
  set_caption(
    autonum = FALSE,
    fp_p = fp_par(line_spacing = 1.5),
    caption = as_paragraph(
      as_chunk(
        "Tabla 2. Coeficientes de la regresión joinpoint de las tasas estandarizadas de mortalidad por códigos garbage nivel 3 y 4 (GC3-GC4) por región y jurisdicción, Argentina (2010-2023).",
        props = fp_text(
          font.size = 12,
          font.family = "Times New Roman",
          bold = TRUE
        )
      )
    )
  )

## Guardar como DOCX ----
# save_as_docx(tab2, path = "figs_tablas/Tabla2.docx")

# Limpiar environment ----------------------------------------------------
rm(pob_est_2022, proy_2010_2023, tab1, tab2, tab_mod)
