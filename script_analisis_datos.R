### Mortalidad por códigos garbage en Argentina (2010–2023):
### redistribución hacia causas específicas
### Análisis de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 30-09-2026 11:44

# Cargar paquetes --------------------------------------------------------
# remotes::install_github("https://github.com/datos-ine/joinpointR")
pacman::p_load(
  PHEindicatormethods,
  joinpointR,
  officer,
  flextable,
  gtsummary,
  scales,
  here,
  rio,
  janitor,
  tidyverse
)


# Formato tablas ---------------------------------------------------------
source("tab_fmt.R")

# Cargar/preparar datos --------------------------------------------------
## Población estándar Argentina (2022) -----
pob_est_2022 <- import(here("clean", "arg_pob_est_2022.rds"))

## Proyecciones poblacionales Argentina (2010-2023) -----
proy_2010_2023 <- import(here("clean", "arg_proy_2010_2023.rds"))


## Defunciones por grupo de causas -----
datos_gc_raw <- import(here("clean", "arg_recod_defun_gbd23.rds"))


# Procesamiento de datos -------------------------------------------------
datos_gc <- datos_gc_raw |>
  # --- Modificar grupo nivel 2 ---
  mutate(across(
    .cols = starts_with("gbd"),
    .fns = ~ fct_collapse(
      .x,
      "ENT:OTR-ENT" = c(
        "ENT:DIG",
        "ENT:MUSC",
        "ENT:NEU-MENT",
        "ENT:PIEL",
        "ENT:SUST"
      ),
      "CE:OTR-CE" = "CE:OTR-TRA"
    )
  )) |>

  # --- Grupos nivel 1 y 2 ---
  mutate(
    across(
      contains("gbd"),
      list(
        # --- Grupo nivel 1 ---
        g1 = ~ str_remove(.x, ":.*"),

        # --- Grupo nivel 2 ---
        g2 = ~ str_remove(.x, ".*:"),

        # --- Grupo de causas ---
        grupo = ~ case_when(
          str_detect(.x, "CRD|DM|ECV|NPL") ~ "ENT-OBJ",
          str_detect(.x, "CA|SH|TRA$|VI") ~ "CE-OBJ",
          .default = str_remove(.x, ".*:")
        ) |>
          fct_relevel(
            "ENT-OBJ",
            "OTR-ENT",
            "CE-OBJ",
            "OTR-CE",
            "INF",
            "MAT-NEO",
            "NUTR",
          )
      ),
      .names = "{.col}_{.fn}"
    )
  ) |>

  # --- Ordenar datos ---
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

# ========================================================================
# ---- Análisis exploratorio ----
# ========================================================================
# ---- Muertes por sexo y edad ----
datos_gc |>
  uncount(weights = n) |>
  tbl_cross(row = grupo_edad, col = sexo) |>
  add_p()


# Tabla 1 ----------------------------------------------------------------
## ---- Muertes por GC1 y GC2 ----
datos_tab1 <- datos_gc |>
  # --- Filtrar GC1-GC2 ---
  filter(gbd_paso1 %in% c("GC:GC1", "GC:GC2")) |>

  # --- Reagrupar niveles ---
  mutate(cie10_cod = str_sub(cie10_cod, 1, 3)) |>

  # --- Calcular frecuencias ---
  count(cie10_cod, wt = n) |>
  mutate(
    pct = percent(
      n / sum(n),
      accuracy = .1,
      decimal.mark = ","
    )
  ) |>

  # --- Añadir descripción códigos DEIS ---
  left_join(
    import(here("raw", "descdef1.xlsx"), sheet = 4),
    by = join_by(cie10_cod == CODIGO)
  ) |>

  # --- Ordenar por frecuencia ---
  arrange(-n) |>

  # --- Ordenar columnas ---
  select(cie10_cod, causa = VALOR, everything()) |>

  # --- Seleccionar 20 primeros ---
  head(n = 20)


## ---- Tabla ----
tab1 <- datos_tab1 |>
  # --- Convertir a tabla ---
  flextable() |>

  # --- Encabezados ---
  set_header_labels(
    cie10_cod = "Código",
    causa = "Causa",
    n = "Frecuencia",
    pct = "%"
  ) |>

  # --- Layout ---
  tab_fmt() |>
  width(width = c(2, 10, 2, 2), unit = "cm") |>
  theme_booktabs(bold_header = TRUE) |>

  # --- Caption ---
  set_caption(
    caption = "Tabla 1. Principales códigos garbage de nivel 1 y 2 (GC1-GC2) registrados como causa básica de defunción en Argentina (2010-2023)."
  )

## Guardar como DOCX ----
# save_as_docx(
#   tab1,
#   path = "figs_tablas/Tabla1.docx",
#   align = "left"
# )

# Figura 2 ---------------------------------------------------------------
## ---- Muertes por sexo, edad y grupo causa ----
datos_fig2 <- datos_gc |>
  # --- Calcular frecuencias x edad y sexo ---
  count(grupo_edad, sexo, gbd_paso1_g1, gbd_paso1_grupo, wt = n) |>
  mutate(
    pct = n / sum(n),
    .by = c(sexo, gbd_paso1_g1, gbd_paso1_grupo)
  )


## ---- Gráfico ----
fig2 <- datos_fig2 |>
  ggplot(aes(x = grupo_edad, y = pct, fill = gbd_paso1_grupo)) +
  facet_grid(sexo ~ gbd_paso1_g1) +
  geom_col(position = "dodge", alpha = .9) +

  # --- Escalas ---
  scale_y_continuous(name = NULL, labels = percent) +
  scale_x_discrete(name = NULL) +
  scale_cbpal_fill(palette = "managua", name = NULL) +

  # --- Layout ---
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90),
    text = element_text(family = "Times New Roman", size = 12)
  )

## Guardar como PNG ----
# ggsave(
#   fig2,
#   filename = "figs_tablas/Figura2.png",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

## Guardar como SVG ----
# ggsave(
#   fig2,
#   filename = "figs_tablas/Figura2.svg",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

# Figura 3 ---------------------------------------------------------------
## ---- Muertes por grupo de causa pasos 1-4----
datos_fig3 <- datos_gc |>
  # --- Seleccionar columnas ---
  select(contains(c("g1", "g2")), n) |>

  # --- Modificar niveles ---
  mutate(
    gbd_paso2a_g2 = if_else(gbd_paso2a_g2 == "NNE", "GC4", gbd_paso2a_g2)
  ) |>

  # --- Variables caracter a factor ---
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x))) |>

  # --- Base long ---
  pivot_longer(
    cols = contains("paso"),
    names_to = c("paso", ".value"),
    names_pattern = "^gbd_(paso[0-9]+[ab]?)_(g1|g2)$"
  ) |>

  # --- Frecuencias ---
  count(
    paso,
    grupo_causa = g1,
    causa = g2,
    wt = n
  ) |>

  # --- Completar frecuencias ---
  complete(paso, nesting(grupo_causa, causa), fill = list(n = 0)) |>
  # --- Frecuencia anterior de cada causa ---
  mutate(
    n_anterior = lag(n),
    .by = c(grupo_causa, causa)
  ) |>

  # --- Totales por paso ---
  group_by(paso) |>
  mutate(
    n_total = sum(n)
  ) |>
  ungroup() |>

  # --- Total del paso anterior ---
  mutate(
    n_total_anterior = lag(n_total)
  ) |>

  # --- Cambio porcentual ---
  mutate(
    pct = case_when(
      paso == "paso1" ~ n / n_total,
      n_total_anterior > 0 ~
        (n - n_anterior) / n_total_anterior,
      TRUE ~ NA_real_
    )
  )


## ---- Gráfico ----
fig3 <- datos_fig3 |>
  mutate(
    causa = factor(
      causa,
      levels = unique(causa[order(grupo_causa)])
    ) |>
      fct_relevel("OTR-CE", after = 4)
  ) |>
  ggplot(aes(
    x = causa,
    y = pct,
    fill = paso,
    group = grupo_causa
  )) +
  geom_col() +

  # --- Escalas ---
  scale_y_continuous(
    name = "Frecuencia (%)",
    labels = label_percent()
  ) +
  scale_x_discrete(name = "Causa") +
  scale_cbpal_fill(palette = "managua", name = NULL) +

  # --- Layout ---
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(angle = 90)
  )

## Guardar como PNG ----
# ggsave(
#   fig3,
#   filename = "figs_tablas/Figura3.png",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

## Guardar como SVG ----
# ggsave(
#   fig3,
#   filename = "figs_tablas/Figura3.svg",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

# ========================================================================
# ---- Tendencias temporales tasas GC ----
# ========================================================================
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
  group = c("nivel", "region_deis")
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


# Figura 4 ---------------------------------------------------------------
fig4 <- c(mod_ar, mod_reg) |>
  gg_jpoint(
    geom = "linepoint",
    facets = "grid",
    aapc = TRUE,
    psize = 1.5,
    text.size = 7,
    date.breaks = "3 years",
    cbpal = "managua"
  ) +
  scale_y_log10()

## Save as PNG ----
# ggsave(
#   fig4,
#   filename = "figs_tablas/Figura4.png",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

# Limpiar environment ----------------------------------------------------
rm(datos_gc_raw, pob_est_2022, proy_2010_2023, datos_tab1, fig2, fig3, fig4)
