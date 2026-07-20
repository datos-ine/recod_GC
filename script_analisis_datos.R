### Mortalidad por códigos basura en Argentina (2010–2023):
### redistribución hacia causas específicas
### Análisis de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar

# Cargar paquetes --------------------------------------------------------
# remotes::install_github("https://github.com/datos-ine/joinpointR")

pacman::p_load(
  # Gráficos
  cols4all,
  patchwork,
  ggridges,
  treemapify,
  # Tablas
  officer,
  flextable,
  # Tasas estandarizadas
  PHEindicatormethods,
  # Regresión joinpoint
  joinpointR,
  # Manejo de datos
  scales,
  here,
  rio,
  janitor,
  tidyverse
  # ,
  # update = TRUE
)


# Tema flextable -----------------------------------------------------------
tab_fmt <- function(x) {
  x |>
    # flextable() |>
    bold(part = "header") |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 9, part = "all") |>
    line_spacing(space = 1.5, part = "all") |>
    valign(valign = "top") |>
    align(align = "left", part = "all") |>
    merge_v(j = 1) |>
    merge_v(j = 2:3, combine = TRUE)
}


# Paletas colorblind-friendly ----------------------------------------------
pal <- c4a(palette = "managua", n = 10, reverse = TRUE) |>
  set_names(c(
    "DM",
    "ECV",
    "ERC",
    "NPL",
    "OENT",
    "TRA",
    "SU",
    "HO",
    "OCE",
    "CMNN"
  ))


# Cargar/preparar datos --------------------------------------------------
## Población estándar Argentina (2022) -----
pob_est_2022 <- import(here("clean", "arg_pob_est_2022.rds"))

## Proyecciones poblacionales Argentina (2010-2023) -----
proy_2010_2023 <- import(here("clean", "arg_proy_2010_2023.rds"))


## Defunciones por grupo de causas -----
datos_gc <- import(here("clean", "arg_defun_recod_2010_2023.rds")) |>

  # Ordenar datos
  mutate(
    jurisdiccion = fct_relevel(
      jurisdiccion,
      "Buenos Aires",
      "CABA",
      "Córdoba",
      "Entre Ríos",
      "Santa Fe",
      "Mendoza",
      "Cuyo2 (La Rioja, San Juan, San Luis)",
      "Chaco",
      "Corrientes",
      "Formosa",
      "Misiones",
      "Tucumán"
    )
  ) |>

  # Modificar niveles paso1
  mutate(
    paso1 = if_else(
      str_detect(paso1, "GC3|GC4") & paso1 != "GC4-NNE",
      str_remove(paso1, "-.*"),
      paso1
    )
  )


# Tabla 1 ----------------------------------------------------------------
tab1 <- tibble(
  Variable = c(
    "Año",
    "Región",
    "Jurisdicción",
    "Sexo",
    "Grupo etario",
    "Causa básica de muerte",
    "Grupo de causas",
    "Subgrupo de causas",
    "Número de muertes"
  ),
  Descripción = c(
    "Año de ocurrencia de la defunción",
    "Agrupación territorial definida por la DEIS para resguardar la confidencialidad de los datos",
    "Nivel de agregación jurisdiccional definido por la DEIS para resguardar la confidencialidad de los datos",
    "Sexo consignado en el acta de defunción",
    "Categorías de edad agrupadas según criterios de la DEIS",
    "Causa básica de muerte codificada según la CIE-10",
    "Clasificación de causas según grandes grupos del GBD-2019",
    "Clasificación de causas según grupos nivel 2 del GBD-2019",
    "Cantidad anual de defunciones según región, jurisdicción, sexo, grupo etario y causa básica de muerte"
  ),
  Valores = c(
    "2010-2023",
    paste(levels(datos_gc$region_deis), collapse = "; "),
    paste(levels(datos_gc$jurisdiccion), collapse = "; "),
    "Masculino; Femenino",
    paste(levels(datos_gc$grupo_edad), collapse = "; "),
    "A00.0-Z99.9",
    "CMNN; ENT; CE; GC",
    "CMNN; DM; ECV; ERC; NPL; OENT; TRA; SU; HO; OCE; GC1; GC2; GC3; GC4; NNE",
    "Conteo de defunciones"
  )
) |>

  # Formato tabla
  flextable() |>
  tab_fmt() |>
  width(width = c(3, 7, 7), unit = "cm") |>
  set_caption(
    autonum = FALSE,
    fp_p = fp_par(line_spacing = 1.5),
    caption = as_paragraph(
      as_chunk(
        "Tabla 1. Variables incluidas en el estudio y categorías de análisis.",
        props = fp_text(
          font.size = 12,
          font.family = "Times New Roman",
          bold = TRUE
        )
      )
    )
  )


# ## Guardar tabla -----
# save_as_docx(
#   tab1,
#   path = "tablas/Tabla1.docx",
#   pr_section = prop_section(
#     page_margins = page_mar(
#       bottom = 0.7874,
#       top = 0.7874,
#       left = 0.7874,
#       right = 0.7874
#     )
#   )
# )

# Tabla 2 ----------------------------------------------------------------
tab2 <- datos_gc |>
  # Filtrar GC1-GC2
  filter(paso1 %in% c("GC1", "GC2")) |>

  # Reagrupar niveles
  mutate(
    cie10_cod = if_else(
      cie10_cod == "A41.9",
      cie10_cod,
      str_sub(cie10_cod, 1, 3)
    )
  ) |>

  # Añadir descripción códigos DEIS
  left_join(
    import(here("raw", "descdef1.xlsx"), sheet = 4),
    by = join_by(cie10_cod == CODIGO)
  ) |>

  # Completar NAs
  mutate(
    VALOR = if_else(
      cie10_cod == "A41.9",
      "Septicemia no especificada",
      VALOR
    )
  ) |>

  # Frecuencia muertes
  count(cie10_cod, causa = VALOR, wt = n) |>
  mutate(pct = percent(n / sum(n), accuracy = .1, decimal.mark = ",")) |>

  # Filtrar por frecuencia
  arrange(-n) |>
  filter_out(n < 10000) |>

  ## Formato tabla
  flextable() |>
  tab_fmt() |>
  set_header_labels(
    cie10_cod = "Código",
    causa = "Causa",
    n = "Frecuencia",
    pct = "%"
  ) |>
  autofit() |>
  set_caption(
    autonum = FALSE,
    fp_p = fp_par(line_spacing = 1.5),
    caption = as_paragraph(
      as_chunk(
        "Tabla 2. Principales códigos garbage de nivel 1 y 2 (GC1-GC2) registrados como causa básica de defunción en Argentina (2010-2023).",
        props = fp_text(
          font.size = 12,
          font.family = "Times New Roman",
          bold = TRUE
        )
      )
    )
  )


# ## Guardar tabla -----
# save_as_docx(
#   tab2,
#   path = "tablas/Tabla2.docx",
#   pr_section = prop_section(
#     page_margins = page_mar(
#       bottom = 0.7874,
#       top = 0.7874,
#       left = 0.7874,
#       right = 0.7874
#     )
#   )
# )

# Figura 2 ---------------------------------------------------------------
## Función auxiliar -----
treeplot_data <- function(data, var) {
  data |>
    mutate(
      grupo_causa = case_when(
        {{ var }} %in% c("DM", "ECV", "ERC", "NPL", "OENT") ~ "ENT",
        {{ var }} %in% c("TRA", "SU", "HO", "OCE") ~ "CE",
        str_detect({{ var }}, "GC") ~ "GC",
        .default = {{ var }}
      ),
      fill_id = {{ var }}
    ) |>
    count(grupo_causa, fill_id, wt = n) |>
    mutate(pct = n / sum(n)) |>
    mutate(
      grupo_causa = if_else(
        grupo_causa == "CE",
        grupo_causa,
        paste0(
          grupo_causa,
          " (",
          percent(sum(pct), accuracy = .1, decimal.mark = ","),
          ")"
        )
      ),
      .by = grupo_causa
    )
}

## Panel 1 -----
g1 <- datos_gc |>
  treeplot_data(var = paso1) |>

  # Generar plot
  ggplot(aes(
    area = pct,
    subgroup = grupo_causa,
    fill = fill_id
  )) +
  labs(subtitle = "Paso 1") +
  theme(
    legend.position = "none",
    text = element_text(family = "Times New Roman")
  )


## Panel 2 -----
g2 <- datos_gc |>
  treeplot_data(var = paso2a) |>

  # Generar plot
  ggplot(aes(
    area = pct,
    subgroup = grupo_causa,
    fill = fill_id
  )) +
  labs(subtitle = "Paso 2A") +
  theme(
    legend.position = "none",
    text = element_text(family = "Times New Roman")
  )


## Panel 3 -----
g3 <- datos_gc |>
  treeplot_data(var = paso2b) |>

  # Generar plot
  ggplot(aes(
    area = pct,
    subgroup = grupo_causa,
    fill = fill_id
  )) +
  labs(subtitle = "Paso 2B") +
  theme(
    legend.position = "none",
    text = element_text(family = "Times New Roman")
  )


## Panel 4 -----
g4 <- datos_gc |>
  treeplot_data(var = paso4) |>

  # Generar plot
  ggplot(aes(
    area = pct,
    subgroup = grupo_causa,
    fill = fill_id
  )) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(subtitle = "Pasos 3-4") +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom",
    legend.key.spacing.x = unit(5, "points"),
    legend.key.size = unit(5, "points")
  )


## Treemap -----
fig2 <- g1 /
  (g2 + g3) /
  g4 &

  # Treemap
  geom_treemap(alpha = .9) &
  geom_treemap_text(
    aes(
      label = case_when(
        pct < .008 | fill_id == "CMNN" ~ "",
        pct >= .009 ~
          paste0(
            fill_id,
            " (",
            percent(pct, accuracy = .1, decimal.mark = ","),
            ")"
          ),

        .default = percent(pct, accuracy = .1, decimal.mark = ",")
      )
    ),
    place = "topright",
    reflow = TRUE,
    color = "white",
    family = "Times New Roman",
    fontface = "bold",
    min.size = 6,
    size = 11
  ) &

  # Subgrupo
  geom_treemap_subgroup_border() &
  geom_treemap_subgroup_text(
    place = "bottomleft",
    size = 9,
    family = "Times New Roman",
    fontface = "bold",
    color = "grey20"
  ) &

  # Layout
  scale_fill_manual(name = NULL, values = pal, na.value = "grey65")


# ## Guardar figura -----
# ggsave(
#   fig2,
#   filename = "figuras/Figura2.png",
#   width = 17,
#   units = "cm",
#   dpi = 300
# )

# Evolución tasas GC por región ------------------------------------------
## Tasas estandarizadas -----
datos_jp_reg <- datos_gc |>
  # Seleccionar muertes por GC
  filter(grupo_causa == "GC") |>
  droplevels() |>

  # Modificar niveles paso 1
  mutate(paso1 = str_remove(paso1, "-.*")) |>

  # Agrupar datos por año
  count(
    anio,
    grupo_edad,
    region_deis,
    nivel = paso1,
    wt = n
  ) |>

  # Unir con proyecciones poblacionales
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

  # Añadir totales Argentina
  (\(x) {
    bind_rows(
      x,
      x |>
        group_by(anio, grupo_edad, nivel) |>
        summarise(
          pob = sum(pob, na.rm = TRUE),
          n = sum(n, na.rm = TRUE),
          region_deis = "Argentina",
          .groups = "drop"
        )
    )
  })() |>

  # Añadir población estándar 2022
  left_join(pob_est_2022) |>

  # Calcular tasa estandarizada
  group_by(anio, region_deis, nivel) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  )


## Regresión joinpoint -----
mod_jp_reg <- model_jp(
  datos_jp_reg,
  value = value,
  time = anio,
  group = c("nivel", "region_deis"),
  step = TRUE,
  k = 3,
  min_dist = 2,
  test = TRUE
)


## Figura 3 --------------------------------------------------------------
fig3 <- mod_jp_reg |>
  gg_jpoint(
    facets = "grid",
    psize = 1.5,
    cbpal = "managua"
  ) +
  labs(y = "log-tasa") +
  theme(
    legend.position = "none",
    text = element_text(family = "Times New Roman", size = 12)
  )

# ### Guardar figura ----
# ggsave(
#   fig3,
#   filename = "figuras/Figura3.png",
#   width = 17,
#   units = "cm",
#   dpi = 300
# )

# Evolución tasas GC por jurisdicción ------------------------------------
## Tasas estandarizadas -----
datos_jp <- datos_gc |>
  # Seleccionar muertes por GC
  filter(grupo_causa == "GC") |>
  droplevels() |>

  # Modificar niveles paso 1
  mutate(paso1 = str_remove(paso1, "-.*")) |>

  # Agrupar datos por año
  count(
    anio,
    grupo_edad,
    region_deis,
    jurisdiccion,
    nivel = paso1,
    wt = n
  ) |>

  # Unir con proyecciones poblacionales
  left_join(
    proy_2010_2023 |>
      # Agrupar por año
      count(
        anio,
        region_deis,
        jurisdiccion,
        grupo_edad,
        wt = proy,
        name = "pob"
      )
  ) |>

  # Añadir totales Argentina
  (\(x) {
    bind_rows(
      x,
      x |>
        group_by(anio, grupo_edad, nivel) |>
        summarise(
          pob = sum(pob, na.rm = TRUE),
          n = sum(n, na.rm = TRUE),
          region_deis = "Argentina",
          jurisdiccion = "Argentina",
          .groups = "drop"
        )
    )
  })() |>

  # Añadir población estándar 2022
  left_join(pob_est_2022) |>

  # Calcular tasa estandarizada
  group_by(anio, region_deis, jurisdiccion, nivel) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  ) |>

  # Crear etiquetas para el gráfico
  mutate(
    reg_jur = if_else(
      jurisdiccion == "Argentina",
      jurisdiccion,
      paste0(
        region_deis,
        ": ",
        str_remove(jurisdiccion, " \\(.*")
      )
    )
  )

## Regresión joinpoint -----
### GC1 -----
mod_jp1 <- model_jp(
  datos_jp |> filter(nivel == "GC1"),
  value = value,
  time = anio,
  group = c("region_deis", "jurisdiccion"),
  step = TRUE,
  k = 3,
  min_dist = 2,
  test = TRUE
)

## AAPC
get_aapc(mod_jp1)


### GC2 -----
mod_jp2 <- model_jp(
  datos_jp |> filter(nivel == "GC2"),
  value = value,
  time = anio,
  group = c("region_deis", "jurisdiccion"),
  step = TRUE,
  k = 3,
  min_dist = 2,
  test = TRUE
)

## AAPC
get_aapc(mod_jp2)


### GC3 -----
mod_jp3 <- model_jp(
  datos_jp |> filter(nivel == "GC3"),
  value = value,
  time = anio,
  group = c("region_deis", "jurisdiccion"),
  step = TRUE,
  k = 3,
  min_dist = 2,
  test = TRUE
)

## AAPC
get_aapc(mod_jp3)


### GC4 -----
mod_jp4 <- model_jp(
  datos_jp |> filter(nivel == "GC4"),
  value = value,
  time = anio,
  group = c("region_deis", "jurisdiccion"),
  step = TRUE,
  k = 3,
  min_dist = 2,
  test = TRUE
)

## AAPC
get_aapc(mod_jp4)
