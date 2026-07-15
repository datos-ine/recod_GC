### Mortalidad por códigos basura en Argentina (2010–2023):
### redistribución hacia causas específicas
### Análisis de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 15-07-2026 13:10

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Gráficos
  cols4all,
  patchwork,
  ggridges,
  treemapify,
  # Tablas
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
    bold(part = "header") |>
    font(fontname = "Times New Roman", part = "all") |>
    fontsize(size = 12, part = "all") |>
    valign(valign = "top") |>
    align(align = "left", part = "all") |>
    merge_v(j = 1) |>
    merge_v(j = 2:3, combine = TRUE)
}


# Paleta colorblind-friendly ---------------------------------------------
pal <-
  # c4a(palette = "cassatt1", n = 10, reverse = TRUE) |>
  c4a(palette = "managua", n = 10, reverse = TRUE) |>
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

  # Modificar niveles paso1
  mutate(
    paso1 = if_else(
      str_detect(paso1, "GC3|GC4") & paso1 != "GC4-NNE",
      str_remove(paso1, "-.*"),
      paso1
    )
  )


## Tabla 2 ---------------------------------------------------------------
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

  # Frecuencia muertes
  count(Código = cie10_cod, wt = n) |>
  mutate(pct = percent(n / sum(n), accuracy = .1, decimal.mark = ",")) |>

  # Filtrar por frecuencia
  arrange(-n) |>
  filter_out(n < 20000) |>

  # Añadir descripción
  mutate(
    Causa = c(
      "Insuficiencia cardíaca",
      "Otras causas mal definidas y no especificadas de mortalidad",
      "Insuficiencia respiratoria",
      "Septicemia no especificada",
      "Neumonitis por alimentos o vómito",
      "Insuficiencia renal no especificada",
      "Otras enfermedades del sistema digestivo",
      "Hipertensión esencial (primaria)",
      "Exposición a factor no especificado",
      "Shock, no clasificado en otra parte",
      "Edema pulmonar",
      "Insuficiencia renal aguda",
      "Otras muertes súbitas de causa desconocida",
      "Embolia pulmonar",
      "Peritonitis"
    ),
    .after = "Código"
  ) |>

  ## Mostrar tabla
  flextable() |>
  tab_fmt()


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
#   width = 15,
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
      as.character(jurisdiccion) == as.character(region_deis),
      jurisdiccion,
      paste0(region_deis, ": ", jurisdiccion)
    )
  )


## Figura 3 --------------------------------------------------------------
fig3 <- datos_jp |>
  # Gráfico
  ggplot(aes(x = value, y = nivel, fill = nivel)) +
  facet_wrap(~reg_jur, ncol = 3) +

  # Geometrías
  geom_density_ridges(
    jittered_points = TRUE,
    position = "raincloud",
    color = NA,
    alpha = .75,
    point_color = "grey40",
    point_alpha = .35,
    scale = 3
  ) +

  # Escalas
  scale_fill_manual(
    values = c4a(palette = "managua", n = 4),
    name = NULL
  ) +

  # Layout
  labs(
    x = "Tasa est. (100.000 hab.)",
    y = NULL
  ) +

  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "Times New Roman", size = 12),
    strip.text = element_text(face = "bold")
  )


# ### Guardar figura ----
# ggsave(
#   fig3,
#   filename = "figuras/Figura3.png",
#   width = 15,
#   units = "cm",
#   dpi = 300
# )

## Regresión joinpoint ---------------------------------------------------
## GC1 -----
mod_jp1 <- datos_jp |>
  filter(nivel == "GC1") |>
  model_jp(
    value = value,
    time = anio,
    group = c("region_deis", "jurisdiccion"),
    step = TRUE,
    k = 3,
    test = TRUE
  )

## AAPC
get_aapc(mods = mod_jp1, digits = 2)


## GC2 -----
mod_jp2 <- datos_jp |>
  filter(nivel == "GC2") |>
  model_jp(
    value = value,
    time = anio,
    group = c("region_deis", "jurisdiccion"),
    step = TRUE,
    k = 3,
    test = TRUE
  )


## AAPC
get_aapc(mods = mod_jp2)


## GC3 -----
mod_jp3 <- datos_jp |>
  filter(nivel == "GC3") |>
  model_jp(
    value = value,
    time = anio,
    group = c("region_deis", "jurisdiccion"),
    step = TRUE,
    k = 3,
    test = TRUE
  )


## AAPC
get_aapc(mods = mod_jp3)


## GC4 -----
mod_jp4 <- datos_jp |>
  filter(nivel == "GC4") |>
  model_jp(
    value = value,
    time = anio,
    group = c("region_deis", "jurisdiccion"),
    step = TRUE,
    k = 3,
    test = TRUE
  )


## AAPC
get_aapc(mods = mod_jp4)


# Tablas suplementarias --------------------------------------------------
## Tabla S3 -----
tabs3 <- mod_jp1 |>
  summary_jp(dec = ",") |>
  flextable() |>
  tab_fmt()


## Tabla S4 -----
tabs4 <- mod_jp2 |>
  summary_jp(dec = ",") |>
  flextable() |>
  tab_fmt()


## Tabla S5 -----
tabs5 <- mod_jp3 |>
  summary_jp(dec = ",") |>
  flextable() |>
  tab_fmt()


## Tabla S6 -----
tabs6 <- mod_jp4 |>
  summary_jp(dec = ",") |>
  flextable() |>
  tab_fmt()


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
          jurisdiccion = "Argentina",
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
  test = TRUE
)

names(mod_jp_reg) <- str_replace(names(mod_jp_reg), "Patagonia", "Pat.")


## Figura 4 --------------------------------------------------------------
fig4 <- mod_jp_reg |>
  gg_jpoint(
    facets = "grid",
    cbpal = "managua",
    psize = 1.5
  ) +

  labs(y = "log-tasa") +
  theme(
    legend.position = "none",
    text = element_text(family = "Times New Roman", size = 12)
  )

# ### Guardar figura ----
# ggsave(
#   fig4,
#   filename = "figuras/Figura4.png",
#   width = 15,
#   units = "cm",
#   dpi = 300
# )
