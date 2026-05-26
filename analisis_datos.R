### Mortalidad por códigos basura en Argentina (2010–2023):
### redistribución hacia causas específicas
### Análisis de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 26-05-2026 15:31

# Cargar paquetes --------------------------------------------------------
# remotes::install_github("datos-ine/joinpointR") # Instala versión desarrollo

pacman::p_load(
  # Gráficos
  scico,
  patchwork,
  ggridges,
  treemapify,
  # Tablas
  flextable,
  gtsummary,
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


# Cargar/preparar datos --------------------------------------------------
## Población estándar Argentina (2022) -----
pob_est_2022 <- import(here("clean", "arg_pob_est_2022.rds"))

## Proyecciones poblacionales Argentina (2010-2023) -----
proy_2010_2023 <- import(here("clean", "arg_proy_mensual_2010_2023.rds")) |>
  # Seleccionar primer mes del año
  filter(mes == 1)


## Defunciones por grupo de causas -----
recod_defun <- import(here("clean", "arg_defun_mes_2010-2023_recod.rds")) |>
  # Ordenar grupo nivel 1
  mutate(
    grupo_n1 = fct_relevel(
      grupo_n1,
      "ENT",
      after = 0
    )
  ) |>
  # Crear variable de agrupamiento nivel 2
  mutate(
    grupo_causa = fct_collapse(
      grupo_n2i,
      "GC1-GC2" = c("GC1", "GC2"),
      "GC3-GC4" = c("GC3", "GC4")
    )
  )


# Paletas colorblind-friendly --------------------------------------------
pal <- scico(n = 4, palette = "managua")

pal2 <- c(scico(n = 10, palette = "managua", direction = -1), "grey60")


# Figura 1 ---------------------------------------------------------------
# ## Cargar datos -----
# source("script_figura_1.R")
#
# ## Guardar figura -----
# export_svg(fig1) |>
#   charToRaw() |>
#   rsvg_png(
#     file = "figuras/Figura1.png",
#     width = 1772,
#     height = 1500
#   )

# Análisis exploratorio --------------------------------------------------
## Tema para las tablas
theme_gtsummary_language(
  language = "es",
  decimal.mark = ",",
  big.mark = "."
)

## Tabla S2: Defunciones por sexo y grupo etario -----
tabs2 <- recod_defun |>
  uncount(weights = n) |>
  tbl_cross(
    col = sexo,
    row = grupo_edad,
    digits = c(0, 1),
    percent = "column",
    label = list(sexo = "Sexo", grupo_edad = "Grupo etario")
  ) |>
  add_p()


## Defunciones por sexo y causa nivel 1 -----
recod_defun |>
  uncount(weights = n) |>
  mutate(
    grupo_n1 = case_when(
      grupo_causa == "GC1-GC2" ~ "GC1-GC2",
      grupo_causa == "GC3-GC4" ~ "GC3-GC4",
      .default = grupo_n1
    )
  ) |>
  tbl_cross(
    col = sexo,
    row = grupo_n1,
    percent = "column",
    digits = c(0, 1)
  ) |>
  add_p()


## Tabla S3: Defunciones por sexo, grupo etario y causa -----
tabs3 <- recod_defun |>
  uncount(weights = n) |>
  mutate(
    grupo_n1 = case_when(
      grupo_causa == "GC1-GC2" ~ "GC1-GC2",
      grupo_causa == "GC3-GC4" ~ "GC3-GC4",
      .default = grupo_n1
    )
  ) |>
  tbl_strata2(
    strata = sexo,
    .combine_with = "tbl_merge",
    .tbl_fun = ~ .x |>
      tbl_cross(
        row = grupo_edad,
        col = grupo_n1,
        percent = "row",
        margin = "row",
        digits = c(0, 1)
      ) |>
      add_p()
  )


# tabs2 <- recod_defun |>
#   # Individualizar filas para calcular frecuencias
#   uncount(weights = n) |>

#   # Tabla 2x2
#   tbl_summary(
#     by = grupo_edad,
#     include = c(sexo, grupo_n1, grupo_causa),
#     digits = list(all_categorical() ~ c(0, 1)),
#     label = list(
#       sexo = "Sexo",
#       grupo_n1 = "Grupo nivel 1",
#       grupo_causa = "Grupo nivel 2"
#     )
#   ) |>
#   add_p() |>

#   # Opciones tabla
#   bold_labels() |>
#   modify_header(
#     label = "**Variable**",
#     p.value = "**p**",
#     all_stat_cols() ~ "{level} ({style_percent(p)}%)"
#   ) |>
#   modify_spanning_header(all_stat_cols() ~ "**Grupo etario**") |>
#   remove_footnote_header() |>
#   modify_indent(columns = label, indent = 0L)

## Tabla S3 --------------------------------------------------------------
tabs3 <- recod_defun |>
  # Filtrar GC1-GC2
  filter(grupo_causa %in% c("GC1-GC2")) |>

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
  autofit()


# Evolución temporal tasas GC --------------------------------------------
## Tasas estandarizadas por año -----
datos_jp <- recod_defun |>
  # Seleccionar muertes por GC
  filter(str_detect(grupo_n1, "GC")) |>
  droplevels() |>

  # Agrupar datos por año
  count(anio, grupo_edad, region_deis, grupo_n2i, wt = n) |>

  # Unir con proyecciones poblacionales
  left_join(
    proy_2010_2023 |>
      # Agrupar por año
      count(anio, region_deis, grupo_edad, wt = proy_pob, name = "pob")
  ) |>

  # Añadir totales Argentina
  (\(x) {
    bind_rows(
      x,
      x |>
        group_by(anio, grupo_edad, grupo_n2i) |>
        summarise(
          region_deis = "Argentina",
          pob = sum(pob, na.rm = TRUE),
          n = sum(n, na.rm = TRUE),
          .groups = "drop"
        )
    )
  })() |>

  # Añadir población estándar 2022
  left_join(pob_est_2022) |>

  # Calcular tasa estandarizada
  group_by(anio, region_deis, grupo_causa = grupo_n2i) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  )


### Regresión joinpoint -----
mod_jp <- model_jp(
  data = datos_jp,
  value = "value",
  time = "anio",
  group = c("region_deis", "grupo_causa"),
  step = TRUE,
  k = 3,
  test = TRUE
)


## AAPCs modelos lineales
get_aapc(mod_jp$NOA2_GC1)
get_aapc(mod_jp$`Patagonia Norte_GC1`)


get_aapc(mod_jp$`Patagonia Norte_GC2`)

get_aapc(mod_jp$Argentina_GC3)
get_aapc(mod_jp$Centro_GC3)
get_aapc(mod_jp$Cuyo_GC3)
get_aapc(mod_jp$NOA1_GC3)
get_aapc(mod_jp$`Patagonia Norte_GC3`)

get_aapc(mod_jp$NOA2_GC4)
get_aapc(mod_jp$`Patagonia Norte_GC4`)


## Tabla S4 --------------------------------------------------------------
tabs4 <- mod_jp |>
  # # Descartar modelos regresión lineal
  # discard(
  #   ~ inherits(.x, "lm") &&
  #     !inherits(.x, "segmented")
  # ) |>

  # Crear tabla
  summary_jp(
    var1 = "Región",
    var2 = "Nivel",
    lan = "es",
    ft = TRUE
  ) |>

  # Cambiar fuente
  merge_v(1) |>
  font(fontname = "Times New Roman", part = "all")


# Figura 2 ---------------------------------------------------------------
fig2 <- datos_jp |>

  # Gráfico
  ggplot(aes(x = value, y = grupo_causa, fill = grupo_causa)) +
  facet_wrap(~region_deis, ncol = 2) +

  # Geometrías
  geom_density_ridges(
    jittered_points = TRUE,
    position = "raincloud",
    color = NA,
    alpha = 0.75,
    point_color = "grey40",
    point_alpha = 0.5,
    scale = 5
  ) +

  # Escalas
  scale_x_continuous(limits = c(0, 150)) +
  scale_fill_manual(values = pal, name = NULL) +

  # Layout
  labs(
    x = "Tasa est. (100.000 hab.)",
    y = NULL,
    caption = c("GC1: amarillo; GC2: rojo; GC3: violeta; CG4: cian")
  ) +

  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "Times New Roman", size = 12),
    strip.text = element_text(face = "bold")
  )


### Guardar figura ----
# ggsave(
#   fig2,
#   filename = "figuras/Figura2.png",
#   width = 15,
#   height = 18,
#   units = "cm",
#   dpi = 300
# )

# Figura 3 ---------------------------------------------------------------
fig3 <- gg_jpoint(mods = mod_jp, facets = TRUE) +
  # Facets
  facet_wrap(
    ~group,
    ncol = 4,
    labeller = as_labeller(
      ~ str_remove(.x, "_.*") |> str_replace("Patagonia", "P.")
    )
  ) +

  # Escalas
  scale_x_continuous(n.breaks = 7) +
  scale_y_continuous(n.breaks = 2) +
  scale_color_manual(values = rep(pal, 10), name = NULL) +

  # Layout
  labs(
    y = "log-tasa",
    caption = c("GC1: amarillo; GC2: rojo; GC3: violeta; CG4: cian")
  ) +
  theme(
    legend.position = "none",
    text = element_text(family = "Times New Roman", size = 11),
    axis.text.x = element_text(angle = 90),
    strip.text = element_text(face = "bold")
  )

### Guardar figura -----
# ggsave(
#   fig3,
#   filename = "figuras/Figura3.png",
#   width = 15,
#   height = 19,
#   units = "cm",
#   dpi = 300
# )

# Figura 4: Cambio frecuencia por grupo causa ----------------------------
## Frecuencias paso1 -----
g1 <- recod_defun |>
  # Frecuencias por grupo
  count(grupo_n1, grupo_causa, wt = n) |>
  mutate(pct = n / sum(n)) |>

  # Crear variable para color de relleno
  mutate(
    fill = fct_collapse(
      grupo_causa,
      "GC" = c("GC1-GC2", "GC3-GC4")
    )
  ) |>

  # Crear etiquetas texto
  mutate(
    label = if_else(
      pct < 0.02,
      percent(pct, accuracy = .1, decimal.mark = ","),
      paste0(grupo_causa, "\n", percent(pct, accuracy = .1, decimal.mark = ","))
    )
  ) |>

  # Gráfico
  ggplot(aes(area = pct, subgroup = grupo_n1, fill = fill)) +
  geom_treemap()


## Frecuencias paso 2 -----
g2 <- recod_defun |>
  # Reagrupar grupo nivel 1
  mutate(
    grupo_n1 = case_when(
      grupo_n2m %in% c("DM", "ECV", "ERC", "NPL", "ONT") ~ "ENT",
      grupo_n2m %in% c("HO", "SU", "TRA", "OCE") ~ "CE",
      .default = grupo_n2m
    ) |>
      fct_relevel("ENT", after = 0)
  ) |>
  # Frecuencias por grupo
  count(grupo_n1, grupo_n2m, wt = n) |>
  mutate(pct = n / sum(n)) |>

  # Crear variable para color de relleno
  mutate(
    fill = fct_collapse(
      grupo_n2m,
      "GC" = c("GC1", "GC2")
    )
  ) |>

  # Crear etiquetas texto
  mutate(
    label = if_else(
      pct < 0.02,
      percent(pct, accuracy = .1, decimal.mark = ","),
      paste0(grupo_n2m, "\n", percent(pct, accuracy = .1, decimal.mark = ","))
    )
  ) |>

  # Gráfico
  ggplot(aes(area = pct, subgroup = grupo_n1, fill = fill)) +
  geom_treemap()


## Frecuencias paso 3-4 -----
g3 <- recod_defun |>
  # Reagrupar grupo nivel 1
  mutate(
    grupo_n1 = case_when(
      grupo_n2f %in% c("DM", "ECV", "ERC", "NPL", "ONT") ~ "ENT",
      grupo_n2f %in% c("HO", "SU", "TRA", "OCE") ~ "CE",
      .default = grupo_n2f
    ) |>
      fct_relevel("ENT", after = 0)
  ) |>
  # Frecuencias por grupo
  count(grupo_n1, grupo_n2f, wt = n) |>
  mutate(pct = n / sum(n)) |>

  # Crear etiquetas texto
  mutate(
    label = if_else(
      pct < 0.02,
      percent(pct, accuracy = .1, decimal.mark = ","),
      paste0(grupo_n2f, "\n", percent(pct, accuracy = .1, decimal.mark = ","))
    )
  ) |>

  # Gráfico
  ggplot(aes(area = pct, subgroup = grupo_n1, fill = grupo_n2f)) +
  geom_treemap()


## Unir gráficos -----
fig4 <- (g1 + theme(legend.position = "none")) /
  (g2 + theme(legend.position = "none")) /
  (g3 +
    guides(fill = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")) &

  # Añadir borde
  geom_treemap_subgroup_border() &

  # Añadir texto
  geom_treemap_text(
    aes(label = label),
    place = "center",
    reflow = TRUE,
    min.size = 3,
    color = "white",
    family = "Times New Roman"
  ) &

  # Escala de colores
  scale_fill_manual(values = pal2, name = NULL) &

  # Layout
  theme(
    text = element_text(family = "Times New Roman", size = 9),
    legend.key.size = unit(1, "points")
  ) &
  plot_annotation(
    caption = c(
      "DM: Diabetes mellitus; ECV: Enf. cardiovasculares; ERC: Enf. respiratorias crónicas; NPL: neoplasias; 
      ONT: Otras ENT; TR: Accidentes de tránsito; HO: Homicidio; SU: Suicidio; OCE: Otras CE"
    )
  ) &

  cowplot::get_legend(g1, legend = "bottom")

### Guardar figura -----
# ggsave(
#   fig4,
#   filename = "figuras/Figura4.png",
#   width = 15,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

# Tabla S5 ---------------------------------------------------------------
tabs5 <- recod_defun |>
  # Frecuencias por grupo
  count(grupo_causa, wt = n) |>

  # Añadir frecuencias finales
  left_join(
    recod_defun |>
      # Frecuencias por grupo
      count(grupo_causa = grupo_n2f, wt = n, name = "n_fin")
  ) |>

  # Cambio absoluto y relativo
  mutate(
    cambio_rel = percent(((n_fin - n) / n), accuracy = .1, decimal.mark = ","),
    razon_post_pre = number(n_fin / n, accuracy = .1, decimal.mark = ",")
  ) |>

  # Renombrar columnas
  rename(
    "Grupo causa" = 1,
    "n (inicial)" = 2,
    "n (final)" = 3,
    "Cambio relativo" = cambio_rel,
    "Razón cambio" = razon_post_pre
  ) |>
  # Formato tabla
  flextable() |>
  bold(part = "header") |>
  fontsize(size = 16, part = "all") |>
  autofit()
