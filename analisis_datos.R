### Mortalidad por códigos basura en Argentina (2010–2023):
### redistribución hacia causas específicas
### Análisis de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Gráficos
  scico,
  patchwork,
  apyramid,
  gghighlight,
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
    grupo_gbd1 = fct_relevel(
      grupo_gbd1,
      "ENT",
      after = 0
    )
  ) |>

  # Crear variable de agrupamiento
  mutate(
    grupo_causa = case_when(
      grupo_gbd2i %in% c("DM", "ECV", "ERC", "NPL") ~ "ENT objetivo",
      grupo_gbd2i %in% c("TRA", "SU", "HO") ~ "CE objetivo",
      grupo_gbd2i %in% c("CMNN", "OCE", "ONT") ~ "Otras causas",
      grupo_gbd2i %in% c("GC1", "GC2") ~ "GC1-GC2",
      grupo_gbd2i %in% c("GC3", "GC4") ~ "GC3-GC4"
    ) |>
      fct_relevel("ENT objetivo", "CE objetivo", "Otras causas")
  )


# Paletas colorblind-friendly --------------------------------------------
pal <- scico(n = 4, palette = "managua")

pal2 <- c(
  scico(n = 10, palette = "managua", direction = -1),
  rep("grey40", 2),
  "grey60"
)

# Análisis exploratorio --------------------------------------------------
## Figura 2 --------------------------------------------------------------
fig2 <- recod_defun |>
  count(grupo_edad, sexo, grupo_causa, wt = n, name = "casos") |>
  mutate(pct = casos / sum(casos), .by = sexo) |>

  # Crear gráfico
  age_pyramid(
    age_group = grupo_edad,
    split_by = sexo,
    count = pct,
    show_midpoint = FALSE,
    pal = pal[c(1, 3)]
  ) +
  gghighlight() +
  facet_wrap(~grupo_causa, ncol = 2) +
  labs(
    x = "Grupo etario",
    y = NULL,
    fill = "Sexo",
    caption = c(
      # "ENT objetivo: DM, ECV, ERC, NPL; CE objetivo: TRA, SU, HO;\n Otras causas: CMNN, OENT, OCE"
      "ENT objetivo: diabetes, enf. cardiovasculares, enf. respiratorias crónicas, neoplasias;\n CE objetivo: tránsito, homicidio, suicidio;\n Otras causas: CMNN, Otras ENT, Otras CE"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90),
    text = element_text(family = "Times New Roman", size = 12),
    plot.caption = element_text(family = "Times New Roman", size = 9)
  )


### Guardar figura -----
# ggsave(
#   fig2,
#   filename = "figuras/Figura2.png",
#   width = 15,
#   units = "cm"
# )

## Tabla 2 ---------------------------------------------------------------
tab2 <- recod_defun |>
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


# Evolución temporal tasas GC por región ---------------------------------
## Tasas estandarizadas por año ------------------------------------------
datos_jp <- recod_defun |>
  # Seleccionar muertes por GC
  filter(str_detect(grupo_gbd1, "GC")) |>
  droplevels() |>

  # Agrupar datos por año
  count(anio, grupo_edad, region_deis, grupo_causa = grupo_gbd2i, wt = n) |>

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
        group_by(anio, grupo_edad, grupo_causa) |>
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
  group_by(anio, region_deis, grupo_causa) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  )


## Regresión joinpoint ---------------------------------------------------
mod_jp <- model_jp(
  data = datos_jp,
  value = "value",
  time = "anio",
  group = c("region_deis", "grupo_causa"),
  step = TRUE,
  k = 3,
  test = TRUE
)

### AAPC modelos lineales -----
# Buscar modelos lineales
mod_jp |>
  keep(~ !inherits(.x, "segmented")) |>
  names()

# Obtener AAPCs
get_aapc(mod_jp$NOA1_GC1)
get_aapc(mod_jp$NOA2_GC1)
get_aapc(mod_jp$Patagonia_GC1)

get_aapc(mod_jp$Argentina_GC3)
get_aapc(mod_jp$Centro_GC3)
get_aapc(mod_jp$Cuyo_GC3)
get_aapc(mod_jp$`NOA (Tucumán)_GC3`)
get_aapc(mod_jp$NOA1_GC3)

get_aapc(mod_jp$NOA1_GC4)
get_aapc(mod_jp$NOA2_GC4)


## Tabla S3 --------------------------------------------------------------
tabs3 <- mod_jp |>
  # Crear tabla
  summary_jp(
    var1 = "Región",
    var2 = "Nivel",
    ft = TRUE,
    lan = "es"
  ) |>

  # Formato tabla
  font(fontname = "Times New Roman", part = "all") |>
  colformat_char(na_str = "—")


# Figura 3 ---------------------------------------------------------------
fig3 <- datos_jp |>

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
#   fig3,
#   filename = "figuras/Figura3.png",
#   width = 15,
#   units = "cm",
#   dpi = 300
# )

# Figura 4 ---------------------------------------------------------------
fig4 <- gg_jpoint(mods = mod_jp, facets = TRUE) +
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
#   fig4,
#   filename = "figuras/Figura4.png",
#   width = 15,
#   height = 19,
#   units = "cm",
#   dpi = 300
# )

# Evolución temporal tasas GC por jurisdicción ---------------------------
## Tasas estandarizadas por año (Centro y NEA) -----
datos_centro <- recod_defun |>
  # Seleccionar muertes por GC
  filter(str_detect(grupo_gbd1, "GC") & region_deis == "Centro") |>
  droplevels() |>

  # Agrupar datos por año
  count(anio, grupo_edad, jurisdiccion, grupo_gbd2i, wt = n) |>

  # Unir con proyecciones poblacionales
  left_join(
    proy_2010_2023 |>
      # Agrupar por año
      count(anio, jurisdiccion, grupo_edad, wt = proy_pob, name = "pob")
  ) |>

  # Añadir población estándar 2022
  left_join(pob_est_2022) |>

  # Calcular tasa estandarizada
  group_by(anio, jurisdiccion, grupo_causa = grupo_gbd2i) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  )


### Regresión joinpoint -----
mod_centro <- model_jp(
  data = datos_centro,
  value = "value",
  time = "anio",
  group = c("jurisdiccion", "grupo_causa"),
  step = TRUE,
  k = 3,
  test = TRUE
)

# Figura 5 ---------------------------------------------------------------
## Paso 1 -----
fig5.1 <- recod_defun |>
  # Modificar grupo causa
  mutate(
    grupo_causa = fct_collapse(grupo_causa, "GC" = c("GC1-GC2", "GC3-GC4")),

    causa = fct_collapse(
      grupo_gbd2i,
      "GC3-GC4" = c("GC3", "GC4")
    )
  ) |>

  # Calcular frecuencias
  count(grupo_causa, causa, wt = n) |>
  mutate(pct = n / sum(n)) |>

  # Crear etiquetas texto
  mutate(
    label = case_when(
      between(pct, 0.007, 0.04) ~ percent(
        pct,
        accuracy = .1,
        decimal.mark = ","
      ),
      pct > 0.025 ~ paste0(
        causa,
        "\n",
        percent(pct, accuracy = .1, decimal.mark = ",")
      ),
      .default = ""
    )
  ) |>

  # Crear gráfico
  ggplot(aes(area = pct, subgroup = grupo_causa, fill = causa, label = label)) +
  geom_treemap() +
  scale_fill_manual(values = pal2, name = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(family = "Times New Roman"),
    legend.key.spacing.x = unit(10, "points"),
    legend.key.size = unit(10, "points")
  )

## Paso 2 -----
fig5.2 <- recod_defun |>
  rename(causa = grupo_gbd2m) |>
  # Modificar grupo causa
  mutate(
    grupo_causa = case_when(
      causa %in% c("DM", "ECV", "ERC", "NPL") ~ "ENT objetivo",
      causa %in% c("TRA", "SU", "HO") ~ "CE objetivo",
      causa %in% c("CMNN", "OCE", "ONT") ~ "Otras causas",
      .default = grupo_causa
    ),
  ) |>

  # Calcular frecuencias
  count(grupo_causa, causa, wt = n) |>
  mutate(pct = n / sum(n)) |>

  # Crear etiquetas texto
  mutate(
    label = case_when(
      between(pct, 0.007, 0.04) ~ percent(
        pct,
        accuracy = .1,
        decimal.mark = ","
      ),
      pct > 0.025 ~ paste0(
        causa,
        "\n",
        percent(pct, accuracy = .1, decimal.mark = ",")
      ),
      .default = ""
    )
  ) |>

  # Crear gráfico
  ggplot(aes(area = pct, subgroup = grupo_causa, fill = causa, label = label)) +
  geom_treemap() +
  scale_fill_manual(values = pal2, name = NULL) +
  theme(legend.position = "none")


## Pasos 3-4 -----
fig5.3 <- recod_defun |>
  rename(causa = grupo_gbd2f) |>
  # Modificar grupo causa
  mutate(
    grupo_causa = case_when(
      causa %in% c("DM", "ECV", "ERC", "NPL") ~ "ENT objetivo",
      causa %in% c("TRA", "SU", "HO") ~ "CE objetivo",
      causa %in% c("CMNN", "OCE", "ONT") ~ "Otras causas",
      .default = grupo_causa
    ),
  ) |>

  # Calcular frecuencias
  count(grupo_causa, causa, wt = n) |>
  mutate(pct = n / sum(n)) |>

  # Crear etiquetas texto
  mutate(
    label = case_when(
      between(pct, 0.007, 0.025) ~ percent(
        pct,
        accuracy = .1,
        decimal.mark = ","
      ),
      pct > 0.025 ~ paste0(
        causa,
        "\n",
        percent(pct, accuracy = .1, decimal.mark = ",")
      ),
      .default = ""
    )
  ) |>

  # Crear gráfico
  ggplot(aes(area = pct, subgroup = grupo_causa, fill = causa, label = label)) +
  geom_treemap() +
  scale_fill_manual(values = pal2, name = NULL) +
  theme(legend.position = "none")

## Crear gráfico
fig5 <- (fig5.1 + theme(legend.position = "none")) /
  fig5.2 /
  fig5.3 +
  cowplot::get_legend(fig5.1) &

  # Geometrías
  geom_treemap_subgroup_border() &
  geom_treemap_text(
    place = "center",
    reflow = TRUE,
    min.size = 3,
    color = "white",
    family = "Times New Roman"
  )

### Guardar figura -----
# ggsave(
#   fig5,
#   filename = "figuras/Figura5.png",
#   width = 15,
#   units = "cm",
#   dpi = 300
# )
