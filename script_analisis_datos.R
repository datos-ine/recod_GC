### Mortalidad por códigos basura en Argentina (2010–2023):
### redistribución hacia causas específicas
### Análisis de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar
# Última modificación: 10-09-2026 11:43

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Gráficos
  # cols4all,
  patchwork,
  ggridges,
  treemapify,
  DiagrammeR,
  DiagrammeRsvg,
  rsvg,
  # Tablas
  officer,
  flextable,
  # Tasas estandarizadas
  PHEindicatormethods,
  # Regresión joinpoint
  # joinpointR,
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


# Figura 1 ---------------------------------------------------------------
fig1 <- grViz(
  '
  digraph G {
    rankdir=LR
   graph[
    fontsize = 14
    fontname="Times-Roman"
    style = filled
    nodesep=.5
    ranksep=.15
    compound=true
   ]
  
   node[
    shape = plain
    style = filled
    fillcolor="grey95"
    fontsize = 14
    fontname="Times-Roman"
    width=3.25
   ]
  
   subgraph cluster_paso1{
    label = <<b>Paso 1<br/> Categorizar grupos de causas</b>>
    fillcolor="#FFCE66BF" 
  
    ent1[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>ENT objetivo</b></td>
    </tr>
    <tr>
    <td> Neoplasias (NPL) </td>
    </tr>    
    <tr>
    <td> Cardiovasculares (ECV) </td>
    </tr>
    <tr>
    <td> Respiratorias crónicas (CRD) </td>
    </tr>    
    <tr>
    <td> Diabetes y renales crónicas (DM-CKD) </td>
    </tr>
    </table>
    >]
  
    ce1[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>CE objetivo</b></td>
    </tr>
    <tr>
    <td> Accidentes de tránsito (TRA) </td>
    </tr>
    <tr>
    <td> Suicidio (SH) </td>
    </tr>    
    <tr>
    <td> Violencia interpersonal (VI) </td>
    </tr> 
    <tr>
    <td> Accidentes por caídas (CA) </td>
    </tr> 
    </table>
    >]
  
    otras1[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Causas no objetivo</b></td>
    </tr>
    <tr>
    <td> CMNN (INF, MAT-NEO, NUTR) </td>
    </tr>
    <tr>
    <td> Otras CE (OTR-CE) </td>
    </tr>
    <tr>
    <td> Otras ENT (OTR-ENT) </td>
    </tr>
    </table>
    >]
  
    gc1[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Códigos garbage</b></td>
    </tr>
    <tr>
    <td> GC1 </td>
    </tr>
    <tr>
    <td> GC2 </td>
    </tr>
    <tr>
    <td> GC3</td>
    </tr>
    <tr>
    <td> GC4</td>
    </tr>
    <tr>
    <td port="nne"> NNE</td>
    </tr>
    </table>
    >]
   }
  
   subgraph cluster_paso2{
    label = <<b>Paso 2<br/>Recategorizar GC3-GC4</b>>
    fillcolor="#92463ABF"
    // labelloc=b
    
  
  ent2[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>ENT objetivo</b></td>
    </tr>
    <tr>
    <td> NPL </td>
    </tr>
    <tr>
    <td> ECV </td>
    </tr>
    <tr>
    <td> CRD </td>
    </tr>
    <tr>
    <td> DM-CKD </td>
    </tr>
    </table>
    >]
  
    ce2[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>CE objetivo</b></td>
    </tr>
    <tr>
    <td> TRA </td>
    </tr>
    <tr>
    <td> SH </td>
    </tr>
    <tr>
    <td> VI </td>
    </tr>
    <tr>
    <td> CA </td>
    </tr>
       </table>
    >]
  
    otras2[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Otras causas</b></td>
    </tr>
    <tr>
    <td port = "cmnn"> CMNN (INF, MAT-NEO, NUTR) </td>
    </tr>
    <tr>
    <td> OTR-CE </td>
    </tr>
    <tr>
    <td> OTR-ENT</td>
    </tr>
    </table>
    >]
  
  gc2[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Códigos garbage</b></td>
    </tr>
    <tr>
    <td> GC1 </td>
    </tr>
    <tr>
    <td port="gc2"> GC2 </td>
    </tr>
    </table>
    >]
  
   }
  
    subgraph cluster_paso3{
    fillcolor="#4D5492BF"
  
    t3[
      label = <<b>Paso 3:<br/> Redistribuir GC2*</b>>
        style = plaintext
        ]

    ent3[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>ENT objetivo</b></td>
    </tr>
    <tr>
    <td> NPL </td>
    </tr>
    <tr>
    <td> ECV + GC2-ECV</td>
    </tr>
    <tr>
    <td> CRD </td>
    </tr>
    <tr>
    <td> DM-CKD </td>
    </tr>
    </table>
    >]
  
    ce3[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>CE objetivo + GC2-CE*</b></td>
    </tr>
    <tr>
    <td> TRA </td>
    </tr>
    <tr>
    <td> SH </td>
    </tr>
    <tr>
    <td> VI </td>
    </tr>
    <tr>
    <td> CA </td>
    </tr>
    </table>
    >]
  
    otras3[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Otras causas</b></td>
    </tr>
    <tr>
    <td> CMNN (INF, MAT-NEO, NUTR) </td>
    </tr>
    <tr>
    <td> OTR-CE + GC2-CE*</td>
    </tr>
    <tr>
    <td> OTR-ENT</td>
    </tr>
    </table>
    >]
  
  gc3[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Códigos garbage</b></td>
    </tr>
    <tr>
    <td port="gc1"> GC1 </td>
    </tr>
    </table>
    >]
    }
  
  
    subgraph cluster_paso4{    
    fillcolor= "#80E6FFBF"

    t4[
    label = <<b>Paso 4:<br/> Redistribuir GC1*</b>>
      style = plaintext
      ]
    
  ent4[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>ENT objetivo</b></td>
    </tr>
    <tr>
    <td> NPL </td>
    </tr>
    <tr>
    <td> ECV</td>
    </tr>
    <tr>
    <td> CRD + GC1-CRD</td>
    </tr>
    <tr>
    <td> DM-CKD </td>
    </tr>
    </table>
    >]
  
    ce4[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>CE objetivo + GC1-CE*</b></td>
    </tr>
    <tr>
    <td> TRA </td>
    </tr>
    <tr>
    <td> SH </td>
    </tr>
    <tr>
    <td> VI </td>
    </tr>
    <tr>
    <td> CA </td>
    </tr>
    </table>
    >]
  
    otras4[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Otras causas</b></td>
    </tr>
    <tr>
    <td port = "cmnn"> CMNN (INF, MAT-NEO, NUTR) </td>
    </tr>
    <tr>
    <td> OTR-CE + GC1-CE*</td>
    </tr>
    <tr>
    <td> OTR-ENT</td>
    </tr>
    </table>
    >]
  
   }
  
    t1[
    label=<* Redistribución proporcional por sexo y edad <br/>
      según frecuencias calculadas luego de <br/>
      recategorizar los GC3 y GC4.>
    shape=plain
    fillcolor="none"
    width=10
          ]
  
  
  gc1:nne -> ent2 [
           headlabel="50%*"
           labelangle=-50 
           labeldistance=2.5
           ]
  gc1:nne -> otras2:cmnn[label="50%*" constraint=false]
  
  gc1 -> ent3 [style="invis"] 
  
  gc3:gc -> {ent4:ent ce4:ce otras4:otras} [style="invis"] 
  
  gc3 -> t1 [style="invis" constraint=false] 

  gc2:gc2 -> t3 [constraint=false]
  gc3 -> t4 [constraint=false]
  
  }
 '
)


# Figura 2 ---------------------------------------------------------------
fig2 <- datos_gc |>
  # Crear grupo de nivel 1
  mutate(grupo_nivel1 = str_remove(gbd_paso1, ":.*")) |>

  # Crear grupo de causas
  mutate(
    grupo_causa = case_when(
      str_detect(gbd_paso1, "CRD|DM|ECV|NPL") ~ "ENT-OBJ",
      str_detect(gbd_paso1, "CA|SH|TRA$|VI") ~ "CE-OBJ",
      str_detect(gbd_paso1, "ENT") ~ "ENT-OTR",
      str_detect(gbd_paso1, "CE") ~ "CE-OTR",
      str_detect(gbd_paso1, "GC") ~ str_remove(gbd_paso1, ".*:"),
      .default = str_remove(gbd_paso1, ":.*")
    )
  ) |>

  # Calcular frecuencias x edad y sexo
  count(grupo_edad, sexo, grupo_nivel1, grupo_causa, wt = n) |>
  mutate(pct = n / sum(n), .by = c(grupo_causa, sexo, grupo_nivel1)) |>

  # Gráfico
  ggplot(aes(x = grupo_edad, y = pct, fill = grupo_causa)) +
  facet_grid(sexo ~ grupo_nivel1) +
  geom_col(position = "dodge") +

  # Escalas
  scale_cbpal_fill(palette = "managua", name = NULL) +
  scale_y_continuous(name = NULL, labels = percent) +
  scale_x_discrete(name = NULL) +

  # Layout
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90),
    text = element_text(family = "Times New Roman", size = 12)
  )


# Figura 3 ---------------------------------------------------------------
## Función auxiliar -----
treeplot_data <- function(data, var) {
  data |>
    separate(
      {{ var }},
      into = c("n1", "n2"),
      sep = ":",
      fill = "left"
    ) |>
    count(n1, n2, wt = n) |>
    mutate(pct = n / sum(n)) |>

    # Convertir a ggplot
    ggplot(aes(
      area = pct,
      subgroup = n1,
      fill = n1
    )) +
    theme(
      legend.position = "none",
      text = element_text(family = "Times New Roman")
    )
}


## Panel 1 -----
g1 <- datos_gc |>
  treeplot_data(var = gbd_paso1) +
  labs(subtitle = "Paso 1")


## Panel 2 -----
g2 <- datos_gc |>
  treeplot_data(var = gbd_paso2a) +
  labs(subtitle = "Paso 2A")


## Panel 3 -----
g3 <- datos_gc |>
  treeplot_data(var = gbd_paso2b) +
  labs(subtitle = "Paso 2B")


## Panel 4 -----
g4 <- datos_gc |>
  treeplot_data(var = gbd_paso3) +
  labs(subtitle = "Paso 3")

## Panel 5 ----
g5 <- datos_gc |>
  treeplot_data(var = gbd_paso4) +
  labs(subtitle = "Paso 4")


## Treemap -----
fig3 <- g1 /
  (g2 + g3) /
  (g4 + g5) &
  # Treemap
  geom_treemap(alpha = .9) &
  geom_treemap_text(
    aes(
      label = if_else(
        pct < 0.015,
        "",
        paste0(
          n2,
          " (",
          percent(pct, accuracy = .1, decimal.mark = ","),
          ")"
        )
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
    size = 10,
    family = "Times New Roman",
    fontface = "bold",
    place = "bottomleft"
  ) &

  # Layout
  scale_cbpal_fill(palette = "managua")


# Tabla S2 ---------------------------------------------------------------
tabs2 <- datos_gc |>
  # Filtrar GC1-GC2
  filter(gbd_paso1 %in% c("GC:GC1", "GC:GC2")) |>

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
  filter_out(n < 10000)


# Tabla S3 ---------------------------------------------------------------
tabs3 <- datos_gc |>
  # Frecuencias iniciales
  count(causa = gbd_paso1, wt = n, name = "n1") |>

  # Añadir frecuencias paso 2
  left_join(
    datos_gc |>
      count(causa = gbd_paso2b, wt = n, name = "n2")
  ) |>

  # Añadir frecuencias paso 4
  left_join(
    datos_gc |>
      # Frecuencias por grupo
      count(causa = gbd_paso4, wt = n, name = "n4")
  ) |>

  # Cambio absoluto
  mutate(razon = number(n4 / n1, accuracy = .1, decimal.mark = ",")) |>

  # Separar en grupo de causa y causa
  separate(causa, into = c("grupo_causa", "causa"), sep = ":")


# Tasas estandarizadas mortalidad x GC -----------------------------------
## Total país -----
tasa_gc_arg <- datos_gc |>
  # Seleccionar muertes por GC
  filter(str_detect(gbd_paso1, "GC")) |>
  droplevels() |>

  # Modificar niveles paso 1
  mutate(nivel = str_remove(gbd_paso1, ".*:")) |>

  # Agrupar datos por año
  count(
    anio,
    grupo_edad,
    nivel,
    wt = n
  ) |>

  # Unir con proyecciones poblacionales
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

  # Añadir población estándar 2022
  left_join(pob_est_2022) |>

  # Calcular tasa estandarizada
  group_by(anio, nivel) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  ) |>

  # Crear etiqueta región
  mutate(region_deis = "Argentina")


## Región DEIS -----
tasa_gc_reg <- datos_gc |>
  # Seleccionar muertes por GC
  filter(str_detect(gbd_paso1, "GC")) |>
  droplevels() |>

  # Modificar niveles paso 1
  mutate(nivel = str_remove(gbd_paso1, ".*:")) |>

  # Agrupar datos por año
  count(
    anio,
    grupo_edad,
    region_deis,
    nivel,
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


## Jurisdicción DEIS -----
tasa_gc_jur <- datos_gc |>
  # Seleccionar muertes por GC
  filter(str_detect(gbd_paso1, "GC")) |>
  droplevels() |>

  # Modificar niveles paso 1
  mutate(nivel = str_remove(gbd_paso1, ".*:")) |>

  # Agrupar datos por año
  count(
    anio,
    grupo_edad,
    region_deis,
    jurisd_deis,
    nivel,
    wt = n
  ) |>

  # Unir con proyecciones poblacionales
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

  # Añadir población estándar 2022
  left_join(pob_est_2022) |>

  # Calcular tasa estandarizada
  group_by(anio, region_deis, jurisd_deis, nivel) |>
  calculate_dsr(
    x = n,
    n = pob,
    stdpop = pob_est_2022,
    type = "standard"
  ) |>

  # Crear etiqueta para las tablas
  mutate(
    label = fct_cross(region_deis, jurisd_deis)
  )


# Evolución tasas GC -----------------------------------------------------
## Regresión joinpoint: Argentina -----
mod_ar <- model_jp_grid(
  data = tasa_gc_arg,
  rate = value,
  time = anio,
  group = "nivel"
)


## Regresión joinpoint: Región DEIS -----
mod_reg <- model_jp_grid(
  data = tasa_gc_reg,
  rate = value,
  time = anio,
  group = c("nivel", "region_deis")
)


## Regresión joinpoint: Jurisdicción DEIS -----
mod_jur <- model_jp_grid(
  data = tasa_gc_jur,
  rate = value,
  time = anio,
  group = c("nivel", "label")
)

## Coeficientes modelos -----
tab_mod <- get_summary(mods = c(mod_ar, mod_reg, mod_jur)) |>
  # Crear columnas nivel, región y jurisdicción
  separate(
    col = model,
    into = c("nivel", "region", "jurisdiccion"),
    fill = "right",
    extra = "merge"
  ) |>

  # Reemplazar NAs
  mutate(region = replace_na(region, "Argentina (Total)")) |>

  # Redondear cifras
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 2))) |>

  # Unir CI
  mutate(
    ci = paste0(apc_lower, "-", apc_upper, " ", apc_sig),
    .after = apc
  ) |>

  # Unir AAPC y significancia
  unite(col = "aapc", c(aapc, aapc_sig), sep = " ") |>

  # Ordenar filas
  arrange(nivel, region) |>

  # Seleccionar columnas
  select(-segment, -contains("_"))


# Tabla 1 ----------------------------------------------------------------
tab1 <- tab_mod |>
  # Filtrar GC1-GC2
  filter(nivel %in% c("GC1", "GC2")) |>

  # Generar tabla
  flextable() |>

  # Encabezados
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

  # Formato tabla
  tab_fmt() |>
  merge_v(j = 2:4, combine = TRUE) |>
  merge_v(j = "region") |>
  merge_v(j = "aapc") |>
  autofit() |>
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


# Tabla 2 ----------------------------------------------------------------
tab2 <- tab_mod |>
  # Filtrar GC3-GC4
  filter(nivel %in% c("GC3", "GC4")) |>

  # Generar tabla
  flextable() |>

  # Encabezados
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

  # Formato tabla
  tab_fmt() |>
  merge_v(j = 2:4, combine = TRUE) |>

  merge_v(j = "region") |>
  merge_v(j = "aapc") |>
  autofit() |>
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

# # Guardar tablas y figuras -----------------------------------------------
# ## Figura 1 -----
# # PNG
# export_svg(fig1) |>
#   charToRaw() |>
#   # rsvg_svg(
#   # file = "figuras/Figura1.svg",
#   rsvg_png(
#     file = "figs_tablas/Figura1.png",
#     width = 560
#   )

# # SVG
# export_svg(fig1) |>
#   charToRaw() |>
#   rsvg_svg(
#   file = "figs_tablas/Figura1.svg",
#     width = 560
#   )

# ## Figura 2 -----
# # PNG
# ggsave(
#   fig2,
#   filename = "figs_tablas/Figura2.png",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

# # SVG
# ggsave(
#   fig2,
#   filename = "figs_tablas/Figura2.svg",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

# ## Figura 3 -----
# # PNG
# ggsave(
#   fig3,
#   filename = "figs_tablas/Figura3.png",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

# # SVG
# ggsave(
#   fig3,
#   filename = "figs_tablas/Figura3.svg",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

# ## Tabla 1 -----
# save_as_docx(
#   tab1,
#   path = "figs_tablas/Tabla1.docx",
#   pr_section = prop_section(
#     page_margins = page_mar(
#       bottom = 0.7874,
#       top = 0.7874,
#       left = 0.7874,
#       right = 0.7874
#     )
#   )
# )

# ## Tabla 2 -----
# save_as_docx(
#   tab2,
#   path = "figs_tablas/Tabla2.docx",
#   pr_section = prop_section(
#     page_margins = page_mar(
#       bottom = 0.7874,
#       top = 0.7874,
#       left = 0.7874,
#       right = 0.7874
#     )
#   )
# )
