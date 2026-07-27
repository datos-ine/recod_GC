### Mortalidad por códigos basura en Argentina (2010–2023):
### redistribución hacia causas específicas
### Análisis de datos
### Autora: Tamara Ricardo
### Revisor: Juan I. Irassar

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Gráficos
  cols4all,
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
  ) |>

  # Modificar niveles paso1
  mutate(
    paso1 = if_else(
      str_detect(paso1, "GC3|GC4") & paso1 != "GC4-NNE",
      str_remove(paso1, "-.*"),
      paso1
    )
  ) |>

  # Crear variable nivel
  mutate(
    nivel = case_when(
      str_detect(paso1, "GC1|GC2") ~ "GC1-GC2",
      str_detect(paso1, "GC3|GC4") ~ "GC3-GC4",
      .default = NA
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
    <td> Diabetes mellitus (DM) </td>
    </tr>
    <tr>
    <td> Enf. cardiovasculares (ECV) </td>
    </tr>
    <tr>
    <td> Enf. respiratorias crónicas (ERC) </td>
    </tr>
    <tr>
    <td> Neoplasias (NPL) </td>
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
    <td> Suicidio (SU) </td>
    </tr>
    <tr>
    <td> Homicidio (HO) </td>
    </tr>
    </table>
    >]
  
    otras1[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Otras causas</b></td>
    </tr>
    <tr>
    <td> CMNN </td>
    </tr>
    <tr>
    <td> Otras CE </td>
    </tr>
    <tr>
    <td> Otras ENT</td>
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
    <td> DM </td>
    </tr>
    <tr>
    <td> ECV </td>
    </tr>
    <tr>
    <td> ERC </td>
    </tr>
    <tr>
    <td> NPL </td>
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
    <td> SU </td>
    </tr>
    <tr>
    <td> HO </td>
    </tr>
    </table>
    >]
  
    otras2[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Otras causas</b></td>
    </tr>
    <tr>
    <td port = "cmnn"> CMNN </td>
    </tr>
    <tr>
    <td> Otras CE </td>
    </tr>
    <tr>
    <td> Otras ENT</td>
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
    <td> DM </td>
    </tr>
    <tr>
    <td> ECV + GC2-ECV</td>
    </tr>
    <tr>
    <td> ERC </td>
    </tr>
    <tr>
    <td> NPL </td>
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
    <td> SU </td>
    </tr>
    <tr>
    <td> HO </td>
    </tr>
    </table>
    >]
  
    otras3[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Otras causas</b></td>
    </tr>
    <tr>
    <td> CMNN </td>
    </tr>
    <tr>
    <td> Otras CE + GC2-CE*</td>
    </tr>
    <tr>
    <td> Otras ENT</td>
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
    <td> DM </td>
    </tr>
    <tr>
    <td> ECV</td>
    </tr>
    <tr>
    <td> ERC + GC1-ERC</td>
    </tr>
    <tr>
    <td> NPL </td>
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
    <td> SU </td>
    </tr>
    <tr>
    <td> HO </td>
    </tr>
    </table>
    >]
  
    otras4[label = <
    <table border="0" cellborder = "1" cellspacing  ="0">
    <tr>
    <td width="250"><b>Otras causas</b></td>
    </tr>
    <tr>
    <td port = "cmnn"> CMNN </td>
    </tr>
    <tr>
    <td> Otras CE + GC1-CE*</td>
    </tr>
    <tr>
    <td> Otras ENT</td>
    </tr>
    </table>
    >]
  
   }
  
    t1[
    label=<* Redistribución proporcional por sexo y edad <br/>>
    shape=plain
    fillcolor="none"
    width=10
          ]
  
  
  gc1:nne -> ent2 [
           headlabel="50%*"
           labelangle=-50 
           labeldistance=2.5
           ]
  gc1:nne -> otras2:cmnn[label="50%" constraint=false]
  
  gc1 -> ent3 [style="invis"] 
  
  gc3:gc -> {ent4:ent ce4:ce otras4:otras} [style="invis"] 
  
  gc3 -> t1 [style="invis" constraint=false] 

  gc2:gc2 -> t3 [constraint=false]
  gc3 -> t4 [constraint=false]
  
  }
 '
)

## Guardar figura -----
# export_svg(fig1) |>
#   charToRaw() |>
#     # rsvg_svg(
#     # file = "figuras/Figura1.svg",
#   rsvg_png(
#     file = "figuras/Figura1.png",
#     width = 560
#   )

# Tabla 1 ----------------------------------------------------------------
tab1 <- tibble(
  Variable = c(
    "Año",
    "Región",
    # "Subgregión*",
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
    "Regionalización sanitaria utilizada por la DEIS para agrupar las provincias argentinas",
    # "Agrupación territorial definida por la DEIS para resguardar la confidencialidad de los datos",
    "Nivel de agregación jurisdiccional definido por la DEIS para resguardar la confidencialidad de los datos",
    "Sexo consignado en el acta de defunción",
    "Categorías de edad agrupadas según criterios de la DEIS",
    "Causa básica de muerte codificada a cuatro dígitos según la CIE-10",
    "Clasificación de causas según grandes grupos del GBD-2019",
    "Clasificación de causas según grupos nivel 2 del GBD-2019",
    "Cantidad anual de defunciones según región, jurisdicción, sexo, grupo etario y causa básica de muerte"
  ),
  Valores = c(
    "2010-2023",
    paste(levels(datos_gc$region_deis), collapse = "; "),
    # paste(levels(datos_gc$subregion_deis), collapse = "; "),
    paste(levels(datos_gc$jurisd_deis), collapse = "; "),
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
  width(width = c(3, 7, 7), unit = "cm") |>
  add_footer_row(
    values = as_paragraph(
      as_b("Centro: "),
      "CABA, Buenos Aires, Córdoba, Entre Ríos, Santa Fe; ",
      as_b("Cuyo2: "),
      "La Rioja, San Juan, San Luis; ",
      as_b("NEA: "),
      "Chaco, Corrientes, Formosa, Misiones; ",
      as_b("NOA1: "),
      "Jujuy, Salta; ",
      as_b("NOA2: "),
      "Catamarca, Santiago del Estero; ",
      as_b("Patagonia Norte: "),
      "La Pampa, Neuquén, Río Negro; ",
      as_b("Patagonia Sur: "),
      "Chubut, Santa Cruz, Tierra del Fuego"
    ),
    colwidths = 3,
    top = FALSE
  ) |>
  tab_fmt() |>
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


# # Guardar tabla -----
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
        paste0(
          grupo_causa,
          "\n(",
          percent(sum(pct), accuracy = .1, decimal.mark = ","),
          ")"
        ),
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
        pct < .01 | fill_id == "CMNN" ~ "",
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


## Guardar figura -----
# ggsave(
#   fig2,
#   filename = "figuras/Figura2.png",
#   width = 17,
#   height = 20,
#   units = "cm",
#   dpi = 300
# )

# Tasas estandarizadas mortalidad x GC -----------------------------------
## Total país -----
tasa_gc_arg <- datos_gc |>
  # Seleccionar muertes por GC
  filter(grupo_causa == "GC") |>
  droplevels() |>

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
  filter(grupo_causa == "GC") |>
  droplevels() |>

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
  filter(grupo_causa == "GC") |>
  droplevels() |>

  # Modificar niveles paso 1
  mutate(paso1 = str_remove(paso1, "-.*")) |>

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
  )


# Evolución tasas GC -----------------------------------------------------
## Unir datos tasas -----
datos_jp <- tasa_gc_arg |>
  # Añadir tasas x región
  bind_rows(tasa_gc_reg) |>

  # Añadir tasas x jurisdicción
  bind_rows(tasa_gc_jur) |>

  # Crear etiqueta
  mutate(
    grupo = if_else(
      is.na(jurisd_deis),
      region_deis,
      paste0(region_deis, ": ", jurisd_deis)
    )
  )


## Regresión joinpoint -----
mod_jp_reg <- model_jp(
  datos_jp,
  value = value,
  time = anio,
  group = c("nivel", "grupo"),
  step = TRUE,
  k = 2,
  min_dist = 2,
  test = TRUE
)

get_aapc(mod_jp_reg, digits = 2) |> print(n = Inf)


# Tabla 2 ----------------------------------------------------------------
tab2 <- mod_jp_reg |>
  summary_jp(dec = ",") |>
  separate_wider_delim(
    cols = subgroup,
    names = c("subgroup", "jurisd"),
    delim = ": ",
    too_few = "align_start"
  ) |>

  jp_to_ft(lan = "es") |>
  set_header_labels(
    Subgrupo = "Región",
    jurisd = "Jurisdicción"
  ) |>
  tab_fmt() |>
  merge_v(j = 2:4, combine = TRUE) |>
  merge_v(j = 2) |>
  autofit() |>
  set_caption(
    autonum = FALSE,
    fp_p = fp_par(line_spacing = 1.5),
    caption = as_paragraph(
      as_chunk(
        "Tabla 2. Coeficientes de la regresión joinpoint de las tasas estandarizadas de mortalidad por códigos garbage (GC) por región y jurisdicción, Argentina (2010-2023).",
        props = fp_text(
          font.size = 12,
          font.family = "Times New Roman",
          bold = TRUE
        )
      )
    )
  )
